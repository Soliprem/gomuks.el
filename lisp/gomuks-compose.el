;;; gomuks-compose.el --- Draft buffers and composition commands -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Own draft buffers and staged files. Delivery lifecycle lives in gomuks-send.

;;; Code:

(require 'gomuks-send)
(require 'gomuks-members)
(declare-function gomuks-compose-mode "gomuks-ui")
(declare-function gomuks-attachment-preview-mode "gomuks-ui")
(declare-function gomuks-home "gomuks-view")
(declare-function gomuks--event-at-point "gomuks-render")
(declare-function evil-insert-state "evil-commands")

(defun gomuks--pending-attachments ()
  "Return this draft's attachments, migrating an older single entry."
  (when (stringp (car-safe gomuks--compose-attachment))
    (setq gomuks--compose-attachment (list gomuks--compose-attachment)))
  gomuks--compose-attachment)

(defun gomuks-compose-send ()
  "Snapshot this draft and retain it until confirmed delivery."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode) (user-error "Not in a Gomuks composer"))
  (when gomuks--compose-sending (user-error "This draft is already being sent"))
  (when gomuks--compose-job
    (if (or (gomuks--send-event gomuks--compose-job)
            (eq (gomuks--send-phase gomuks--compose-job) 'uncertain))
        (user-error "Use C-c C-y to recover the previous send before submitting again")
      (remhash (gomuks--send-id gomuks--compose-job) gomuks--sends)
      (setq gomuks--compose-job nil)))
  (let ((text (string-trim-right (buffer-substring-no-properties (point-min) (point-max))))
        (attachments (gomuks--pending-attachments))
        (relation (or gomuks--compose-relation
                      (when gomuks--thread-root
                        `((rel_type . "m.thread") (event_id . ,gomuks--thread-root))))))
    (when (and (not attachments) (string-empty-p (string-trim text)))
      (user-error "Message is empty"))
    (gomuks--start-send (current-buffer) gomuks--room-id text relation attachments)))

(defun gomuks-send-message (&optional text)
  "Compose a message, or submit TEXT with retained delivery recovery."
  (interactive)
  (unless (derived-mode-p 'gomuks-room-mode) (user-error "Open a gomuks room first"))
  (if (null text) (gomuks-compose)
    (when (string-empty-p (string-trim text)) (user-error "Message is empty"))
    (gomuks--start-send nil gomuks--room-id text
                        (when gomuks--thread-root
                          `((rel_type . "m.thread") (event_id . ,gomuks--thread-root))) nil)))

(defun gomuks-insert-emoji ()
  "Search for an emoji and insert it into the current draft."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (if (fboundp 'emoji-search)
      (call-interactively #'emoji-search)
    (insert (read-char-by-name "Emoji: "))))

(defun gomuks--show-room (buffer)
  "Display room BUFFER with its composer below it."
  (delete-other-windows)
  (switch-to-buffer buffer)
  (let ((composer-window (split-window (selected-window) -6 'below)))
    (set-frame-parameter nil 'gomuks-composer-window composer-window)
    (set-window-buffer composer-window (gomuks--composer-buffer buffer)))
  (select-window (get-buffer-window buffer)))

(defun gomuks--mention-capf ()
  "Complete an @name in the composer with a room member."
  (when (and gomuks--room-id
             (save-excursion
               (skip-chars-backward "^ \t\n@")
               (eq (char-before) ?@)))
    (gomuks--request-mention-members gomuks--room-id)
    (let ((start (save-excursion
                   (skip-chars-backward "^ \t\n@")
                   (1- (point))))
          (state (gethash gomuks--room-id gomuks--member-state))
          candidates)
      (when state
        (maphash
         (lambda (user-id rowid)
           (when-let* ((event (gethash rowid gomuks--events)))
             (when (member (gomuks--alist 'membership
                                          (gomuks--event-content event))
                           '("join" "invite"))
               (push (cons (format "@%s (%s)"
                                   (gomuks--sender-name gomuks--room-id
                                                        `((sender . ,user-id)))
                                   user-id)
                           user-id)
                     candidates))))
         state))
      (when candidates
        (setq candidates (nreverse candidates))
        (list start (point) candidates
              :exit-function
              (lambda (candidate status)
                (when (eq status 'finished)
                  (let ((user-id (cdr (assoc candidate candidates)))
                        (name nil))
                    (when user-id
                      (setq name (gomuks--sender-name
                                  gomuks--room-id `((sender . ,user-id))))
                      (delete-region start (point))
                      (insert (format "[%s](https://matrix.to/#/%s) "
                                      (replace-regexp-in-string
                                       "[][\\`*_()]"
                                       (lambda (match) (concat "\\" match)) name)
                                      (url-hexify-string user-id))))))))))))

(defun gomuks--delete-temp-attachments (attachments)
  "Delete temporary files in ATTACHMENTS."
  (dolist (attachment attachments)
    (when (cadr attachment)
      (ignore-errors (delete-file (car attachment))))))

(defun gomuks--update-composer-header ()
  "Show the current draft target and pending attachment in the header."
  (let ((attachments (gomuks--pending-attachments)))
    (setq header-line-format
          (concat " " (or gomuks--compose-context
                          (and gomuks--room-id
                               (gomuks--room-name gomuks--room-id))
                          "Compose a message")
                  "  •  C-c C-c send  •  C-c C-a attach"
                  (when attachments
                    (format "  •  %s [C-c C-o preview, C-c C-d remove]"
                            (if (cdr attachments)
                                (format "%d attachments" (length attachments))
                              (nth 2 (car attachments)))))
                  (when gomuks--compose-job
                    (format "  •  %s" (gomuks--send-phase gomuks--compose-job)))
                  "  •  C-c C-y retry  •  C-c C-k return"))))

(defun gomuks--cleanup-compose-attachment ()
  "Release staged files except those still owned by a retained send record."
  (let* ((owned (and gomuks--compose-job
                     (gethash (gomuks--send-id gomuks--compose-job) gomuks--sends)
                     (gomuks--send-remaining gomuks--compose-job)))
         (staged (gomuks--pending-attachments)))
    (setq gomuks--compose-attachment nil)
    (gomuks--delete-temp-attachments (cl-set-difference staged owned :test #'eq))))

(defun gomuks--prepare-attachment-change ()
  "Return unaccepted failed snapshots to this draft before changing its files."
  (when gomuks--compose-job
    (if (or (gomuks--send-event gomuks--compose-job)
            (eq (gomuks--send-phase gomuks--compose-job) 'uncertain))
        (user-error "Recover or discard the accepted send before changing its attachments")
      (remhash (gomuks--send-id gomuks--compose-job) gomuks--sends)
      (setq gomuks--compose-job nil))))

(defun gomuks--stage-attachment (file temporary &optional label)
  "Queue FILE in this draft until send; delete it later if TEMPORARY.
LABEL is shown in the composer header."
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (when gomuks--compose-sending
    (user-error "This draft is already being sent"))
  (gomuks--prepare-attachment-change)
  (unless (file-regular-p file)
    (user-error "Attachment file does not exist"))
  (gomuks--pending-attachments)
  (add-hook 'kill-buffer-hook #'gomuks--cleanup-compose-attachment nil t)
  (setq gomuks--compose-attachment
        (append gomuks--compose-attachment
                (list (list file temporary
                            (or label (file-name-nondirectory file))))))
  (gomuks--update-composer-header)
  (message "gomuks: attached %s; send with C-c C-c"
           (nth 2 (car (last gomuks--compose-attachment))))
  t)

(defun gomuks--attachment-choices ()
  "Return numbered completion choices for this draft's attachments."
  (cl-loop for attachment in (gomuks--pending-attachments)
           for index from 1
           for file = (car attachment)
           for size = (when-let* ((attributes (file-attributes file)))
                        (file-attribute-size attributes))
           collect (cons (format "%d: %s (%s)" index (nth 2 attachment)
                                 (if size (file-size-human-readable size)
                                   "missing file"))
                         attachment)))

(defun gomuks--choose-attachment (prompt)
  "Choose a pending attachment with PROMPT."
  (let ((choices (gomuks--attachment-choices)))
    (unless choices (user-error "No attachment in this draft"))
    (if (cdr choices)
        (cdr (assoc (completing-read prompt choices nil t) choices))
      (cdar choices))))

(defun gomuks-compose-remove-attachment (&optional attachment)
  "Remove ATTACHMENT from this draft, prompting when needed."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (when gomuks--compose-sending
    (user-error "This draft is already being sent"))
  (gomuks--prepare-attachment-change)
  (setq attachment (or attachment
                       (gomuks--choose-attachment "Remove attachment: ")))
  (unless (memq attachment (gomuks--pending-attachments))
    (user-error "That attachment is no longer in this draft"))
  (setq gomuks--compose-attachment
        (delq attachment gomuks--compose-attachment))
  (gomuks--delete-temp-attachments (list attachment))
  (gomuks--update-composer-header)
  (message "gomuks: attachment removed"))

(defun gomuks-compose-preview-attachment ()
  "Preview a staged attachment and offer to remove it."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (let* ((source (current-buffer))
         (attachment (gomuks--choose-attachment "Preview attachment: "))
         (file (car attachment))
         (buffer (get-buffer-create "*Gomuks attachment preview*")))
    (unless (file-readable-p file)
      (user-error "Attachment file is missing"))
    (with-current-buffer buffer
      (gomuks-attachment-preview-mode)
      (setq-local gomuks--preview-source source
                  gomuks--preview-attachment attachment
                  header-line-format
                  (format " %s  •  RET open externally  •  d remove  •  q close"
                          (nth 2 attachment)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (nth 2 attachment)))
        (if-let* ((image (and (display-images-p)
                              (ignore-errors (create-image file)))))
            (insert-image image "[image preview]")
          (insert (format "Inline preview unavailable here. Press RET to open %s\n"
                          file)))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun gomuks-attachment-preview-open ()
  "Open the previewed attachment with the system viewer."
  (interactive)
  (unless gomuks--preview-attachment
    (user-error "No staged attachment in this preview"))
  (browse-url-of-file (car gomuks--preview-attachment)))

(defun gomuks-attachment-preview-remove ()
  "Remove the attachment shown in this preview."
  (interactive)
  (unless (and (buffer-live-p gomuks--preview-source)
               gomuks--preview-attachment)
    (user-error "No staged attachment in this preview"))
  (let ((source gomuks--preview-source)
        (attachment gomuks--preview-attachment))
    (with-current-buffer source
      (gomuks-compose-remove-attachment attachment)))
  (quit-window t))

(defun gomuks--composer-buffer (room-buffer &optional relation context initial-text)
  "Return the draft buffer for ROOM-BUFFER, preserving any existing text.
RELATION is the Matrix relation for the draft, CONTEXT labels its target,
and INITIAL-TEXT seeds a newly created draft."
  (with-current-buffer room-buffer
    (let* ((id gomuks--room-id)
           (thread gomuks--thread-root)
           (name (format "*Gomuks compose: %s%s%s*"
                         id (if thread (concat " thread " thread) "")
                         (if context (concat " " context) "")))
           (existing (get-buffer name))
           (buffer (get-buffer-create name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'gomuks-compose-mode)
          (gomuks-compose-mode))
        (setq gomuks--room-id id
              gomuks--thread-root thread
              gomuks--compose-relation relation
              gomuks--compose-context context
              gomuks--compose-source room-buffer)
        (unless existing
          (when initial-text (insert initial-text))
          (set-buffer-modified-p nil))
        (gomuks--update-composer-header))
      buffer)))

(defun gomuks--show-composer (buffer)
  "Display and focus composer BUFFER below the room view."
  (let ((window (frame-parameter nil 'gomuks-composer-window)))
    (unless (and (window-live-p window)
                 (eq (window-frame window) (selected-frame)))
      (setq window (split-window (selected-window) -6 'below))
      (set-frame-parameter nil 'gomuks-composer-window window))
    (set-window-buffer window buffer)
    (select-window window)
    (goto-char (point-max))
    (gomuks--request-mention-members gomuks--room-id)
    (when (and (bound-and-true-p evil-mode)
               (fboundp 'evil-insert-state))
      (evil-insert-state))))

(defun gomuks-compose ()
  "Focus the writable draft buffer below the current room."
  (interactive)
  (unless (derived-mode-p 'gomuks-room-mode)
    (user-error "Open a gomuks room first"))
  (gomuks--show-composer (gomuks--composer-buffer (current-buffer))))

(defun gomuks-compose-leave ()
  "Return to the room, keeping the current draft."
  (interactive)
  (let ((window (get-buffer-window gomuks--compose-source t)))
    (if (window-live-p window)
        (select-window window)
      (gomuks-home))))

(defun gomuks--send-related (text relation &optional base-content)
  "Submit TEXT with RELATION and optional BASE-CONTENT, retaining recovery."
  (let ((send (gomuks--make-send :id (cl-incf gomuks--send-serial)
                                 :room gomuks--room-id :backend gomuks-backend-url
                                 :account gomuks--user-id :text text :caption text
                                 :relation relation :content base-content
                                 :token (gomuks--cache-token gomuks--room-id))))
    (puthash (gomuks--send-id send) send gomuks--sends)
    (gomuks--send-submit send)))

(defun gomuks-reply (&optional text)
  "Compose a reply to the message at point, or send TEXT directly."
  (interactive)
  (let ((event-id (gomuks--alist 'event_id (gomuks--event-at-point))))
    (unless event-id (user-error "This message has no event ID yet"))
    (let ((relation (append (when gomuks--thread-root
                              `((rel_type . "m.thread")
                                (event_id . ,gomuks--thread-root)))
                            `((m.in_reply_to . ((event_id . ,event-id)))))))
      (if text
          (gomuks--send-related text relation)
        (gomuks--show-composer
         (gomuks--composer-buffer (current-buffer) relation
                                  (concat "reply " event-id)))))))

(defun gomuks-edit (&optional text)
  "Compose an edit for the message at point, or send TEXT directly."
  (interactive)
  (let* ((event (gomuks--event-at-point))
         (event-id (gomuks--alist 'event_id event)))
    (when (and gomuks--user-id (gomuks--alist 'sender event)
               (not (equal gomuks--user-id (gomuks--alist 'sender event))))
      (user-error "You can only edit your own messages"))
    (unless event-id (user-error "This message has no event ID yet"))
    (let ((relation `((rel_type . "m.replace") (event_id . ,event-id))))
      (if text
          (gomuks--send-related text relation)
        (gomuks--show-composer
         (gomuks--composer-buffer
          (current-buffer) relation (concat "edit " event-id)
          (gomuks--edit-source event)))))))

(defun gomuks-send-file (file &optional sticker delete-after-upload)
  "Stage FILE in a composer, or submit it with recoverable delivery ownership.
STICKER sends native sticker content. DELETE-AFTER-UPLOAD means the temporary
file is owned until confirmed delivery, rather than merely until upload."
  (interactive "fFile to send: ")
  (unless gomuks--room-id (user-error "Open a room first"))
  (if (and (not sticker) (derived-mode-p 'gomuks-compose-mode))
      (gomuks--stage-attachment file delete-after-upload)
    (gomuks--start-send
     nil gomuks--room-id ""
     (or gomuks--compose-relation
         (when gomuks--thread-root
           `((rel_type . "m.thread") (event_id . ,gomuks--thread-root))))
     (list (list file delete-after-upload (file-name-nondirectory file))) sticker)))

(defun gomuks-send-sticker (file)
  "Upload an image FILE and send it as a Matrix sticker."
  (interactive "fSticker image: ")
  (unless (file-regular-p file)
    (user-error "Sticker file does not exist"))
  (unless (member (downcase (or (file-name-extension file) ""))
                  '("png" "webp" "gif" "jpg" "jpeg"))
    (user-error "Choose a PNG, WebP, GIF, or JPEG image"))
  (gomuks-send-file file t))

(defun gomuks-paste-image ()
  "Attach an image from the graphical clipboard to the current draft."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (unless (display-graphic-p)
    (user-error "Image paste needs a graphical Emacs frame"))
  (let ((image
         (cl-loop for (type . suffix) in '((image/png . ".png")
                                           (image/jpeg . ".jpg")
                                           (image/webp . ".webp")
                                           (image/gif . ".gif"))
                  for data = (condition-case nil
                                 (gui-get-selection 'CLIPBOARD type)
                               (error nil))
                  when (and (stringp data) (> (length data) 0))
                  return (cons suffix data))))
    (unless image
      (user-error "No image found on the clipboard"))
    (let ((file (make-temp-file "gomuks-paste-" nil (car image))))
      (unwind-protect
          (let ((coding-system-for-write 'no-conversion))
            (write-region (encode-coding-string (cdr image) 'binary)
                          nil file nil 'silent)
            (when (gomuks--stage-attachment file t "Clipboard image")
              (setq file nil)))
        (when file (delete-file file))))))

(defun gomuks-send-gif (source)
  "Stage or send a GIF/WebP from a local file or bounded HTTPS download."
  (interactive (list (read-string "GIF file or direct URL: ")))
  (unless gomuks--room-id (user-error "Open a room first"))
  (if (string-match-p "\\`https://" source)
      (let ((buffer (current-buffer)) (id gomuks--room-id)
            (token (gomuks--cache-token gomuks--room-id)))
        (gomuks--curl-file
         source nil
         (lambda (failure path)
           (if failure (message "gomuks: GIF download failed: %s" failure)
             (let* ((prefix (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally path nil 0 12)
                              (buffer-string)))
                    (suffix (cond ((string-prefix-p "GIF8" prefix) ".gif")
                                  ((and (string-prefix-p "RIFF" prefix)
                                        (>= (length prefix) 12)
                                        (equal (substring prefix 8 12) "WEBP")) ".webp"))))
               (if (and suffix (buffer-live-p buffer)
                        (gomuks--cache-current-p id token))
                   (let ((file (concat path suffix)))
                     (rename-file path file)
                     (condition-case err
                         (with-current-buffer buffer (gomuks-send-file file nil t))
                       (error (delete-file file) (message "gomuks: %s" (error-message-string err)))))
                 (delete-file path)
                 (message "gomuks: GIF target expired or URL was not a GIF/WebP")))))
         (* 32 1024 1024)))
    (unless (and (file-regular-p source)
                 (member (downcase (or (file-name-extension source) "")) '("gif" "webp")))
      (user-error "Choose a GIF or WebP file, or paste an HTTPS URL"))
    (gomuks-send-file source)))

(provide 'gomuks-compose)
;;; gomuks-compose.el ends here
