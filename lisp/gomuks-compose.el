;;; gomuks-compose.el --- Composer for gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-
;;
;; Copyright (C) 2026 Francesco Prem Solidoro
;;
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Maintainer: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Created: settembre 30, 2026
;; Modified: settembre 30, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/Soliprem/gomuks.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gomuks-media)
(declare-function gomuks-compose-mode "gomuks-ui")
(declare-function gomuks-attachment-preview-mode "gomuks-ui")
(declare-function gomuks-home "gomuks-view")
(declare-function gomuks--event-at-point "gomuks-view")
(declare-function evil-insert-state "evil-commands")

(defun gomuks--pending-attachments ()
  "Return this draft's attachments, migrating an older single entry."
  (when (stringp (car-safe gomuks--compose-attachment))
    (setq gomuks--compose-attachment (list gomuks--compose-attachment)))
  gomuks--compose-attachment)

(defun gomuks-compose-send ()
  "Send this draft, uploading its pending attachment when present."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Not in a Gomuks composer"))
  (when gomuks--compose-sending
    (user-error "This draft is already being sent"))
  (let* ((buffer (current-buffer))
         (text (string-trim-right (buffer-substring-no-properties
                                   (point-min) (point-max))))
         (attachments (gomuks--pending-attachments))
         (id gomuks--room-id)
         (relation (or gomuks--compose-relation
                       (when gomuks--thread-root
                         `((rel_type . "m.thread")
                           (event_id . ,gomuks--thread-root))))))
    (when (and (not attachments) (string-empty-p (string-trim text)))
      (user-error "Message is empty"))
    (setq gomuks--compose-sending t)
    (if attachments
        (gomuks--send-next-attachment buffer text id relation attachments)
      (gomuks--post
       "send_message"
       (append `((room_id . ,id) (text . ,text))
               (when-let* ((mentions (gomuks--draft-mentions text)))
                 `((mentions . ,mentions)))
               (when relation `((relates_to . ,relation))))
       (lambda (failure _response)
         (gomuks--finish-compose-send buffer text failure))))))

(defun gomuks-send-message (&optional text)
  "Compose a message, or send TEXT directly when supplied."
  (interactive)
  (unless (derived-mode-p 'gomuks-room-mode)
    (user-error "Open a gomuks room first"))
  (if (null text)
      (gomuks-compose)
    (when (string-empty-p (string-trim text))
      (user-error "Message is empty"))
    (let ((id gomuks--room-id)
          (relation (when gomuks--thread-root
                      `((rel_type . "m.thread")
                        (event_id . ,gomuks--thread-root)))))
      (gomuks--post
       "send_message"
       (append `((room_id . ,id) (text . ,text))
               (when relation `((relates_to . ,relation))))
       (lambda (failure _response)
         (message "gomuks: %s" (or failure "message queued")))))))

(defun gomuks-insert-emoji ()
  "Search for an emoji and insert it into the current draft."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (if (fboundp 'emoji-search)
      (call-interactively #'emoji-search)
    (insert (read-char-by-name "Emoji: " t))))

(defun gomuks--show-room (buffer)
  "Display room BUFFER with its composer below it."
  (delete-other-windows)
  (switch-to-buffer buffer)
  (let ((composer-window (split-window (selected-window) -6 'below)))
    (set-frame-parameter nil 'gomuks-composer-window composer-window)
    (set-window-buffer composer-window (gomuks--composer-buffer buffer)))
  (select-window (get-buffer-window buffer)))

(defun gomuks--request-mention-members (id)
  "Load the member list for room ID once for composer completion."
  (when (and id (not (gethash id gomuks--mention-members-loaded)))
    (puthash id 'loading gomuks--mention-members-loaded)
    (gomuks--post
     "get_room_state"
     (append `((room_id . ,id) (include_members . t))
             (unless (gomuks--alist 'has_member_list
                                    (gethash id gomuks--rooms))
               '((fetch_members . t))))
     (lambda (failure events)
       (if failure
           (remhash id gomuks--mention-members-loaded)
         (let ((state (or (gethash id gomuks--member-state)
                          (make-hash-table :test 'equal))))
           (dolist (event events)
             (when (and (equal (gomuks--alist 'type event) "m.room.member")
                        (equal (gomuks--alist 'room_id event) id))
               (puthash (gomuks--alist 'rowid event) event gomuks--events)
               (puthash (gomuks--alist 'state_key event)
                        (gomuks--alist 'rowid event) state)))
           (puthash id state gomuks--member-state)
           (puthash id t gomuks--mention-members-loaded)))))))

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
        (list start (point) (nreverse candidates)
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

(defun gomuks--draft-mentions (text)
  "Return Matrix mention metadata for completed mentions in TEXT."
  (let ((start 0) ids)
    (while (string-match "https://matrix\\.to/#/\\(%40[^)[:space:]]+\\)" text start)
      (let ((end (match-end 0))
            (id (url-unhex-string (match-string 1 text))))
        (when (and (string-match-p "\\`@[^:]+:.+\\'" id)
                   (not (member id ids)))
          (push id ids))
        (setq start end)))
    (when ids
      `((user_ids . ,(vconcat (nreverse ids)))))))

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
                  "  •  C-c C-k return"))))

(defun gomuks--cleanup-compose-attachment ()
  "Forget pending attachments and remove their temporary files."
  (when (and (gomuks--pending-attachments) (not gomuks--compose-sending))
    (let ((attachments gomuks--compose-attachment))
      (setq gomuks--compose-attachment nil)
      (gomuks--delete-temp-attachments attachments))))

(defun gomuks--stage-attachment (file temporary &optional label)
  "Queue FILE in this draft until send; delete it later if TEMPORARY.
LABEL is shown in the composer header."
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (when gomuks--compose-sending
    (user-error "This draft is already being sent"))
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

(defun gomuks--finish-compose-send (buffer text failure &optional remaining)
  "Finish sending BUFFER's TEXT, reporting FAILURE.
Clean temporary files in REMAINING if BUFFER was closed during sending."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq gomuks--compose-sending nil)
        (when (and (not failure)
                   (equal text (string-trim-right (buffer-string))))
          (erase-buffer)
          (set-buffer-modified-p nil)))
    (gomuks--delete-temp-attachments remaining))
  (message "gomuks: %s" (or failure "message queued")))

(defun gomuks--send-next-attachment (buffer text id relation remaining)
  "Send the first of REMAINING from BUFFER, with TEXT on the last.
Use ID and RELATION for every attachment event."
  (let ((attachment (car remaining))
        (caption (if (cdr remaining) "" text)))
    (condition-case upload-error
        (progn
          (message "gomuks: uploading %s" (nth 2 attachment))
          (gomuks--upload-file
           (car attachment) id
           (lambda (failure content)
             (if failure
                 (gomuks--finish-compose-send buffer text failure remaining)
               (condition-case send-error
                   (gomuks--post
                    "send_message"
                    (append `((room_id . ,id) (text . ,caption)
                              (base_content . ,content))
                            (when (not (string-empty-p caption))
                              (when-let* ((mentions (gomuks--draft-mentions caption)))
                                `((mentions . ,mentions))))
                            (when relation `((relates_to . ,relation))))
                    (lambda (send-failure _response)
                      (if send-failure
                          (gomuks--finish-compose-send
                           buffer text send-failure remaining)
                        (gomuks--delete-temp-attachments (list attachment))
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (setq gomuks--compose-attachment
                                  (cdr gomuks--compose-attachment))
                            (gomuks--update-composer-header)))
                        (cond
                         ((not (buffer-live-p buffer))
                          (gomuks--finish-compose-send
                           buffer text "draft closed" (cdr remaining)))
                         ((cdr remaining)
                          (gomuks--send-next-attachment
                           buffer text id relation (cdr remaining)))
                         (t (gomuks--finish-compose-send buffer text nil))))))
                 (error
                  (gomuks--finish-compose-send
                   buffer text (error-message-string send-error) remaining)))))))
      (error
       (gomuks--finish-compose-send
        buffer text (error-message-string upload-error) remaining)))))

(defun gomuks--send-related (text relation &optional base-content)
  "Send TEXT with Matrix RELATION and optional BASE-CONTENT."
  (gomuks--post
   "send_message"
   (append `((room_id . ,gomuks--room-id) (text . ,text)
             (relates_to . ,relation))
           (when base-content `((base_content . ,base-content))))
   (lambda (failure _response)
     (message "gomuks: %s" (or failure "message queued")))))

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
    (unless event-id (user-error "This message has no event ID yet"))
    (let ((relation `((rel_type . "m.replace") (event_id . ,event-id))))
      (if text
          (gomuks--send-related text relation)
        (gomuks--show-composer
         (gomuks--composer-buffer
          (current-buffer) relation (concat "edit " event-id)
          (gomuks--alist 'body (gomuks--event-content event))))))))

(defun gomuks-send-file (file &optional sticker delete-after-upload)
  "Attach FILE to a draft, or send it from a room buffer.
When STICKER is non-nil, send an `m.sticker' event immediately.
When DELETE-AFTER-UPLOAD is non-nil, remove FILE when it is no longer needed."
  (interactive "fFile to send: ")
  (unless gomuks--room-id (user-error "Open a room first"))
  (if (and (not sticker) (derived-mode-p 'gomuks-compose-mode))
      (gomuks--stage-attachment file delete-after-upload)
    (let* ((id gomuks--room-id)
           (relation (or gomuks--compose-relation
                         (when gomuks--thread-root
                           `((rel_type . "m.thread")
                             (event_id . ,gomuks--thread-root))))))
      (message "gomuks: uploading %s" (file-name-nondirectory file))
      (gomuks--upload-file
       file id
       (lambda (failure content)
         (when delete-after-upload
           (ignore-errors (delete-file file)))
         (if failure
             (message "gomuks: upload failed: %s" failure)
           (when sticker
             (puthash "msgtype" "m.sticker" content))
           (gomuks--post
            "send_message"
            (append `((room_id . ,id) (text . "")
                      (base_content . ,content))
                    (when relation `((relates_to . ,relation))))
            (lambda (send-failure _response)
              (message "gomuks: %s"
                       (or send-failure "attachment queued"))))))))))

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
  "Send a GIF or animated WebP from local file or HTTPS SOURCE."
  (interactive (list (read-string "GIF file or direct URL: ")))
  (unless gomuks--room-id (user-error "Open a room first"))
  (if (string-match-p "\\`https://" source)
      (let ((source-buffer (current-buffer)))
        (url-retrieve
         source
         (lambda (status)
           (let ((response-buffer (current-buffer)))
             (unwind-protect
                 (let* ((code (and (boundp 'url-http-response-status)
                                   url-http-response-status))
                        (body-start (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "\r?\n\r?\n" nil t)))
                        (prefix (and body-start
                                     (buffer-substring-no-properties
                                      body-start (min (point-max) (+ body-start 12)))))
                        (suffix (cond
                                 ((and prefix (string-prefix-p "GIF8" prefix)) ".gif")
                                 ((and prefix (string-prefix-p "RIFF" prefix)
                                       (>= (length prefix) 12)
                                       (string= (substring prefix 8 12) "WEBP"))
                                  ".webp"))))
                   (cond
                    ((or (plist-get status :error) (not (equal code 200)))
                     (message "gomuks: GIF download failed: %s"
                              (or (plist-get status :error) code)))
                    ((not suffix)
                     (message "gomuks: URL did not return a GIF or WebP image"))
                    (t
                     (let ((file (make-temp-file "gomuks-gif-" nil suffix))
                           (coding-system-for-write 'binary))
                       (condition-case err
                           (progn
                             (write-region body-start (point-max) file nil 'silent)
                             (if (buffer-live-p source-buffer)
                                 (with-current-buffer source-buffer
                                   (gomuks-send-file file nil t))
                               (delete-file file)))
                         (error
                          (delete-file file)
                          (message "gomuks: GIF download failed: %s" err)))))))
               (when (buffer-live-p response-buffer)
                 (kill-buffer response-buffer)))))
         nil t))
    (unless (and (file-regular-p source)
                 (member (downcase (or (file-name-extension source) ""))
                         '("gif" "webp")))
      (user-error "Choose a GIF or WebP file, or paste an HTTPS URL"))
    (gomuks-send-file source)))

(provide 'gomuks-compose)
;;; gomuks-compose.el ends here
