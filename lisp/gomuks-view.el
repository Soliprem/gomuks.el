;;; gomuks-view.el --- View for gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-
;;
;; Copyright (C) 2026 Francesco Prem Solidoro
;;
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Maintainer: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Created: settembre 30, 2026
;; Modified: settembre 30, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/Soliprem/gomuks-view
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gomuks-compose)
(declare-function gomuks-rooms-mode "gomuks-ui")
(declare-function gomuks-room-mode "gomuks-ui")
(declare-function gomuks-search-mode "gomuks-ui")
(declare-function gomuks-reactions-mode "gomuks-ui")
(declare-function notifications-notify "notifications")
(declare-function emoji--init "emoji")
(declare-function emoji--read-emoji "emoji")

(defun gomuks--view-identity ()
  "Return the identity of the current room, search, or reactions view."
  (list major-mode gomuks--room-id gomuks--thread-root gomuks--context-target
        gomuks--search-generation gomuks--reactions-key
        (gomuks--alist 'event_id gomuks--reactions-event)))

(defun gomuks--view-current-p (buffer identity)
  "Return non-nil when BUFFER is live and still represents IDENTITY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (equal identity (gomuks--view-identity)))))

(defun gomuks--reset-views (&optional room-id)
  "Clear retained data and request state in room, search, and reaction views.
Preserve view targets, queries, and parent buffers.  When ROOM-ID is
non-nil, update only views for that room.  Start no requests."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'gomuks-room-mode 'gomuks-search-mode
                                'gomuks-reactions-mode)
                 (or (null room-id)
                     (equal gomuks--room-id room-id)))
        (setq gomuks--initial-history-requested nil
              gomuks--thread-events nil gomuks--thread-next-batch nil
              gomuks--context-events nil
              gomuks--search-results nil gomuks--search-next-batch nil
              gomuks--search-loading nil gomuks--search-error nil
              gomuks--search-generation (1+ gomuks--search-generation)
              gomuks--reactions-event nil gomuks--reactions-key nil
              header-line-format nil)
        (let ((inhibit-read-only t)) (erase-buffer))))))


(defun gomuks--room-preview (event topic)
  "Describe the latest EVENT in a room, falling back to TOPIC."
  (let* ((content (gomuks--event-content event))
         (attachment (gomuks--attachment content event))
         (kind (plist-get attachment :kind))
         (body (gomuks--alist 'body content)))
    (cond
     (attachment
      (format "%s: %s"
              (pcase kind
                ("m.image" "Image") ("m.sticker" "Sticker")
                ("m.video" "Video") ("m.audio" "Audio")
                (_ "File"))
              (plist-get attachment :name)))
     ((and (stringp body) (not (string-empty-p (string-trim body)))) body)
     (t (or topic "")))))

(defun gomuks--render-rooms ()
  "Redraw the Gomuks home page from cached room state."
  (when-let* ((buffer (get-buffer "*Gomuks*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (entries nil)
            (selected (get-text-property (point) 'gomuks-room-id))
            (unread-total 0)
            (status (if (and (string= gomuks--connection-status "Disconnected")
                             (process-live-p gomuks--stream))
                        "Connected" gomuks--connection-status)))
        (maphash (lambda (id meta)
                   (unless (member id gomuks--hidden-room-ids)
                     (push (cons id meta) entries)))
                 gomuks--rooms)
        (setq entries (sort entries
                            (lambda (a b)
                              (> (or (gomuks--alist 'sorting_timestamp (cdr a)) 0)
                                 (or (gomuks--alist 'sorting_timestamp (cdr b)) 0)))))
        (dolist (entry entries)
          (cl-incf unread-total
                   (or (gomuks--alist 'unread_notifications (cdr entry)) 0)))
        (erase-buffer)
        (insert "\n  " (propertize "gomuks" 'face 'gomuks-title-face)
                "     " (propertize status
                                    'face (if (string= status "Connected")
                                              'success 'shadow))
                "\n  " (propertize "Matrix in Emacs" 'face 'shadow)
                "\n\n  " (propertize "HOME" 'face 'gomuks-heading-face)
                (format "     %d rooms  ·  %d unread\n" (length entries) unread-total)
                "  " (propertize "RET" 'face 'help-key-binding)
                " open   " (propertize "C-k" 'face 'help-key-binding)
                " switch   " (propertize "m" 'face 'help-key-binding)
                " mute   " (propertize (if (bound-and-true-p evil-mode)
                                           "g r" "g")
                                       'face 'help-key-binding)
                " reconnect   " (propertize "q" 'face 'help-key-binding)
                " leave\n\n  " (propertize "RECENT ROOMS" 'face 'gomuks-heading-face)
                "\n\n")
        (unless entries
          (insert (if (> (hash-table-count gomuks--rooms) 0)
                      "  No visible rooms.\n"
                    "  Waiting for rooms from the backend…\n")))
        (dolist (entry entries)
          (let* ((start (point))
                 (id (car entry))
                 (meta (cdr entry))
                 (unread (or (gomuks--alist 'unread_notifications meta) 0))
                 (preview-event (gethash (gomuks--alist 'preview_event_rowid meta)
                                         gomuks--events))
                 (preview (gomuks--room-preview preview-event
                                                (gomuks--alist 'topic meta))))
            (insert "  "
                    (propertize (format "%-31s"
                                        (truncate-string-to-width
                                         (gomuks--room-name id) 30 nil nil "…"))
                                'face 'gomuks-room-face)
                    (if (gethash id gomuks--muted-rooms)
                        (propertize "[muted] " 'face 'shadow)
                      "")
                    (propertize (truncate-string-to-width
                                 (replace-regexp-in-string "[\n\r]+" " " preview)
                                 (max 12 (- (window-width) 50)) nil nil "…")
                                'face 'shadow)
                    (if (> unread 0)
                        (propertize (format "  %d" unread) 'face 'gomuks-unread-face)
                      "")
                    "\n")
            (put-text-property start (point) 'gomuks-room-id id)))
        (goto-char (or (and selected
                            (text-property-any (point-min) (point-max)
                                               'gomuks-room-id selected))
                       (next-single-property-change (point-min) 'gomuks-room-id)
                       (point-min)))))))

(defun gomuks--visible-body (event body)
  "Return BODY without Matrix's quoted reply fallback for EVENT."
  (if (and (stringp body) (gomuks--reply-target event)
           (string-prefix-p "> " body)
           (string-match "\n\n" body))
      (substring body (match-end 0))
    body))

(defun gomuks--formatted-body (content event)
  "Render CONTENT's Matrix HTML, or return nil if it cannot be rendered.
Remove the quoted reply fallback for EVENT before rendering."
  (when (and (equal (gomuks--alist 'format content) "org.matrix.custom.html")
             (stringp (gomuks--alist 'formatted_body content))
             (fboundp 'libxml-parse-html-region))
    (condition-case nil
        (with-temp-buffer
          (insert (gomuks--alist 'formatted_body content))
          (let* ((dom (libxml-parse-html-region))
                 (body (or (car (dom-by-tag dom 'body)) dom))
                 (shr-width 10000)
                 (shr-use-fonts nil)
                 (shr-inhibit-images t))
            (when (gomuks--reply-target event)
              (setcdr (cdr body)
                      (cl-remove-if (lambda (node)
                                      (and (listp node) (eq (car node) 'mx-reply)))
                                    (dom-children body))))
            (erase-buffer)
            (shr-insert-document dom)
            (string-trim-right (buffer-string))))
      (error nil))))

(defun gomuks--reply-summary (room-id event-id)
  "Return a short label for EVENT-ID in ROOM-ID."
  (if-let* ((event (gomuks--find-event room-id event-id)))
      (let ((body (gomuks--visible-body
                   event (gomuks--alist 'body (gomuks--event-content event)))))
        (format "%s: %s"
                (gomuks--sender-name room-id event)
                (truncate-string-to-width
                 (replace-regexp-in-string "[\n\r]+" " " (or body "[attachment]"))
                 72 nil nil "…")))
    "earlier message"))

(defun gomuks--thread-counts (room-id)
  "Return cached reply counts by thread root in ROOM-ID."
  (let ((counts (make-hash-table :test 'equal)))
    (maphash (lambda (_rowid event)
               (when (and (equal (gomuks--alist 'room_id event) room-id)
                          (equal (gomuks--alist 'relation_type event) "m.thread"))
                 (let ((root (gomuks--alist 'relates_to event)))
                   (when root
                     (puthash root (1+ (gethash root counts 0)) counts)))))
             gomuks--events)
    counts))


(defun gomuks--request-missing-members (id events)
  "Fetch room member ID events needed to name senders in EVENTS."
  (when (process-live-p gomuks--stream)
    (let ((keys nil)
          (generation (gomuks--cache-token id)))
      (dolist (event events)
        (when-let* ((sender (gomuks--alist 'sender event)))
          (let ((rowid (when-let* ((state (gethash id gomuks--member-state)))
                         (gethash sender state)))
                (key (cons id sender)))
            (when (and (not (and rowid (gethash rowid gomuks--events)))
                       (not (gethash key gomuks--requested-members)))
              (puthash key t gomuks--requested-members)
              (push `((room_id . ,id) (type . "m.room.member")
                      (state_key . ,sender)) keys)))))
      (when keys
        (gomuks--post
         "get_specific_room_state" `((keys . ,(vconcat (reverse keys))))
         (lambda (failure response)
           (when (gomuks--cache-current-p id generation)
             (if failure
                 (dolist (key keys)
                   (remhash (cons id (gomuks--alist 'state_key key))
                            gomuks--requested-members))
               (let ((state (or (gethash id gomuks--member-state)
                                (make-hash-table :test 'equal))))
                 (dolist (event response)
                   (when (and (equal (gomuks--alist 'type event) "m.room.member")
                              (equal (gomuks--alist 'room_id event) id)
                              (gomuks--alist 'rowid event))
                     (gomuks--store-events (list event))
                     (unless (gethash (gomuks--alist 'state_key event) state)
                       (puthash (gomuks--alist 'state_key event)
                                (gomuks--alist 'rowid event) state))))
                 (puthash id state gomuks--member-state)
                 (gomuks--render-room id))))))))))

(defun gomuks--echo-area-notifications-p ()
  "Return non-nil when incoming messages should appear in the echo area."
  (or (eq gomuks-echo-area-notifications t)
      (and (eq gomuks-echo-area-notifications 'in-gomuks)
           (with-current-buffer (window-buffer (selected-window))
             (derived-mode-p 'gomuks-rooms-mode
                             'gomuks-room-mode
                             'gomuks-compose-mode
                             'gomuks-search-mode
                             'gomuks-reactions-mode)))))

(defun gomuks--notify (id event)
  "Notify the user about EVENT in room ID."
  (unless (or (gethash id gomuks--muted-rooms)
              (member id gomuks--hidden-room-ids))
    (let ((title (gomuks--room-name id))
          (body (format "%s: %s"
                        (gomuks--sender-name id event)
                        (or (gomuks--alist 'body (gomuks--event-content event))
                            "[new message]"))))
      (when (gomuks--echo-area-notifications-p)
        (message "gomuks %s — %s" title body))
      (when (and gomuks-desktop-notifications
                 (require 'notifications nil t))
        (condition-case nil
            (notifications-notify :title title :body body :app-name "gomuks.el")
          (error nil))))))

(defun gomuks--event-at-point ()
  "Return the message event at point, or signal a user error."
  (or (get-text-property (point) 'gomuks-event)
      (get-text-property (max (point-min) (1- (point))) 'gomuks-event)
      (user-error "No message on this line")))

(defun gomuks-copy-sender-id ()
  "Copy the full Matrix ID of the message sender at point."
  (interactive)
  (let ((sender (gomuks--alist 'sender (gomuks--event-at-point))))
    (unless sender (user-error "This message has no sender"))
    (kill-new sender)
    (message "Copied sender ID: %s" sender)))

(defun gomuks-activate-at-point ()
  "Activate the link or action button at point."
  (interactive)
  (if-let* ((button (button-at (point))))
      (push-button button)
    (user-error "No link or button at point")))

(defun gomuks--buttonize-urls (start end)
  "Make URLs between START and END clickable without changing their text."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search t))
      (while (re-search-forward browse-url-button-regexp end t)
        (let ((url (match-string-no-properties 0)))
          (make-text-button
           (match-beginning 0) (match-end 0)
           'follow-link t
           'help-echo url
           'face 'link
           'url url
           'action (lambda (button)
                     (browse-url (button-get button 'url)))))))))

(defun gomuks--insert-attachment (event attachment)
  "Insert ATTACHMENT controls for EVENT at point."
  (let* ((name (plist-get attachment :name))
         (size (plist-get attachment :size))
         (kind (or (plist-get attachment :kind) "m.file"))
         (cached (gomuks--cached-media attachment)))
    (if (equal kind "m.sticker")
        (progn
          (insert "    ")
          (insert-text-button
           (format "[Sticker: %s]" name)
           'face 'link 'follow-link t
           'help-echo "Open sticker; press D to save"
           'display (when (and cached (display-images-p))
                      (condition-case nil
                          (create-image cached nil nil :max-width 160 :max-height 160)
                        (error nil)))
           'action (lambda (_button) (gomuks-open-attachment event)))
          (insert "\n"))
      (insert "    " (propertize (format "[%s]" (upcase (string-remove-prefix "m." kind)))
                                 'face 'gomuks-heading-face)
              " " (propertize name 'face 'gomuks-room-face))
      (when (numberp size)
        (insert "  " (propertize (file-size-human-readable size) 'face 'shadow)))
      (insert "  ")
      (insert-text-button "Open" 'face 'link 'follow-link t
                          'action (lambda (_button) (gomuks-open-attachment event)))
      (insert "  ")
      (insert-text-button "Save" 'face 'link 'follow-link t
                          'action (lambda (_button) (gomuks-save-attachment nil event)))
      (insert "\n")
      (when (and cached (display-images-p) (equal kind "m.image"))
        (condition-case nil
            (when-let* ((image (create-image cached nil nil
                                             :max-width 360 :max-height 220)))
              (insert "    ")
              (insert-image image "[image preview]")
              (insert "\n"))
          (error nil))))
    (gomuks--maybe-preview-image attachment gomuks--room-id)))

(defun gomuks--insert-event (event &optional previous thread-counts)
  "Insert EVENT, grouping it after PREVIOUS when appropriate.
THREAD-COUNTS maps thread root IDs to known reply counts."
  (let* ((edit (gethash (gomuks--alist 'last_edit_rowid event) gomuks--events))
         (content (if edit
                      (or (gomuks--alist 'm.new_content
                                         (gomuks--event-content edit))
                          (gomuks--event-content edit))
                    (gomuks--event-content event)))
         (attachment (unless (gomuks--alist 'redacted_by event)
                       (gomuks--attachment content event)))
         (body (if (gomuks--alist 'redacted_by event)
                   "[redacted]"
                 (or (gomuks--formatted-body content event)
                     (gomuks--visible-body event (gomuks--alist 'body content)))))
         (reply-to (gomuks--reply-target event))
         (reactions (cl-remove-if-not
                     (lambda (entry) (and (numberp (cdr entry)) (> (cdr entry) 0)))
                     (gomuks--alist 'reactions event)))
         (kind (or (gomuks--alist 'decrypted_type event)
                   (gomuks--alist 'type event))))
    (when (or body attachment (string= kind "m.room.encrypted"))
      (let* ((start (point))
             (sender (gomuks--alist 'sender event))
             (name (gomuks--sender-name gomuks--room-id event))
             (timestamp (gomuks--alist 'timestamp event))
             (previous-time (gomuks--alist 'timestamp previous))
             (grouped (and previous (equal sender (gomuks--alist 'sender previous))
                           (equal name (gomuks--sender-name gomuks--room-id previous))
                           (numberp timestamp) (numberp previous-time)
                           (<= 0 (- timestamp previous-time) (* 15 60 1000)))))
        (insert (propertize
                 (if timestamp
                     (format-time-string "%H:%M"
                                         (seconds-to-time (/ timestamp 1000.0)))
                   "     ")
                 'face 'shadow)
                "  ")
        (if grouped
            (insert (propertize "│" 'face 'shadow) "  ")
          (insert (propertize name 'face 'gomuks-sender-face
                              'help-echo (or sender "Unknown sender"))
                  "  "))
        (when reply-to
          (insert "\n        ")
          (insert-text-button
           (concat "↪ " (gomuks--reply-summary gomuks--room-id reply-to))
           'face 'link 'follow-link t
           'help-echo "Follow reply"
           'action (lambda (_button) (gomuks-follow-reply reply-to)))
          (insert "\n        "))
        (when (and body (not (equal (plist-get attachment :kind) "m.sticker"))
                   (or (not attachment)
                       (not (equal body (plist-get attachment :name)))))
          (let ((body-start (point)))
            (insert (replace-regexp-in-string "\n" "\n        " body))
            (gomuks--buttonize-urls body-start (point))
            (insert "\n")))
        (unless (or body attachment)
          (insert (if (gomuks--alist 'decryption_error event)
                      "[unable to decrypt]" "[encrypted]") "\n"))
        (when (and attachment
                   (or (equal (plist-get attachment :kind) "m.sticker")
                       (not body) (equal body (plist-get attachment :name))))
          (insert "\n"))
        (when attachment
          (gomuks--insert-attachment event attachment))
        (when reactions
          (insert "        ")
          (dolist (entry reactions)
            (let ((key (gomuks--key-string (car entry))))
              (insert-text-button
               (format "%s %s" key (cdr entry))
               'face 'link 'follow-link t
               'help-echo "Show who used this reaction"
               'action (lambda (_button) (gomuks-show-reactions event key)))
              (insert "  ")))
          (insert "\n"))
        (when thread-counts
          (let* ((event-id (gomuks--alist 'event_id event))
                 (count (and event-id (gethash event-id thread-counts)))
                 (thread-reply (equal (gomuks--alist 'relation_type event)
                                      "m.thread")))
            (when (or count thread-reply)
              (insert "        ")
              (insert-text-button
               (if count (format "↳ %d thread repl%s" count
                                 (if (= count 1) "y" "ies"))
                 "↳ in thread")
               'face 'link 'follow-link t
               'action (lambda (_button) (gomuks-open-thread event)))
              (insert "\n"))))
        (unless (bolp) (insert "\n"))
        (put-text-property start (point) 'gomuks-event event)
        (put-text-property start (point) 'gomuks-event-rowid
                           (gomuks--alist 'rowid event))
        t))))

(defun gomuks--room-header (id)
  "Return the fixed header for room ID in the current room buffer."
  (let* ((name (replace-regexp-in-string "[\n\r]+" " " (gomuks--room-name id)))
         (topic (gomuks--alist 'topic (gethash id gomuks--rooms)))
         (kind (cond (gomuks--context-target "REPLY CONTEXT")
                     (gomuks--thread-root "THREAD"))))
    (concat " " (propertize name 'face 'gomuks-room-face)
            (when (gethash id gomuks--muted-rooms)
              (propertize "  [muted]" 'face 'shadow))
            (when kind (concat "  ·  " (propertize kind 'face 'gomuks-heading-face)))
            (when (and (stringp topic) (not (string-empty-p topic)))
              (concat "  ·  "
                      (propertize
                       (truncate-string-to-width
                        (replace-regexp-in-string "[\n\r]+" " " topic)
                        (max 20 (- (window-width) (string-width name) 10))
                        nil nil "…")
                       'face 'shadow))))))

(defun gomuks--render-buffer (buffer id)
  "Redraw room, thread, or reply context BUFFER for room ID."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max)))
            (rowid (get-text-property (point) 'gomuks-event-rowid))
            (windows (mapcar (lambda (window)
                               (list window
                                     (or (= (window-point window) (point-max))
                                         (pos-visible-in-window-p
                                          (max (point-min) (1- (point-max)))
                                          window))
                                     (window-start window)
                                     (get-text-property (window-start window)
                                                        'gomuks-event-rowid)))
                             (get-buffer-window-list buffer nil t)))
            (shown-events nil)
            (thread-counts (unless (or gomuks--thread-root gomuks--context-target)
                             (gomuks--thread-counts id)))
            (previous nil))
        (setq-local header-line-format (gomuks--room-header id))
        (erase-buffer)
        (insert (propertize (if (bound-and-true-p evil-mode)
                                "i" "C-c C-s")
                            'face 'help-key-binding)
                " compose   " (propertize (if (bound-and-true-p evil-mode)
                                              "p" "M-p")
                                          'face 'help-key-binding)
                " older   " (propertize "C-k" 'face 'help-key-binding)
                " switch   " (propertize "b" 'face 'help-key-binding)
                " home   " (propertize "o" 'face 'help-key-binding)
                " open   " (propertize "D" 'face 'help-key-binding)
                " save\n\n")
        (cond
         (gomuks--context-target
          (dolist (event gomuks--context-events)
            (push event shown-events)
            (when (gomuks--insert-event event previous)
              (setq previous event))))
         (gomuks--thread-root
          (dolist (event gomuks--thread-events)
            (push event shown-events)
            (when (gomuks--insert-event event previous)
              (setq previous event))))
         (t
          (dolist (rowid (gethash id gomuks--timelines))
            (when-let* ((event (gethash rowid gomuks--events)))
              (push event shown-events)
              (when (gomuks--insert-event event previous thread-counts)
                (setq previous event))))))
        (cond
         (at-end (goto-char (point-max)))
         (rowid
          (when-let* ((position (text-property-any
                                 (point-min) (point-max)
                                 'gomuks-event-rowid rowid)))
            (goto-char position))))
        (dolist (state windows)
          (pcase-let ((`(,window ,at-bottom ,start ,start-rowid) state))
            (when (window-live-p window)
              (if at-bottom
                  (progn
                    (save-selected-window
                      (select-window window)
                      (goto-char (point-max))
                      (recenter -1))
                    (set-window-point window (point-max)))
                (set-window-start
                 window
                 (or (and start-rowid
                          (text-property-any (point-min) (point-max)
                                             'gomuks-event-rowid start-rowid))
                     (min start (point-max)))
                 t)))))
        (gomuks--request-missing-members id shown-events)))))

(defun gomuks--render-room (id)
  "Redraw all open room, thread, and context buffers for room ID."
  (when-let* ((buffer (get-buffer (format "*Gomuks: %s*" id))))
    (gomuks--render-buffer buffer id))
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (and (derived-mode-p 'gomuks-room-mode)
                 (or gomuks--thread-root gomuks--context-target)
                 (equal gomuks--room-id id)))
      (gomuks--render-buffer buffer id))))

(defun gomuks--selected-room-buffer ()
  "Return the main room buffer being viewed, including from its composer."
  (let ((buffer (window-buffer (selected-window))))
    (with-current-buffer buffer
      (cond
       ((and (derived-mode-p 'gomuks-room-mode)
             (not gomuks--thread-root) (not gomuks--context-target))
        buffer)
       ((and (derived-mode-p 'gomuks-compose-mode)
             (buffer-live-p gomuks--compose-source)
             (with-current-buffer gomuks--compose-source
               (and (derived-mode-p 'gomuks-room-mode)
                    (not gomuks--thread-root)
                    (not gomuks--context-target))))
        gomuks--compose-source)))))

(defun gomuks--maybe-mark-read ()
  "Mark the newest visible message in the active room as read."
  (when (or (not (display-graphic-p))
            (frame-focus-state (selected-frame)))
    (when-let* ((buffer (gomuks--selected-room-buffer))
                (window (get-buffer-window buffer (selected-frame)))
                (id (buffer-local-value 'gomuks--room-id buffer))
                (meta (gethash id gomuks--rooms)))
      (when (and (or (> (or (gomuks--alist 'unread_messages meta) 0) 0)
                     (> (or (gomuks--alist 'unread_notifications meta) 0) 0)
                     (gomuks--alist 'marked_unread meta))
                 (>= (window-end window t)
                     (with-current-buffer buffer (point-max))))
        (when-let* ((event (cl-loop for rowid in (reverse (gethash id gomuks--timelines))
                                    for item = (gethash rowid gomuks--events)
                                    when (gomuks--alist 'event_id item)
                                    return item))
                    (event-id (gomuks--alist 'event_id event))
                    (generation (gomuks--cache-token id)))
          (unless (equal event-id (gethash id gomuks--last-read))
            (puthash id event-id gomuks--last-read)
            (gomuks--post
             "mark_read"
             `((room_id . ,id) (event_id . ,event-id)
               (receipt_type . "m.read"))
             (lambda (failure _response)
               (when (and failure (gomuks--cache-current-p id generation))
                 (when (equal event-id (gethash id gomuks--last-read))
                   (remhash id gomuks--last-read))
                 (message "gomuks: read receipt failed: %s" failure))))))))))

(defun gomuks-home ()
  "Show the Gomuks home page in the full window."
  (interactive)
  (unless (and gomuks--previous-window-config
               (eq (window-configuration-frame gomuks--previous-window-config)
                   (selected-frame)))
    (setq gomuks--previous-window-config (current-window-configuration)))
  (delete-other-windows)
  (let ((buffer (get-buffer-create "*Gomuks*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-rooms-mode)
        (gomuks-rooms-mode)))
    (switch-to-buffer buffer)
    (gomuks--render-rooms)))

(defun gomuks-quit ()
  "Leave Gomuks and restore the previous window layout."
  (interactive)
  (if (and gomuks--previous-window-config
           (eq (window-configuration-frame gomuks--previous-window-config)
               (selected-frame)))
      (progn
        (set-window-configuration gomuks--previous-window-config)
        (setq gomuks--previous-window-config nil))
    (quit-window)))

(defun gomuks-back ()
  "Return to the room that opened this thread or reply context."
  (interactive)
  (cond
   ((not (buffer-live-p gomuks--parent-buffer)) (gomuks-home))
   ((with-current-buffer gomuks--parent-buffer
      (derived-mode-p 'gomuks-search-mode))
    (delete-other-windows)
    (switch-to-buffer gomuks--parent-buffer))
   (t (gomuks--show-room gomuks--parent-buffer))))

(defun gomuks-open-room ()
  "Open the room on the current home-page line."
  (interactive)
  (let ((id (get-text-property (point) 'gomuks-room-id)))
    (unless id (user-error "No room on this line"))
    (gomuks--open-room-id id)))

(defun gomuks-toggle-mute ()
  "Toggle Matrix push notifications for the room at point or in this buffer."
  (interactive)
  (let* ((id (or gomuks--room-id (get-text-property (point) 'gomuks-room-id)))
         (muted (and id (gethash id gomuks--muted-rooms)))
         (generation (gomuks--cache-token id)))
    (unless id (user-error "No room selected"))
    (gomuks--post
     "mute_room" `((room_id . ,id) (muted . ,(if muted :false t)))
     (lambda (failure _response)
       (when (gomuks--cache-current-p id generation)
         (if failure
             (message "gomuks: could not %s %s: %s"
                      (if muted "unmute" "mute") (gomuks--room-name id) failure)
           (if muted (remhash id gomuks--muted-rooms)
             (puthash id t gomuks--muted-rooms))
           (gomuks--render-rooms)
           (gomuks--render-room id)
           (message "gomuks: %s %s"
                    (if muted "unmuted" "muted") (gomuks--room-name id))))))))

(defun gomuks--switch-room (hidden)
  "Choose a cached room and open it, selecting HIDDEN rooms when non-nil."
  (let (rooms)
    (maphash (lambda (id meta)
               (when (if hidden (member id gomuks--hidden-room-ids)
                       (not (member id gomuks--hidden-room-ids)))
                 (push (cons id meta) rooms)))
             gomuks--rooms)
    (unless rooms (user-error (if hidden "No hidden rooms loaded yet"
                                "No rooms loaded yet")))
    (setq rooms (sort rooms
                      (lambda (a b)
                        (> (or (gomuks--alist 'sorting_timestamp (cdr a)) 0)
                           (or (gomuks--alist 'sorting_timestamp (cdr b)) 0)))))
    (let* ((candidates
            (mapcar (lambda (room)
                      (let* ((id (car room))
                             (name (replace-regexp-in-string
                                    "[\n\r]+" " " (gomuks--room-name id)))
                             (unread (or (gomuks--alist 'unread_notifications
                                                        (cdr room)) 0)))
                        (cons (format "%s%s  %s" name
                                      (if (> unread 0) (format " (%d)" unread) "")
                                      id)
                              id)))
                    rooms))
           (completion-extra-properties '(:display-sort-function identity))
           (choice (completing-read (if hidden "Hidden room: " "Switch to room: ")
                                    candidates nil t)))
      (when-let* ((id (cdr (assoc choice candidates))))
        (gomuks--open-room-id id)))))

(defun gomuks-switch-room ()
  "Choose a visible room from the cached room list and open it."
  (interactive)
  (gomuks--switch-room nil))

(defun gomuks-switch-hidden-room ()
  "Choose a hidden room and open it."
  (interactive)
  (gomuks--switch-room t))

(defun gomuks-toggle-hidden-room ()
  "Hide or unhide the room at point or in the current room buffer."
  (interactive)
  (let ((id (or gomuks--room-id (get-text-property (point) 'gomuks-room-id))))
    (unless id (user-error "No room selected"))
    (let ((hidden (member id gomuks--hidden-room-ids)))
      (setq gomuks--hidden-room-ids
            (if hidden (delete id gomuks--hidden-room-ids)
              (cons id gomuks--hidden-room-ids)))
      (gomuks--save-hidden-rooms)
      (gomuks--render-rooms)
      (message "gomuks: room %s" (if hidden "unhidden" "hidden")))))

(defun gomuks--open-room-id (id)
  "Open room ID and its composer."
  (let ((buffer (get-buffer-create (format "*Gomuks: %s*" id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-room-mode)
        (gomuks-room-mode))
      (setq gomuks--room-id id))
    (gomuks--render-room id)
    (gomuks--show-room buffer)
    (when (with-current-buffer buffer
            (and (null (gethash id gomuks--timelines))
                 (not gomuks--initial-history-requested)))
      (with-current-buffer buffer
        (setq gomuks--initial-history-requested t)
        (gomuks-load-history)))
    (gomuks--maybe-mark-read)))

(defun gomuks--event-id-position (event-id)
  "Find the displayed position of EVENT-ID in the current buffer."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (when (equal (gomuks--alist 'event_id
                                  (get-text-property position 'gomuks-event))
                   event-id)
        (setq found position))
      (setq position (or (next-single-property-change
                          position 'gomuks-event nil (point-max))
                         (point-max))))
    found))

(defun gomuks--show-reply-context (id target source events)
  "Show EVENTS around TARGET in room ID, returning to SOURCE with `b'."
  (let ((buffer (get-buffer-create (format "*Gomuks context: %s*" target))))
    (gomuks--store-events events)
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-room-mode)
        (gomuks-room-mode))
      (setq gomuks--room-id id
            gomuks--thread-root nil
            gomuks--context-target target
            gomuks--context-events events
            gomuks--parent-buffer source)
      (gomuks--render-buffer buffer id))
    (gomuks--show-room buffer)
    (when-let* ((position (with-current-buffer buffer
                            (gomuks--event-id-position target))))
      (goto-char position)
      (recenter))))

(defun gomuks-follow-reply (&optional target)
  "Go to the message replied to at point, or to TARGET when supplied."
  (interactive)
  (setq target (or target (gomuks--reply-target (gomuks--event-at-point))))
  (unless target (user-error "This message is not a reply"))
  (let* ((id gomuks--room-id)
         (source (current-buffer))
         (view (gomuks--view-identity))
         (room (get-buffer (format "*Gomuks: %s*" id)))
         (current-position (gomuks--event-id-position target))
         (generation (gomuks--cache-token id))
         (room-position (and room
                             (with-current-buffer room
                               (gomuks--event-id-position target)))))
    (cond
     (current-position
      (goto-char current-position)
      (recenter))
     (room-position
      (gomuks--show-room room)
      (goto-char room-position)
      (recenter))
     (t
      (gomuks--post
       "get_event_context" `((room_id . ,id) (event_id . ,target) (limit . 12))
       (lambda (failure response)
         (when (and (gomuks--cache-current-p id generation)
                    (gomuks--view-current-p source view))
           (if failure
               (gomuks--post
                "get_event" `((room_id . ,id) (event_id . ,target))
                (lambda (event-failure event)
                  (when (and (gomuks--cache-current-p id generation)
                             (gomuks--view-current-p source view))
                    (if event-failure
                        (message "gomuks: could not follow reply: %s" event-failure)
                      (gomuks--show-reply-context id target source (list event))))))
             (gomuks--store-events (gomuks--alist 'related_events response))
             (gomuks--show-reply-context
              id target source
              (append (reverse (gomuks--alist 'before response))
                      (list (gomuks--alist 'event response))
                      (gomuks--alist 'after response)))))))))))

(defun gomuks--render-search ()
  "Redraw the current search buffer from its result events."
  (let ((inhibit-read-only t)
        (selected (get-text-property (point) 'gomuks-event-id)))
    (erase-buffer)
    (insert (propertize (format "SEARCH  %s\n" (gomuks--room-name gomuks--room-id))
                        'face 'gomuks-title-face)
            (format "%s  ·  RET open  ·  n more  ·  q back\n\n"
                    gomuks--search-query))
    (dolist (event gomuks--search-results)
      (let* ((start (point))
             (timestamp (gomuks--alist 'timestamp event))
             (body (gomuks--visible-body
                    event (gomuks--alist 'body (gomuks--event-content event))))
             (summary (truncate-string-to-width
                       (replace-regexp-in-string "[\n\r]+" " " (or body "[attachment]"))
                       (max 30 (- (window-width) 32)) nil nil "…")))
        (insert (if timestamp
                    (format-time-string "%Y-%m-%d %H:%M "
                                        (seconds-to-time (/ timestamp 1000.0)))
                  "                 ")
                (gomuks--sender-name gomuks--room-id event) ": " summary "\n")
        (put-text-property start (point) 'gomuks-event event)
        (put-text-property start (point) 'gomuks-event-id
                           (gomuks--alist 'event_id event))))
    (when gomuks--search-error
      (insert (propertize (format "Search failed: %s\n" gomuks--search-error)
                          'face 'error)))
    (when (and (not gomuks--search-results) (not gomuks--search-loading)
               (not gomuks--search-error))
      (insert "No results in the local Gomuks history.\n"))
    (when gomuks--search-loading (insert "Searching…\n"))
    (when gomuks--search-next-batch (insert "\nn  More results\n"))
    (goto-char (or (and selected
                        (text-property-any (point-min) (point-max)
                                           'gomuks-event-id selected))
                   (next-single-property-change (point-min) 'gomuks-event-id)
                   (point-min)))))

(defun gomuks--search-fetch (&optional more)
  "Fetch the first or next page of results in the current search buffer.
When MORE is non-nil, use the saved pagination token."
  (unless gomuks--search-loading
    (if (and more (not gomuks--search-next-batch))
        (message "gomuks: no more search results")
      (let* ((buffer (current-buffer))
             (id gomuks--room-id)
             (query gomuks--search-query)
             (generation gomuks--search-generation)
             (cache-token (gomuks--cache-token id))
             (token (and more gomuks--search-next-batch)))
        (setq gomuks--search-loading t)
        (setq gomuks--search-error nil)
        (gomuks--render-search)
        (gomuks--post
         "search_local"
         (append `((search_term . ,query) (room_ids . ,(vector id))
                   (limit . 50) (sort_by_time . t))
                 (when token `((next_batch . ,token))))
         (lambda (failure response)
           (when (and (gomuks--cache-current-p id cache-token)
                      (buffer-live-p buffer))
             (with-current-buffer buffer
               (when (and (derived-mode-p 'gomuks-search-mode)
                          (equal id gomuks--room-id)
                          (= generation gomuks--search-generation))
                 (setq gomuks--search-loading nil)
                 (if failure
                     (setq gomuks--search-error failure)
                   (setq gomuks--search-results
                         (if more
                             (append gomuks--search-results
                                     (gomuks--alist 'events response))
                           (gomuks--alist 'events response))
                         gomuks--search-next-batch
                         (let ((next (gomuks--alist 'next_batch response)))
                           (unless (string-empty-p (or next "")) next)))
                   (gomuks--store-events (gomuks--alist 'events response)))
                 (gomuks--render-search))))))))))

(defun gomuks-search ()
  "Search the backend's local history for messages in this room."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (let ((query (string-trim (read-string "Search room: " nil nil gomuks--search-query)))
        (source (if (derived-mode-p 'gomuks-search-mode)
                    gomuks--parent-buffer (current-buffer)))
        (id gomuks--room-id))
    (when (string-empty-p query) (user-error "Search query is empty"))
    (let ((buffer (get-buffer-create (format "*Gomuks search: %s*" id))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'gomuks-search-mode)
          (gomuks-search-mode))
        (setq gomuks--room-id id
              gomuks--parent-buffer source
              gomuks--search-query query
              gomuks--search-results nil
              gomuks--search-next-batch nil
              gomuks--search-loading nil
              gomuks--search-error nil
              gomuks--search-generation (1+ gomuks--search-generation))
        (gomuks--render-search))
      (delete-other-windows)
      (switch-to-buffer buffer)
      (with-current-buffer buffer (gomuks--search-fetch)))))

(defun gomuks-search-more ()
  "Load the next page of results in this search buffer."
  (interactive)
  (gomuks--search-fetch t))

(defun gomuks-search-open ()
  "Open the search result at point with nearby messages."
  (interactive)
  (let* ((event (gomuks--event-at-point))
         (id gomuks--room-id)
         (target (gomuks--alist 'event_id event))
         (generation (gomuks--cache-token id))
         (view (gomuks--view-identity))
         (source (current-buffer)))
    (unless target (user-error "This search result has no event ID"))
    (gomuks--post
     "get_event_context" `((room_id . ,id) (event_id . ,target) (limit . 12))
     (lambda (failure response)
       (when (and (gomuks--cache-current-p id generation)
                  (gomuks--view-current-p source view))
         (if failure
             (gomuks--show-reply-context id target source (list event))
           (gomuks--store-events (gomuks--alist 'related_events response))
           (gomuks--show-reply-context
            id target source
            (append (reverse (gomuks--alist 'before response))
                    (list (gomuks--alist 'event response))
                    (gomuks--alist 'after response)))))))))

(defun gomuks-search-back ()
  "Return from search results to the room that opened them."
  (interactive)
  (if (buffer-live-p gomuks--parent-buffer)
      (gomuks--show-room gomuks--parent-buffer)
    (gomuks-home)))

(defun gomuks--read-reaction ()
  "Read an emoji reaction by name, or raw text with a prefix argument."
  (if (and (not current-prefix-arg)
           (require 'emoji nil t)
           (fboundp 'emoji--read-emoji))
      (progn (emoji--init) (car (emoji--read-emoji)))
    (if current-prefix-arg
        (read-string "Reaction: ")
      (char-to-string (read-char-by-name "Reaction emoji: " t)))))

(defun gomuks-react (key)
  "React to the message at point with KEY."
  (interactive (list (gomuks--read-reaction)))
  (when (string-empty-p (string-trim key))
    (user-error "Reaction is empty"))
  (let ((event-id (gomuks--alist 'event_id (gomuks--event-at-point))))
    (unless event-id (user-error "This message has no event ID yet"))
    (gomuks--post
     "send_event"
     `((room_id . ,gomuks--room-id) (type . "m.reaction")
       (content . ((m.relates_to . ((rel_type . "m.annotation")
                                    (event_id . ,event-id) (key . ,key))))))
     (lambda (failure _response)
       (message "gomuks: %s" (or failure "reaction queued"))))))

(defun gomuks-reactions-quit ()
  "Close the reaction list and return to the message."
  (interactive)
  (quit-window t))

(defun gomuks-remove-reaction ()
  "Redact your reaction on the current line."
  (interactive)
  (unless (derived-mode-p 'gomuks-reactions-mode)
    (user-error "Open a reaction list first"))
  (let ((event-id (get-text-property (point) 'gomuks-reaction-event-id))
        (sender (get-text-property (point) 'gomuks-reaction-sender))
        (room-id gomuks--room-id)
        (buffer (current-buffer))
        (generation (gomuks--cache-token gomuks--room-id))
        (view (gomuks--view-identity))
        (event gomuks--reactions-event)
        (key gomuks--reactions-key))
    (unless event-id (user-error "No reaction on this line"))
    (unless (and gomuks--user-id (equal sender gomuks--user-id))
      (user-error "You can only remove your own reaction"))
    (when (yes-or-no-p (format "Remove your %s reaction? " key))
      (gomuks--post
       "redact_event" `((room_id . ,room-id) (event_id . ,event-id))
       (lambda (failure _response)
         (when (and (gomuks--cache-current-p room-id generation)
                    (gomuks--view-current-p buffer view))
           (if failure
               (message "gomuks: could not remove reaction: %s" failure)
             (when (and event key (buffer-live-p buffer))
               (with-current-buffer buffer
		 (gomuks-show-reactions event key))))))))))

(defun gomuks-show-reactions (event key)
  "Show the people who reacted to EVENT with KEY."
  (let* ((id gomuks--room-id)
         (event-id (gomuks--alist 'event_id event))
         (generation (gomuks--cache-token id))
         view
         (buffer (get-buffer-create
                  (format "*Gomuks reactions: %s %s*" event-id key))))
    (unless event-id (user-error "This message has no event ID yet"))
    (with-current-buffer buffer
      (gomuks-reactions-mode)
      (setq-local gomuks--room-id id
                  gomuks--reactions-event event
                  gomuks--reactions-key key)
      (setq view (gomuks--view-identity))
      (setq-local header-line-format
                  (concat " " (gomuks--room-name id) "  ·  " key
                          " reactions  ·  d remove yours  ·  q close"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading reactions…\n")))
    (select-window
     (display-buffer buffer '((display-buffer-below-selected)
                              (window-height . 10))))
    (let ((load
           (lambda ()
             (gomuks--post
              "get_related_events"
              `((room_id . ,id) (event_id . ,event-id)
                (relation_type . "m.annotation"))
              (lambda (failure response)
                (when (and (gomuks--cache-current-p id generation)
                           (gomuks--view-current-p buffer view))
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t)
                          (seen (make-hash-table :test 'equal))
                          (count 0))
                      (erase-buffer)
                      (if failure
                          (insert (propertize
                                   (format "Could not load reactions: %s\n" failure)
                                   'face 'error))
                        (dolist (reaction response)
                          (let ((sender (gomuks--alist 'sender reaction))
                                (reaction-id (gomuks--alist 'event_id reaction)))
                            (when (and (stringp sender)
                                       (not (gomuks--alist 'redacted_by reaction))
                                       (equal key
                                              (gomuks--alist
                                               'key (gomuks--alist
                                                     'm.relates_to
                                                     (gomuks--event-content reaction))))
                                       (not (gethash sender seen)))
                              (puthash sender t seen)
                              (setq count (1+ count))
                              (let ((start (point)))
                                (insert (gomuks--sender-name id reaction)
                                        "  " (propertize sender 'face 'shadow)
                                        (if (equal sender gomuks--user-id)
                                            "  [d remove]" "") "\n")
                                (put-text-property start (point)
                                                   'gomuks-reaction-event-id reaction-id)
                                (put-text-property start (point)
                                                   'gomuks-reaction-sender sender)))))
                        (when (= count 0)
                          (insert "No reactions found.\n"))))
                    (goto-char (point-min)))))))))
      (if gomuks--user-id
          (funcall load)
        (gomuks--post
         "get_state" '()
         (lambda (_failure state)
           (when (and (gomuks--cache-current-p id generation)
                      (gomuks--view-current-p buffer view))
             (unless gomuks--user-id
               (setq gomuks--user-id (gomuks--alist 'user_id state)))
             (funcall load))))))))

(defun gomuks-redact ()
  "Redact the message at point after confirmation."
  (interactive)
  (let ((event-id (gomuks--alist 'event_id (gomuks--event-at-point))))
    (unless event-id (user-error "This message has no event ID yet"))
    (when (yes-or-no-p "Redact this message? ")
      (gomuks--post "redact_event"
                    `((room_id . ,gomuks--room-id) (event_id . ,event-id))
                    (lambda (failure _response)
                      (message "gomuks: %s" (or failure "redacted")))))))

(defun gomuks-mark-read ()
  "Mark the message at point as read."
  (interactive)
  (let ((event-id (gomuks--alist 'event_id (gomuks--event-at-point))))
    (unless event-id (user-error "This message has no event ID yet"))
    (gomuks--post "mark_read"
                  `((room_id . ,gomuks--room-id) (event_id . ,event-id)
                    (receipt_type . "m.read"))
                  (lambda (failure _response)
                    (message "gomuks: %s" (or failure "marked read"))))))

(defun gomuks-load-history ()
  "Load an older page of messages in the current room."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (catch 'gomuks-history-done
    (when gomuks--thread-root
      (if (not gomuks--thread-next-batch)
          (message "gomuks: no older thread messages")
        (let ((buffer (current-buffer))
              (id gomuks--room-id)
              (view (gomuks--view-identity))
              (root gomuks--thread-root)
              (generation (gomuks--cache-token gomuks--room-id))
              (token gomuks--thread-next-batch))
          (gomuks--post
           "paginate_manual"
           `((room_id . ,id) (thread_root . ,root) (since . ,token)
             (direction . "b") (limit . 50))
           (lambda (failure response)
             (when (and (gomuks--cache-current-p id generation)
                        (gomuks--view-current-p buffer view))
               (if failure (message "gomuks: %s" failure)
                 (with-current-buffer buffer
                   (gomuks--store-events
                    (append (gomuks--alist 'events response)
                            (gomuks--alist 'related_events response)))
                   (setq gomuks--thread-events
                         (gomuks--merge-thread-events
                          gomuks--thread-root gomuks--thread-events
                          (gomuks--alist 'events response))
                         gomuks--thread-next-batch
                         (let ((next (gomuks--alist 'next_batch response)))
                           (unless (string-empty-p (or next "")) next)))
                   (gomuks--render-room id))))))))
      (throw 'gomuks-history-done nil))
    (let* ((id gomuks--room-id)
           (buffer (current-buffer))
           (view (gomuks--view-identity))
           (generation (gomuks--cache-token id))
           (oldest (car (gethash id gomuks--timelines))))
      (gomuks--post
       "paginate" `((room_id . ,id)
                    (max_timeline_id . ,(or (gethash oldest gomuks--timeline-ids) 0))
                    (limit . 50))
       (lambda (failure response)
         (when (and (gomuks--cache-current-p id generation)
                    (gomuks--view-current-p buffer view))
           (if failure
               (progn
                 (with-current-buffer buffer
                   (setq gomuks--initial-history-requested nil))
                 (message "gomuks: %s" failure))
             (let ((new-rows nil))
               (gomuks--store-events
                (append (gomuks--alist 'events response)
                        (gomuks--alist 'related_events response)))
               (dolist (event (gomuks--alist 'events response))
                 (when-let* ((rowid (gomuks--alist 'rowid event)))
                   (puthash rowid (gomuks--alist 'timeline_rowid event)
                            gomuks--timeline-ids)
                   (when (and (gomuks--alist 'timeline_rowid event)
                              (not (member rowid (gethash id gomuks--timelines))))
                     (push rowid new-rows))))
               (puthash id (append new-rows (gethash id gomuks--timelines))
                        gomuks--timelines)
               (with-current-buffer buffer (gomuks--render-room id))
               (gomuks--maybe-mark-read)
               (message "gomuks: loaded %d older messages" (length new-rows))))))))))

(defun gomuks-open-thread (&optional event)
  "Open the thread containing EVENT, or the message at point."
  (interactive)
  (let* ((source (current-buffer))
         (event (or event (gomuks--event-at-point)))
         (root (gomuks--thread-root-id event))
         (id gomuks--room-id)
         (generation (gomuks--cache-token id))
         (view nil)
         (root-event (if (equal root (gomuks--alist 'event_id event))
                         event (gomuks--find-event id root)))
         (buffer (get-buffer-create (format "*Gomuks thread: %s*" root)))
         (existing (with-current-buffer buffer
                     (and (equal gomuks--room-id id)
                          (equal gomuks--thread-root root)
                          gomuks--initial-history-requested))))
    (unless root (user-error "This message has no event ID yet"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-room-mode)
        (gomuks-room-mode))
      (setq gomuks--room-id id gomuks--thread-root root
            gomuks--context-target nil
            gomuks--parent-buffer (if (eq source buffer)
                                      gomuks--parent-buffer source)
            gomuks--thread-events
            (gomuks--merge-thread-events
             root (and existing gomuks--thread-events)
             (if root-event (list root-event) nil)))
      (unless existing
        (setq gomuks--thread-next-batch nil
              gomuks--initial-history-requested t))
      (setq view (gomuks--view-identity)))
    (gomuks--render-buffer buffer id)
    (gomuks--show-room buffer)
    (unless root-event
      (gomuks--post
       "get_event" `((room_id . ,id) (event_id . ,root))
       (lambda (failure fetched)
         (when (and (gomuks--cache-current-p id generation)
                    (gomuks--view-current-p buffer view))
           (if failure
               (message "gomuks: thread root unavailable: %s" failure)
             (with-current-buffer buffer
               (gomuks--store-events (list fetched))
               (setq gomuks--thread-events
                     (gomuks--merge-thread-events
                      root gomuks--thread-events (list fetched)))
               (gomuks--render-buffer buffer id)))))))
    (unless existing
      (gomuks--post
       "paginate_manual"
       `((room_id . ,id) (thread_root . ,root) (direction . "b") (limit . 50))
       (lambda (failure response)
         (when (and (gomuks--cache-current-p id generation)
                    (gomuks--view-current-p buffer view))
           (if failure
               (progn
                 (with-current-buffer buffer (setq gomuks--initial-history-requested nil))
                 (message "gomuks: %s" failure))
             (with-current-buffer buffer
               (gomuks--store-events
                (append (gomuks--alist 'events response)
                        (gomuks--alist 'related_events response)))
               (setq gomuks--thread-events
                     (gomuks--merge-thread-events
                      root gomuks--thread-events
                      (gomuks--alist 'events response))
                     gomuks--thread-next-batch
                     (let ((next (gomuks--alist 'next_batch response)))
                       (unless (string-empty-p (or next "")) next)))
               (gomuks--render-room id)))))))))

(provide 'gomuks-view)
;;; gomuks-view.el ends here
