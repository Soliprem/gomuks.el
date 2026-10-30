;;; gomuks-render.el --- Drawing views from the cache -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Draw the home page, timelines and search results from cached state.
;; Drawing never starts requests; `gomuks--refresh-view' does that afterwards.

;;; Code:

(require 'gomuks-media)
(require 'gomuks-content)
(require 'dom)
(require 'shr)
(declare-function gomuks-follow-reply "gomuks-view" (&optional target))
(declare-function gomuks-open-thread "gomuks-view" (&optional event))
(declare-function gomuks-show-reactions "gomuks-reactions" (event key))
(declare-function gomuks--reaction-help "gomuks-reactions" (event key count))

(defun gomuks--person-info (id user timestamp)
  "Describe USER in room ID, including a local TIMESTAMP when available."
  (let ((name (gomuks--one-line (gomuks--sender-name id `((sender . ,user))))))
    (concat name (unless (equal name user) (format " (%s)" user))
            (when (numberp timestamp)
              (concat " · " (gomuks--format-time "%Y-%m-%d %H:%M:%S %Z" timestamp))))))

(defun gomuks--hover-text (text target)
  "Mark TEXT as popup details for hover TARGET."
  (propertize text 'gomuks-hover-target target 'gomuks-hover-buffer (current-buffer)
              'help-echo-inhibit-substitution t))

(defun gomuks--room-preview (event topic)
  "Describe the latest EVENT in a room, falling back to TOPIC."
  (let* ((content (gomuks--effective-content event))
         (attachment (gomuks--attachment content event))
         (kind (plist-get attachment :kind))
         (body (gomuks--alist 'body content)))
    (cond
     ((gomuks--alist 'redacted_by event) "[redacted]")
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
            (entries (gomuks--rooms-by-recency
                      (lambda (id) (not (member id gomuks--hidden-room-ids)))))
            (selected (get-text-property (point) 'gomuks-room-id))
            (unread-total 0)
            (status (if (and (string= gomuks--connection-status "Disconnected")
                             (process-live-p gomuks--stream))
                        "Connected" gomuks--connection-status)))
        (dolist (entry entries)
          (cl-incf unread-total
                   (or (gomuks--alist 'unread_notifications (cdr entry)) 0)))
        (erase-buffer)
        (insert "\n  " (propertize "gomuks" 'face 'gomuks-title-face)
                "     " (propertize status
                                    'face (if (string= status "Connected")
                                              'success 'shadow))
                "\n  " (propertize "Gomacs? Emuks?" 'face 'shadow)
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
                                 (gomuks--one-line preview)
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

(defun gomuks--formatted-body (content event)
  "Render CONTENT's Matrix HTML, or return nil if it cannot be rendered.
Remove the quoted reply fallback for EVENT before rendering."
  (when (and (equal (gomuks--alist 'format content) "org.matrix.custom.html")
             (stringp (gomuks--alist 'formatted_body content))
             (fboundp 'libxml-parse-html-region))
    (condition-case nil
        (with-temp-buffer
          (insert (gomuks--alist 'formatted_body content))
          (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
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

(defun gomuks--event-summary (event width)
  "Return EVENT's text on one line, truncated to WIDTH."
  (truncate-string-to-width
   (gomuks--one-line
    (if (gomuks--alist 'redacted_by event) "[redacted]"
      (or (gomuks--visible-body
           event (gomuks--alist 'body (gomuks--effective-content event)))
          "[attachment]")))
   width nil nil "…"))

(defun gomuks--reply-summary (room-id event-id)
  "Return a short label for EVENT-ID in ROOM-ID."
  (if-let* ((event (gomuks--find-event room-id event-id)))
      (format "%s: %s" (gomuks--sender-name room-id event)
              (gomuks--event-summary event 72))
    "earlier message"))

(defun gomuks--thread-counts (room-id)
  "Return a table of thread reply counts in ROOM-ID, keyed by root ID."
  (let ((counts (make-hash-table :test 'equal)))
    (maphash (lambda (key rows)
               (when (equal (car key) room-id)
                 (when (> (hash-table-count rows) 0)
                   (puthash (cdr key) (hash-table-count rows) counts))))
             gomuks--thread-index)
    counts))

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
           ;; An underline crosses the image at its baseline.
           'face '(:inherit link :underline nil) 'follow-link t
           'help-echo "Open sticker; press D to save"
           'display (when (and gomuks-inline-images cached (display-images-p)
                               (gomuks--preview-file-p cached))
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
      (when (and gomuks-inline-images cached (display-images-p)
                 (gomuks--preview-file-p cached) (equal kind "m.image"))
        (condition-case nil
            (when-let* ((image (create-image cached nil nil
                                             :max-width 360 :max-height 220)))
              (insert "    ")
              (insert-image image "[image preview]")
              (insert "\n"))
          (error nil))))
    nil))

(defun gomuks--event-date (event)
  "Return EVENT's local calendar date, or nil without a timestamp."
  (when-let* ((timestamp (gomuks--alist 'timestamp event))
              ((numberp timestamp)))
    (gomuks--format-time "%Y-%m-%d" timestamp)))

(defun gomuks--readers-for-events (id events)
  "Map displayed event IDs to public receipt records in room ID's EVENTS.
Receipts on hidden events attach to the preceding visible message."
  (let ((positions (make-hash-table :test 'equal))
        (readers (make-hash-table :test 'equal)) previous)
    (dolist (event events)
      (let* ((content (gomuks--effective-content event))
             (event-id (gomuks--alist 'event_id event)))
        (when (or (gomuks--alist 'redacted_by event)
                  (gomuks--alist 'body content)
                  (gomuks--formatted-body content event)
                  (gomuks--attachment content event)
                  (equal (gomuks--event-type event) "m.room.encrypted"))
          (setq previous event-id))
        (when event-id (puthash event-id previous positions))))
    (when-let* ((state (gethash id gomuks--receipts)))
      (maphash
       (lambda (_key receipt)
         (let* ((user (gomuks--alist 'user_id receipt))
                (thread (gomuks--alist 'thread_id receipt))
                (position (gethash (gomuks--alist 'event_id receipt) positions)))
           (when (and position (not (equal user gomuks--user-id))
                      (or (null thread) (equal thread "")
                          (equal thread (or gomuks--thread-root "main"))))
             (cl-pushnew receipt (gethash position readers) :test #'equal))))
       state))
    readers))

(defun gomuks--insert-event (event &optional previous thread-counts readers)
  "Insert EVENT, grouping it after PREVIOUS when appropriate.
THREAD-COUNTS maps thread root IDs to known reply counts.
READERS maps displayed event IDs to the public receipts at those positions."
  (let* ((content (gomuks--effective-content event))
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
         (kind (gomuks--event-type event)))
    (when (or body attachment (string= kind "m.room.encrypted"))
      (let* ((start (point))
             (sender (gomuks--alist 'sender event))
             (name (gomuks--sender-name gomuks--room-id event))
             (timestamp (gomuks--alist 'timestamp event))
             (previous-time (gomuks--alist 'timestamp previous))
             (date (gomuks--event-date event))
             (previous-date (gomuks--event-date previous))
             (grouped (and previous (equal sender (gomuks--alist 'sender previous))
                           (equal name (gomuks--sender-name gomuks--room-id previous))
                           (equal date previous-date)
                           (numberp timestamp) (numberp previous-time)
                           (<= 0 (- timestamp previous-time) (* 15 60 1000)))))
        (when (and date (not (equal date previous-date)))
          (insert (propertize
                   (concat "\n── " (gomuks--format-time "%A, %Y-%m-%d" timestamp)
                           " ──\n\n")
                   'face 'shadow 'gomuks-date date
                   'gomuks-event nil 'gomuks-event-rowid nil)))
        (setq start (point))
        (insert (propertize
                 (if timestamp (gomuks--format-time "%H:%M" timestamp) "     ")
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
            (put-text-property body-start (point) 'wrap-prefix "        ")
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
            (let ((key (gomuks--key-string (car entry))) (count (cdr entry)))
              (insert-text-button
               (format "%s %s" key (cdr entry))
               'face 'link 'follow-link t
               'help-echo (lambda (window _object _position)
                            (with-current-buffer (window-buffer window)
                              (gomuks--reaction-help event key count)))
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
        (when-let* ((receipts (and readers (gethash (gomuks--alist 'event_id event) readers))))
          (let* ((users (sort (delete-dups (mapcar (lambda (receipt) (gomuks--alist 'user_id receipt))
                                                 receipts)) #'string-lessp))
                 (details (sort (copy-sequence receipts)
                                (lambda (a b) (string-lessp (gomuks--alist 'user_id a)
                                                           (gomuks--alist 'user_id b))))))
            (insert "        "
                    (propertize
                     (concat "Read by "
                             (mapconcat (lambda (user)
                                          (gomuks--sender-name gomuks--room-id `((sender . ,user))))
                                        users ", "))
                     'face 'shadow 'mouse-face 'highlight 'wrap-prefix "        "
                     'help-echo
                     (gomuks--hover-text
                      (concat "Read receipts\n"
                              (mapconcat
                               (lambda (receipt)
                                 (concat (gomuks--person-info gomuks--room-id
                                                             (gomuks--alist 'user_id receipt)
                                                             (gomuks--alist 'timestamp receipt))
                                         (when-let* ((thread (gomuks--alist 'thread_id receipt)))
                                           (if (equal thread "main") " · main timeline" " · thread"))))
                               details "\n"))
                      (list 'receipt (gomuks--alist 'event_id event))))
                    "\n")))
        (unless (bolp) (insert "\n"))
        (put-text-property start (point) 'gomuks-event event)
        (put-text-property start (point) 'gomuks-event-rowid
                           (gomuks--alist 'rowid event))
        t))))

(defun gomuks--room-header (id kind)
  "Return the header line for room ID's view of KIND."
  (let* ((name (gomuks--one-line (gomuks--room-name id)))
         (topic (gomuks--alist 'topic (gethash id gomuks--rooms)))
         (label (pcase kind ('context "REPLY CONTEXT") ('thread "THREAD"))))
    (concat " " (propertize name 'face 'gomuks-room-face)
            (when (gethash id gomuks--muted-rooms)
              (propertize "  [muted]" 'face 'shadow))
            (when label (concat "  ·  " (propertize label 'face 'gomuks-heading-face)))
            (when (and (stringp topic) (not (string-empty-p topic)))
              (concat "  ·  "
                      (propertize
                       (truncate-string-to-width
                        (gomuks--one-line topic)
                        (max 20 (- (window-width) (string-width name) 10))
                        nil nil "…")
                       'face 'shadow))))))

(defun gomuks--position-anchor (position)
  "Remember POSITION as an event row and offset, or an absolute fallback."
  (if (= position (point-max)) 'end
    (let* ((row (get-text-property position 'gomuks-event-rowid))
           (start (and row (text-property-any (point-min) (point-max)
                                               'gomuks-event-rowid row))))
      (list row (and start (- position start)) position))))

(defun gomuks--anchor-position (anchor)
  "Resolve ANCHOR after a timeline redraw, clamping within its event."
  (if (eq anchor 'end) (point-max)
    (pcase-let ((`(,row ,offset ,fallback) anchor))
      (if-let* ((start (and row (text-property-any (point-min) (point-max)
                                                 'gomuks-event-rowid row))))
          (min (+ start offset)
               (1- (next-single-property-change start 'gomuks-event-rowid
                                                nil (point-max))))
        (min fallback (point-max))))))

(defun gomuks--render-buffer (buffer id)
  "Redraw room ID's BUFFER from the cache and return the events shown."
  ;; visible redraw stays O(n); bound or incrementally update history
  ;; when measured latency requires it. Hidden views defer this work.
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (kind (cond (gomuks--context-target 'context)
                         (gomuks--thread-root 'thread) (t 'room)))
             (events (gomuks--events-for-rows
                      (gomuks--event-rows
                       (pcase kind
                         ('context gomuks--context-events)
                         ('thread gomuks--thread-events)
                         ('room (gethash id gomuks--timelines))))))
             (anchor (gomuks--position-anchor (point)))
             (windows (mapcar (lambda (window)
                                (list window
                                      (gomuks--position-anchor (window-point window))
                                      (gomuks--position-anchor (window-start window))))
                              (get-buffer-window-list buffer nil t)))
             (thread-counts (when (eq kind 'room) (gomuks--thread-counts id)))
             (readers (gomuks--readers-for-events id events))
             (previous nil))
        (setq-local header-line-format (gomuks--room-header id kind))
        (erase-buffer)
        (insert (propertize (if (bound-and-true-p evil-mode)
                                "i" "C-c C-s")
                            'face 'help-key-binding)
                " compose   "
                (if (eq kind 'context) ""
                  (concat (propertize (if (bound-and-true-p evil-mode) "p" "M-p")
                                      'face 'help-key-binding) " older   "))
                (propertize "C-k" 'face 'help-key-binding)
                " switch   " (propertize "b" 'face 'help-key-binding)
                " home   " (propertize "o" 'face 'help-key-binding)
                " open   " (propertize "D" 'face 'help-key-binding)
                " save\n\n")
        (dolist (event events)
          (when (gomuks--insert-event event previous thread-counts readers)
            (setq previous event)))
        (goto-char (gomuks--anchor-position anchor))
        (dolist (state windows)
          (pcase-let ((`(,window ,point-anchor ,start-anchor) state))
            (when (window-live-p window)
              (set-window-point window (gomuks--anchor-position point-anchor))
              (if (eq point-anchor 'end)
                  (with-selected-window window (recenter -1))
                (set-window-start window (gomuks--anchor-position start-anchor) t)))))
        (setq gomuks--view-dirty nil)
        events))))

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

(defun gomuks--render-search ()
  "Redraw this search buffer and return the events shown."
  (let ((inhibit-read-only t)
        (events (gomuks--events-for-rows (gomuks--event-rows gomuks--search-results)))
        (selected (get-text-property (point) 'gomuks-event-id)))
    (erase-buffer)
    (insert (propertize (format "SEARCH  %s\n" (gomuks--room-name gomuks--room-id))
                        'face 'gomuks-title-face)
            (format "%s  ·  RET open  ·  n more  ·  q back\n\n"
                    gomuks--search-query))
    (dolist (event events)
      (let ((start (point))
            (timestamp (gomuks--alist 'timestamp event))
            (summary (gomuks--event-summary event (max 30 (- (window-width) 32)))))
        (insert (if timestamp (gomuks--format-time "%Y-%m-%d %H:%M " timestamp)
                  "                 ")
                (gomuks--sender-name gomuks--room-id event) ": " summary "\n")
        (put-text-property start (point) 'gomuks-event event)
        (put-text-property start (point) 'gomuks-event-id
                           (gomuks--alist 'event_id event))
        (put-text-property start (point) 'gomuks-event-rowid
                           (gomuks--alist 'rowid event))))
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
                   (point-min)))
    (setq gomuks--view-dirty nil)
    events))

(provide 'gomuks-render)
;;; gomuks-render.el ends here
