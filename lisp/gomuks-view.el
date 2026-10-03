;;; gomuks-view.el --- Room navigation and history workflows -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Own view identities and pagination. Cached-state rendering lives in gomuks-render.

;;; Code:

(require 'gomuks-compose)
(require 'gomuks-render)
(declare-function gomuks-rooms-mode "gomuks-ui")
(declare-function gomuks-room-mode "gomuks-ui")
(declare-function gomuks-search-mode "gomuks-ui")
(declare-function gomuks-reactions-mode "gomuks-ui")
(declare-function notifications-notify "notifications")
(declare-function emoji--init "emoji")
(declare-function emoji--read-emoji "emoji")

(defun gomuks--view-identity ()
  "Return the identity of the current room, search, or reactions view."
  (list gomuks--view-generation major-mode gomuks--room-id gomuks--thread-root gomuks--context-target
        gomuks--search-generation gomuks--reactions-key
        (gomuks--alist 'event_id (gomuks--reaction-target))))

(defun gomuks--reaction-target ()
  "Query the current reaction target, accepting an old buffer on reload."
  (cond ((listp gomuks--reactions-event) gomuks--reactions-event)
        ((stringp gomuks--reactions-event)
         (or (gomuks--find-event gomuks--room-id gomuks--reactions-event)
             `((event_id . ,gomuks--reactions-event))))
        (t (gethash gomuks--reactions-event gomuks--events))))

(defun gomuks--view-current-p (buffer identity)
  "Return non-nil when BUFFER is live and still represents IDENTITY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer (equal identity (gomuks--view-identity)))))

(defun gomuks--navigation-current-p (buffer identity serial)
  "Query whether BUFFER still owns navigation SERIAL for view IDENTITY."
  (and (gomuks--view-current-p buffer identity)
       (= serial (buffer-local-value 'gomuks--navigation-serial buffer))))

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
              gomuks--history-state 'idle
              gomuks--history-serial (1+ gomuks--history-serial)
              gomuks--view-generation (cl-incf gomuks--view-serial)
              gomuks--view-dirty t
              gomuks--thread-events nil gomuks--thread-next-batch nil
              gomuks--context-events nil
              gomuks--search-results nil gomuks--search-next-batch nil
              gomuks--search-loading nil gomuks--search-error nil
              gomuks--search-generation (1+ gomuks--search-generation)
              gomuks--reactions-event nil gomuks--reactions-key nil
              header-line-format nil)
        (let ((inhibit-read-only t)) (erase-buffer))))))


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

(defun gomuks--view-events ()
  "Query current events belonging to this room, thread, context or search."
  (gomuks--events-for-rows
   (gomuks--event-rows
    (cond ((derived-mode-p 'gomuks-search-mode) gomuks--search-results)
          (gomuks--context-target gomuks--context-events)
          (gomuks--thread-root gomuks--thread-events)
          (t (gethash gomuks--room-id gomuks--timelines))))))

(defun gomuks--refresh-view (buffer)
  "Procedure: render BUFFER, then explicitly schedule its missing dependencies."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((id gomuks--room-id)
            (events (gomuks--view-events)))
        (if (derived-mode-p 'gomuks-search-mode)
            (progn (gomuks--render-search) (setq gomuks--view-dirty nil))
          (gomuks--render-buffer buffer id))
        (gomuks--request-missing-members id events)
        (dolist (event events)
          (when-let* ((attachment (gomuks--attachment
                                   (gomuks--effective-content event) event)))
            (gomuks--maybe-preview-image attachment id)))))))

(defun gomuks--render-room (id)
  "Procedure: invalidate ID's retained views and refresh each visible view once."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'gomuks-room-mode 'gomuks-search-mode)
                 (equal gomuks--room-id id))
        (setq gomuks--view-dirty t)
        (when (get-buffer-window buffer t) (gomuks--refresh-view buffer))))))

(defun gomuks--refresh-visible-views (&rest _)
  "Refresh dirty Gomuks views newly displayed in a window."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'no-minibuffer))
      (let ((buffer (window-buffer window)))
        (when (buffer-local-value 'gomuks--view-dirty buffer)
          (gomuks--refresh-view buffer))))))

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
  "Persist a proposed visibility preference, then commit it to live state."
  (interactive)
  (let ((id (or gomuks--room-id (get-text-property (point) 'gomuks-room-id))))
    (unless id (user-error "No room selected"))
    (let* ((hidden (member id gomuks--hidden-room-ids))
           (proposed (if hidden (remove id gomuks--hidden-room-ids)
                       (cons id gomuks--hidden-room-ids))))
      (let ((gomuks--hidden-room-ids proposed)) (gomuks--save-hidden-rooms))
      (setq gomuks--hidden-room-ids proposed)
      (gomuks--render-rooms)
      (message "gomuks: room %s" (if hidden "unhidden" "hidden")))))

(defun gomuks--open-room-id (id)
  "Open room ID and its composer."
  (let ((buffer (get-buffer-create (format "*Gomuks: %s*" id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-room-mode)
        (gomuks-room-mode))
      (setq gomuks--room-id id))
    (gomuks--show-room buffer)
    (gomuks--refresh-view buffer)
    (when (with-current-buffer buffer
            (and (null (gethash id gomuks--timelines))
                 (not gomuks--initial-history-requested)))
      (with-current-buffer buffer
        (setq gomuks--initial-history-requested t)
        (gomuks-load-history)))
    (gomuks--maybe-mark-read)))

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
            gomuks--context-events (gomuks--event-rows events)
            gomuks--parent-buffer source)
      (gomuks--refresh-view buffer))
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
         (navigation (cl-incf gomuks--navigation-serial))
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
                    (gomuks--navigation-current-p source view navigation))
           (if failure
               (gomuks--post
                "get_event" `((room_id . ,id) (event_id . ,target))
                (lambda (event-failure event)
                  (when (and (gomuks--cache-current-p id generation)
                             (gomuks--navigation-current-p source view navigation))
                    (if event-failure
                        (message "gomuks: could not follow reply: %s" event-failure)
                      (gomuks--show-reply-context id target source (list event))))))
             (gomuks--store-events (gomuks--alist 'related_events response))
             (gomuks--show-reply-context
              id target source
              (append (reverse (gomuks--alist 'before response))
                      (list (gomuks--alist 'event response))
                      (gomuks--alist 'after response)))))))))))

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
  "Advance this view's explicit history workflow by one older page."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (when gomuks--context-target
    (user-error "Reply context is a fixed excerpt; open the room to load older history"))
  (pcase gomuks--history-state
    ('loading (message "gomuks: history is already loading"))
    ('exhausted (message "gomuks: no older messages"))
    (_ (gomuks--request-history))))

(defun gomuks--request-history ()
  "Procedure: capture ownership, request one page, then commit it if still current."
  (let* ((buffer (current-buffer)) (id gomuks--room-id)
         (view (gomuks--view-identity)) (generation (gomuks--cache-token id))
         (serial (cl-incf gomuks--history-serial)) (root gomuks--thread-root)
         (token gomuks--thread-next-batch)
         (oldest (car (gethash id gomuks--timelines)))
         (cursor (or (gethash oldest gomuks--timeline-ids) 0))
         (command (if root "paginate_manual" "paginate"))
         (data (if root
                   (append `((room_id . ,id) (thread_root . ,root)
                             (direction . "b") (limit . 50))
                           (when token `((since . ,token))))
                 `((room_id . ,id) (max_timeline_id . ,cursor) (limit . 50))))
         (complete
          (lambda (failure response)
            (when (and (gomuks--cache-current-p id generation)
                       (gomuks--view-current-p buffer view))
              (with-current-buffer buffer
                (when (and (= serial gomuks--history-serial)
                           (eq gomuks--history-state 'loading))
                  (if failure
                      (progn
                        (setq gomuks--history-state 'error
                              gomuks--initial-history-requested nil)
                        (message "gomuks: history failed: %s" failure))
                    (let* ((events (gomuks--alist 'events response))
                           (next (gomuks--alist 'next_batch response))
                           added)
                      (gomuks--store-events
                       (append events (gomuks--alist 'related_events response)))
                      (if root
                          (setq gomuks--thread-events
                                (gomuks--merge-thread-rows root gomuks--thread-events events)
                                gomuks--thread-next-batch
                                (and (stringp next) (not (string-empty-p next))
                                     (not (equal next token)) next)
                                gomuks--history-state
                                (if gomuks--thread-next-batch 'idle 'exhausted))
                        (setq added
                              (gomuks--store-timeline
                               id (mapcar (lambda (event)
                                            `((event_rowid . ,(gomuks--alist 'rowid event))
                                              (timeline_rowid . ,(gomuks--alist 'timeline_rowid event))))
                                          (reverse events)) t)
                              gomuks--history-state (if (and added (or (not (assq 'has_more response))
								       (gomuks--alist 'has_more response)))
							'idle 'exhausted)))
                      (setq gomuks--initial-history-requested t)
                      (gomuks--render-room id)
                      (gomuks--maybe-mark-read)))))))))
    (setq gomuks--history-state 'loading gomuks--initial-history-requested t)
    (condition-case err (gomuks--post command data complete)
      (error (funcall complete (error-message-string err) nil)))))

(defun gomuks-open-thread (&optional event)
  "Open the thread containing EVENT, or the message at point."
  (interactive)
  (let* ((source (current-buffer))
         (event (or event (gomuks--event-at-point)))
         (root (gomuks--thread-root-id event))
         (id gomuks--room-id)
         (generation (gomuks--cache-token id))
         (view nil)
         (root-event (or (gomuks--find-event id root)
                         (when (equal root (gomuks--alist 'event_id event)) event)))
         (buffer (get-buffer-create (format "*Gomuks thread: %s*" root)))
         (existing (with-current-buffer buffer
                     (and (equal gomuks--room-id id)
                          (equal gomuks--thread-root root)
                          gomuks--initial-history-requested))))
    (unless root (user-error "This message has no event ID yet"))
    (when root-event (gomuks--store-events (list root-event)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'gomuks-room-mode)
        (gomuks-room-mode))
      (setq gomuks--room-id id gomuks--thread-root root
            gomuks--context-target nil
            gomuks--parent-buffer (if (eq source buffer)
                                      gomuks--parent-buffer source)
            gomuks--thread-events
            (gomuks--merge-thread-rows
             root (and existing gomuks--thread-events)
             (if root-event (list root-event) nil)))
      (unless existing
        (setq gomuks--thread-next-batch nil
              gomuks--initial-history-requested t))
      (setq view (gomuks--view-identity)))
    (gomuks--refresh-view buffer)
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
                     (gomuks--merge-thread-rows
                      root gomuks--thread-events (list fetched)))
               (gomuks--refresh-view buffer)))))))
    (unless existing
      (with-current-buffer buffer
        (setq gomuks--history-state 'idle)
        (gomuks-load-history)))))


(provide 'gomuks-view)
;;; gomuks-view.el ends here
