;;; gomuks-core.el --- Shared state and cache ownership -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Cache writers own domain tables; queries never start effects.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'browse-url)
(require 'json)
(require 'dom)
(require 'shr)
(require 'subr-x)
(require 'url)
(require 'url-http)
(defgroup gomuks nil "Frontend for the gomuks Matrix backend." :group 'applications)
(defcustom gomuks-backend-url "http://localhost:29325"
  "Base URL of the gomuks backend."
  :type 'string :group 'gomuks)
(defcustom gomuks-username nil
  "Backend basic-auth username; nil uses auth-source or prompts."
  :type '(choice (const nil) string) :group 'gomuks)
(defcustom gomuks-reconnect-delay 5
  "Seconds before reconnecting after the event stream closes."
  :type 'number :group 'gomuks)
(defcustom gomuks-desktop-notifications t
  "Show desktop notifications for events flagged by the backend."
  :type 'boolean :group 'gomuks)
(defcustom gomuks-echo-area-notifications 'in-gomuks
  "When to show incoming message notifications in the Emacs echo area.
The default shows them only while a Gomuks buffer is selected."
  :type '(choice (const :tag "Only while in Gomuks" in-gomuks)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'gomuks)
(defcustom gomuks-inline-images t
  "Show small image attachments inside room timelines."
  :type 'boolean :group 'gomuks)
(defcustom gomuks-inline-image-max-bytes (* 4 1024 1024)
  "Largest image automatically downloaded for an inline preview."
  :type 'integer :group 'gomuks)
(defcustom gomuks-request-timeout 30
  "Deadline in seconds for ordinary backend requests."
  :type 'number :group 'gomuks)
(defcustom gomuks-transfer-timeout 120
  "Deadline in seconds for uploads and explicit media downloads."
  :type 'number :group 'gomuks)
(defcustom gomuks-preview-concurrency 2
  "Maximum number of automatic image downloads running together."
  :type 'integer :group 'gomuks)
(defcustom gomuks-media-cache-max-bytes (* 64 1024 1024)
  "Maximum total bytes retained in the downloaded media cache."
  :type 'integer :group 'gomuks)
(defcustom gomuks-audio-backend 'empv
  "Use EMPV or EMMS to play audio attachments."
  :type '(choice (const empv) (const emms)) :group 'gomuks)
(defcustom gomuks-hidden-rooms-file
  (locate-user-emacs-file "gomuks-hidden-rooms")
  "Local file containing room IDs hidden from the Gomuks interface."
  :type 'file :group 'gomuks)

(defface gomuks-title-face
  '((t :inherit bold :height 1.6)) "Home page title." :group 'gomuks)
(defface gomuks-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Section headings." :group 'gomuks)
(defface gomuks-room-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Room names." :group 'gomuks)
(defface gomuks-sender-face
  '((t :inherit font-lock-type-face :weight bold))
  "Message senders." :group 'gomuks)
(defface gomuks-unread-face
  '((t :inherit warning :weight bold))
  "Unread counters." :group 'gomuks)

(defvar gomuks--password nil
  "Password used for backend basic authentication.")
(defvar gomuks--user-id nil
  "Matrix user ID reported by the backend.")
(defvar gomuks--skip-auth-source nil
  "Non-nil after manually overriding stored credentials this session.")
(defvar gomuks--stream nil
  "Process receiving the Gomuks event stream.")
(defvar gomuks--reconnect-timer nil
  "Timer scheduled to reconnect the event stream.")
(defvar gomuks--pending ""
  "Incomplete line currently buffered from the event stream.")
(defvar gomuks--rooms (make-hash-table :test 'equal)
  "Room metadata keyed by room ID.")
(defvar gomuks--hidden-room-ids nil
  "Room IDs hidden from the home page, normal switcher, and notifications.")

(defun gomuks--load-hidden-rooms ()
  "Load hidden room IDs from `gomuks-hidden-rooms-file'."
  (setq gomuks--hidden-room-ids
        (when (file-exists-p gomuks-hidden-rooms-file)
          (condition-case err
              (with-temp-buffer
                (insert-file-contents gomuks-hidden-rooms-file)
                (let ((ids (read (current-buffer))))
                  (unless (and (listp ids) (cl-every #'stringp ids))
                    (error "Invalid hidden room IDs"))
                  ids))
            (error
             (message "gomuks: could not load hidden rooms: %s"
                      (error-message-string err))
             nil)))))

(defun gomuks--save-hidden-rooms ()
  "Save hidden room IDs to `gomuks-hidden-rooms-file'."
  (let* ((directory (file-name-directory (expand-file-name gomuks-hidden-rooms-file)))
         (temporary nil))
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file (expand-file-name ".gomuks-hidden-" directory)))
          (with-temp-file temporary
            (prin1 gomuks--hidden-room-ids (current-buffer))
            (insert "\n"))
          (rename-file temporary gomuks-hidden-rooms-file t)
          (setq temporary nil))
      (when temporary (delete-file temporary)))))

(gomuks--load-hidden-rooms)
(defvar gomuks--muted-rooms (make-hash-table :test 'equal)
  "Room IDs muted by the current account's push rules.")
(defvar gomuks--events (make-hash-table :test 'equal)
  "Cached events keyed by backend row ID.")
(defvar gomuks--event-index (make-hash-table :test 'equal)
  "Event rows indexed by (ROOM-ID . EVENT-ID).")
(defvar gomuks--thread-index (make-hash-table :test 'equal)
  "Thread reply row sets indexed by (ROOM-ID . ROOT-ID).")
(defvar gomuks--member-state (make-hash-table :test 'equal)
  "Member event row IDs keyed by room ID and sender ID.")
(defvar gomuks--requested-members (make-hash-table :test 'equal)
  "Member profiles already requested from the backend.")
(defvar gomuks--mention-members-loaded (make-hash-table :test 'equal)
  "Rooms whose member list has been requested for mention completion.")
(defvar gomuks--timelines (make-hash-table :test 'equal)
  "Timeline event row IDs keyed by room ID.")
(defvar gomuks--timeline-ids (make-hash-table :test 'equal)
  "Timeline row IDs keyed by event row ID.")
(defvar gomuks--last-read (make-hash-table :test 'equal)
  "Last read receipt event ID sent for each room.")
(defvar gomuks--previous-window-config nil
  "Window configuration saved before opening the home page.")
(defvar gomuks--connection-status "Disconnected"
  "Status text displayed on the home page.")
(defvar gomuks--cache-generation 0
  "Generation of the entire room cache; advanced by a full reset.")
(defvar gomuks--room-generations (make-hash-table :test 'equal)
  "Request generations keyed by room ID; advanced by a room reset or leave.")
(defvar gomuks--media-cache (make-hash-table :test 'equal)
  "Downloaded media paths keyed by attachment identity.")
(defvar gomuks--media-pending (make-hash-table :test 'equal)
  "Callbacks waiting for each in-progress media download.")
(defvar gomuks--media-failures (make-hash-table :test 'equal)
  "Attachments whose automatic preview download failed.")

(defun gomuks--reset-cache ()
  "Clear cached room data. Preserve account identity and pending stream data."
  (setq gomuks--cache-generation (1+ gomuks--cache-generation)
        gomuks--room-generations (make-hash-table :test 'equal)
        gomuks--rooms (make-hash-table :test 'equal)
        gomuks--muted-rooms (make-hash-table :test 'equal)
        gomuks--events (make-hash-table :test 'equal)
        gomuks--event-index (make-hash-table :test 'equal)
        gomuks--thread-index (make-hash-table :test 'equal)
        gomuks--member-state (make-hash-table :test 'equal)
        gomuks--requested-members (make-hash-table :test 'equal)
        gomuks--mention-members-loaded (make-hash-table :test 'equal)
        gomuks--timelines (make-hash-table :test 'equal)
        gomuks--timeline-ids (make-hash-table :test 'equal)
        gomuks--last-read (make-hash-table :test 'equal)))

(defun gomuks--store-events (events)
  "Store EVENTS by row ID, replacing previous event objects.
Skip events without a row ID.  Update the event cache and its lookup indexes."
  (dolist (event events)
    (when-let* ((rowid (gomuks--alist 'rowid event)))
      (when-let* ((old (gethash rowid gomuks--events)))
        (gomuks--index-event old rowid nil))
      (puthash rowid event gomuks--events)
      (gomuks--index-event event rowid t))))

(defun gomuks--index-event (event rowid add)
  "Write EVENT's lookup indexes, adding ROWID when ADD is non-nil."
  (let ((room (gomuks--alist 'room_id event))
        (id (gomuks--alist 'event_id event)))
    (when (and room id)
      (if add (puthash (cons room id) rowid gomuks--event-index)
        (remhash (cons room id) gomuks--event-index)))
    (when (equal (gomuks--alist 'relation_type event) "m.thread")
      (let* ((key (cons room (gomuks--alist 'relates_to event)))
             (rows (gethash key gomuks--thread-index)))
        (when (and add (not rows))
          (setq rows (make-hash-table :test 'equal))
          (puthash key rows gomuks--thread-index))
        (when rows
          (if (and add (not (gomuks--alist 'redacted_by event)))
              (puthash rowid t rows)
            (remhash rowid rows))
          (when (= (hash-table-count rows) 0) (remhash key gomuks--thread-index)))))))

(defun gomuks--event-rows (events)
  "Transform EVENTS into unique identified row IDs, preserving order.
Accept existing row IDs too, so retained views can be refreshed on reload."
  (let ((seen (make-hash-table :test 'equal)) rows)
    (dolist (event events (nreverse rows))
      (let ((row (if (listp event) (gomuks--alist 'rowid event) event)))
        (when (and row (not (gethash row seen)))
          (puthash row t seen) (push row rows))))))

(defun gomuks--merge-thread-rows (root old new)
  "Query the current cache to merge OLD and NEW row IDs for ROOT."
  (gomuks--event-rows
   (gomuks--merge-thread-events root
				(gomuks--events-for-rows (gomuks--event-rows old))
				(gomuks--events-for-rows (gomuks--event-rows new)))))

(defun gomuks--store-timeline (id items &optional prepend)
  "Write unique timeline ITEMS for room ID in one batch.
PREPEND places older rows before current rows. Return the added rows."
  (let* ((old (gethash id gomuks--timelines))
         (seen (make-hash-table :test 'equal)) added)
    (dolist (row old) (puthash row t seen))
    (dolist (item items)
      (let ((row (gomuks--alist 'event_rowid item))
            (timeline (gomuks--alist 'timeline_rowid item)))
        (when (and row timeline)
          (puthash row timeline gomuks--timeline-ids)
          (unless (gethash row seen)
            (puthash row t seen) (push row added)))))
    (setq added (nreverse added))
    (puthash id (if prepend (append added old) (append old added))
             gomuks--timelines)
    added))

(defun gomuks--store-members (id events &optional only-missing)
  "Write identified member EVENTS for ID, optionally ONLY-MISSING profiles."
  (let ((state (or (gethash id gomuks--member-state)
                   (make-hash-table :test 'equal))))
    (dolist (event events)
      (when (and (equal (gomuks--alist 'room_id event) id)
                 (equal (gomuks--alist 'type event) "m.room.member")
                 (gomuks--alist 'rowid event))
        (gomuks--store-events (list event))
        (let ((key (gomuks--alist 'state_key event)))
          (unless (and only-missing (gethash key state))
            (puthash key (gomuks--alist 'rowid event) state)))))
    (puthash id state gomuks--member-state)))

(defun gomuks--forget-room (id)
  "Write a room leave: invalidate ID and remove all of its cached events."
  (gomuks--reset-room-cache id)
  (remhash id gomuks--rooms)
  (remhash id gomuks--member-state)
  (remhash id gomuks--muted-rooms)
  (maphash (lambda (row event)
             (when (equal (gomuks--alist 'room_id event) id)
               (gomuks--index-event event row nil)
               (remhash row gomuks--timeline-ids)
               (remhash row gomuks--events))) gomuks--events))

(defun gomuks--events-for-rows (rowids)
  "Return cached events for ROWIDS in order, omitting missing rows."
  (delq nil
        (mapcar (lambda (rowid)
                  (and rowid (gethash rowid gomuks--events)))
                rowids)))

(defun gomuks--cache-token (room-id)
  "Return the current cache and room generations for ROOM-ID."
  (cons gomuks--cache-generation (gethash room-id gomuks--room-generations 0)))

(defun gomuks--cache-current-p (room-id token)
  "Return non-nil when TOKEN still belongs to ROOM-ID's current cache."
  (equal token (gomuks--cache-token room-id)))

(defun gomuks--reset-room-cache (room-id)
  "Invalidate ROOM-ID's requests and clear its timeline and request markers.
Preserve cached events, room metadata, and member indexes."
  (puthash room-id (1+ (gethash room-id gomuks--room-generations 0))
           gomuks--room-generations)
  (dolist (rowid (gethash room-id gomuks--timelines))
    (remhash rowid gomuks--timeline-ids))
  (remhash room-id gomuks--timelines)
  (remhash room-id gomuks--last-read)
  (remhash room-id gomuks--mention-members-loaded)
  (maphash (lambda (key _value)
             (when (equal (car key) room-id)
               (remhash key gomuks--requested-members)))
           gomuks--requested-members))

(defvar gomuks--view-serial 0 "Monotonic identity for new view buffer lifetimes.")
(defvar-local gomuks--view-generation 0 "Identity of this view buffer lifetime.")
(defvar-local gomuks--navigation-serial 0 "Latest explicit context navigation request.")
(defvar-local gomuks--search-serial 0 "Latest search page request.")

(defvar-local gomuks--room-id nil
  "Room ID associated with the current Gomuks buffer.")
(defvar-local gomuks--reactions-event nil
  "Row ID or Matrix event ID of the message whose reactions this buffer shows.")
(defvar-local gomuks--reactions-key nil
  "Reaction key shown in this buffer.")
(defvar-local gomuks--initial-history-requested nil
  "Non-nil after requesting the first page for this room buffer.")
(defvar-local gomuks--thread-root nil
  "Thread root event ID shown by the current buffer.")
(defvar-local gomuks--thread-events nil
  "Event row IDs shown in the current thread buffer.")
(defvar-local gomuks--thread-next-batch nil
  "Pagination token for older events in the current thread.")
(defvar-local gomuks--context-target nil
  "Reply target event ID shown by the current context buffer.")
(defvar-local gomuks--context-events nil
  "Event row IDs shown around the current reply target.")
(defvar-local gomuks--parent-buffer nil
  "Buffer to return to from a thread or reply context.")
(defvar-local gomuks--compose-relation nil
  "Matrix relation attached to the current draft.")
(defvar-local gomuks--compose-context nil
  "Label describing the current draft's reply or edit target.")
(defvar-local gomuks--compose-sending nil
  "Non-nil while the current draft is being sent.")
(defvar-local gomuks--compose-attachment nil
  "Pending attachments as a list of (FILE TEMPORARY LABEL) entries.")
(defvar-local gomuks--compose-source nil
  "Room buffer associated with the current composer.")
(defvar-local gomuks--preview-source nil
  "Composer buffer associated with this attachment preview.")
(defvar-local gomuks--preview-attachment nil
  "Attachment entry displayed in this preview.")
(defvar-local gomuks--search-query nil
  "Query displayed by the current search buffer.")
(defvar-local gomuks--search-results nil
  "Event row IDs displayed by the current search buffer.")
(defvar-local gomuks--history-state 'idle
  "History workflow phase: idle, loading, exhausted or error.")
(defvar-local gomuks--history-serial 0
  "History request serial; changing it invalidates old completions.")
(defvar-local gomuks--view-dirty nil
  "Non-nil when cached updates have not yet been rendered in this view.")
(defvar-local gomuks--search-next-batch nil
  "Token for the next page of search results.")
(defvar-local gomuks--search-loading nil
  "Non-nil while a search request is pending.")
(defvar-local gomuks--search-error nil
  "Most recent backend search error in this buffer.")
(defvar-local gomuks--search-generation 0
  "Serial number used to ignore stale search responses.")

(defun gomuks--alist (key object)
  "Return the value of KEY in alist OBJECT, comparing keys with `equal'."
  (alist-get key object nil nil #'equal))

(defun gomuks--key-string (key)
  "Return KEY as a string when it is a symbol."
  (if (symbolp key) (symbol-name key) key))

(defun gomuks--room-name (id)
  "Return the display name of room ID, or ID when unnamed."
  (or (gomuks--alist 'name (gethash id gomuks--rooms)) id))

(defun gomuks--event-content (event)
  "Return the decrypted content of EVENT when available."
  (or (gomuks--alist 'decrypted event)
      (gomuks--alist 'content event)))

(defun gomuks--reply-target (event)
  "Return the event ID that EVENT replies to, if any."
  (gomuks--alist
   'event_id
   (gomuks--alist 'm.in_reply_to
                  (gomuks--alist 'm.relates_to
                                 (gomuks--event-content event)))))

(defun gomuks--thread-root-id (event)
  "Return the root ID for EVENT's thread, or EVENT's own ID."
  (if (equal (gomuks--alist 'relation_type event) "m.thread")
      (gomuks--alist 'relates_to event)
    (gomuks--alist 'event_id event)))

(defun gomuks--merge-thread-events (root old new)
  "Merge OLD and NEW thread events, with ROOT first and no duplicates."
  (let ((seen (make-hash-table :test 'equal)) merged)
    (dolist (event (append new old))
      (when-let* ((id (gomuks--alist 'event_id event)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push event merged))))
    (sort merged
          (lambda (a b)
            (let ((a-id (gomuks--alist 'event_id a))
                  (b-id (gomuks--alist 'event_id b)))
              (cond ((equal a-id root) t)
                    ((equal b-id root) nil)
                    (t (< (or (gomuks--alist 'timestamp a) 0)
                          (or (gomuks--alist 'timestamp b) 0)))))))))

(defun gomuks--find-event (room-id event-id)
  "Find EVENT-ID in ROOM-ID among cached events."
  (or (gethash (gethash (cons room-id event-id) gomuks--event-index)
               gomuks--events)
      (catch 'found
	(maphash (lambda (_rowid event)
		   (when (and (equal (gomuks--alist 'room_id event) room-id)
                              (equal (gomuks--alist 'event_id event) event-id))
                     (throw 'found event)))
		 gomuks--events)
	nil)))

(defun gomuks--sender-name (id event)
  "Return EVENT's display name in room ID, falling back to its MXID localpart."
  (let* ((sender (gomuks--alist 'sender event))
         (profile (gomuks--alist 'com.beeper.per_message_profile
                                 (gomuks--event-content event)))
         (state (gethash id gomuks--member-state))
         (rowid (and sender state (gethash sender state)))
         (member (and rowid (gethash rowid gomuks--events)))
         (name (or (gomuks--alist 'displayname profile)
                   (gomuks--alist 'displayname (gomuks--event-content member)))))
    (if (and (stringp name) (not (string-empty-p (string-trim name))))
        (replace-regexp-in-string "[\n\r]+" " " (string-trim name))
      (if (and (stringp sender)
               (string-match "\\`@\\([^:]+\\):" sender))
          (match-string 1 sender)
        (or sender "?")))))



(defun gomuks--store-sync (sync)
  "Write SYNC's domain state and return room IDs whose views need resetting.
Do not touch buffers, issue requests or emit notifications."
  (let ((reset (copy-sequence (gomuks--alist 'left_rooms sync))))
    (when (gomuks--alist 'clear_state sync) (gomuks--reset-cache))
    (when-let* ((rules (gomuks--alist 'm.push_rules
                                      (gomuks--alist 'account_data sync))))
      (clrhash gomuks--muted-rooms)
      (dolist (rule (gomuks--alist 'room
                                   (gomuks--alist 'global
                                                  (gomuks--alist 'content rules))))
        (when (and (gomuks--alist 'enabled rule)
                   (not (member "notify" (gomuks--alist 'actions rule))))
          (puthash (gomuks--alist 'rule_id rule) t gomuks--muted-rooms))))
    (dolist (id (gomuks--alist 'left_rooms sync)) (gomuks--forget-room id))
    (dolist (entry (gomuks--alist 'rooms sync))
      (let* ((id (gomuks--key-string (car entry)))
             (room (cdr entry)) (meta (gomuks--alist 'meta room)))
        (when meta
          (when (and (gomuks--alist 'marked_unread meta)
                     (not (gomuks--alist 'marked_unread (gethash id gomuks--rooms))))
            (remhash id gomuks--last-read))
          (puthash id meta gomuks--rooms))
        (gomuks--store-events (gomuks--alist 'events room))
        (when-let* ((members (gomuks--alist 'm.room.member
                                            (gomuks--alist 'state room))))
          (let ((state (or (gethash id gomuks--member-state)
                           (make-hash-table :test 'equal))))
            (dolist (member members)
              (when (cdr member)
                (puthash (gomuks--key-string (car member)) (cdr member) state)))
            (puthash id state gomuks--member-state)))
        (when (gomuks--alist 'reset room)
          (gomuks--reset-room-cache id) (push id reset))
        (gomuks--store-timeline id (gomuks--alist 'timeline room))))
    reset))

(defun gomuks--store-decryption (data)
  "Write decrypted events and any supplied room preview metadata from DATA."
  (let ((id (gomuks--alist 'room_id data)))
    (gomuks--store-events (gomuks--alist 'events data))
    (when (assq 'preview_event_rowid data)
      (let ((meta (copy-tree (gethash id gomuks--rooms))))
        (setf (alist-get 'preview_event_rowid meta)
              (gomuks--alist 'preview_event_rowid data))
        (puthash id meta gomuks--rooms)))))

(provide 'gomuks-core)
;;; gomuks-core.el ends here
