;;; gomuks-core.el --- Shared state and cache -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; User options, the room cache and helpers for reading backend events.
;; The cache functions here only update tables: they never touch buffers or
;; start requests.  Modules below the views report changes with
;; `gomuks--announce' instead of calling the views directly.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

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

;;;; Change notifications

(defvar gomuks--change-functions nil
  "Functions called with (EVENT &optional DATA EXTRA) after state changes.
EVENT is one of `status', `sync', `room', `media', `decrypted',
`send-complete' or `account'.  `gomuks.el' installs the handler that
routes each one to the views, sends and sticker pickers.")

(defun gomuks--announce (event &optional data extra)
  "Report EVENT with DATA and EXTRA to `gomuks--change-functions'."
  (run-hook-with-args 'gomuks--change-functions event data extra))

;;;; Session state

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
(defvar gomuks--hidden-room-ids nil
  "Room IDs hidden from the home page, normal switcher, and notifications.")
(defvar gomuks--previous-window-config nil
  "Window configuration saved before opening the home page.")
(defvar gomuks--connection-status "Disconnected"
  "Status text displayed on the home page.")
(defvar gomuks--cache-generation 0
  "Generation of the entire room cache; advanced by a full reset.")

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

;;;; Room cache

(defvar gomuks--cache-tables nil
  "Alist of (TABLE . LIFETIME) for every room cache table.
A full reset empties all of them.  When a room is left, its keys are also
dropped from `room' and `timeline' tables; when a room's timeline resets,
only from `timeline' tables.  A key belongs to a room when it is the room
ID or a cons whose car is the room ID.")

(defmacro gomuks--defcache (name lifetime doc)
  "Define NAME as a room cache table with LIFETIME, documented by DOC."
  (declare (indent 2) (doc-string 3))
  `(progn (defvar ,name (make-hash-table :test 'equal) ,doc)
          (setf (alist-get ',name gomuks--cache-tables) ',lifetime)))

(gomuks--defcache gomuks--rooms room
  "Room metadata keyed by room ID.")
(gomuks--defcache gomuks--muted-rooms room
  "Room IDs muted by the current account's push rules.")
(gomuks--defcache gomuks--member-state room
  "Member event row IDs keyed by room ID and sender ID.")
(gomuks--defcache gomuks--image-pack-state room
  "Image pack event rows per room, keyed by (TYPE . STATE-KEY).")
(gomuks--defcache gomuks--timelines timeline
  "Timeline event row IDs keyed by room ID.")
(gomuks--defcache gomuks--last-read timeline
  "Last read receipt event ID sent for each room.")
(gomuks--defcache gomuks--receipts timeline
  "Latest public receipts per room, keyed by (USER-ID THREAD-ID).")
(gomuks--defcache gomuks--requested-receipts timeline
  "Receipt lookups already requested, keyed by (ROOM-ID . EVENT-ID).")
(gomuks--defcache gomuks--requested-members timeline
  "Member profiles already requested, keyed by (ROOM-ID . USER-ID).")
(gomuks--defcache gomuks--mention-members-loaded timeline
  "Rooms whose member list has been requested for mention completion.")
(gomuks--defcache gomuks--events session
  "Cached events keyed by backend row ID.")
(gomuks--defcache gomuks--event-index session
  "Event rows indexed by (ROOM-ID . EVENT-ID).")
(gomuks--defcache gomuks--thread-index session
  "Thread reply row sets indexed by (ROOM-ID . ROOT-ID).")
(gomuks--defcache gomuks--timeline-ids session
  "Timeline row IDs keyed by event row ID.")
(gomuks--defcache gomuks--account-data session
  "Global account data events keyed by type.")
(gomuks--defcache gomuks--room-generations session
  "Request generations keyed by room ID; advanced by a room reset or leave.")

(defun gomuks--reset-cache ()
  "Empty every cache table and invalidate requests started before now.
Account identity and buffered stream data are kept."
  (cl-incf gomuks--cache-generation)
  (dolist (entry gomuks--cache-tables)
    (set (car entry) (make-hash-table :test 'equal))))

(defun gomuks--drop-room-keys (id lifetimes)
  "Remove room ID's keys from cache tables whose lifetime is in LIFETIMES."
  (dolist (entry gomuks--cache-tables)
    (when (memq (cdr entry) lifetimes)
      (let ((table (symbol-value (car entry))))
        (maphash (lambda (key _value)
                   (when (equal (if (consp key) (car key) key) id)
                     (remhash key table)))
                 table)))))

(defun gomuks--reset-room-cache (room-id)
  "Invalidate ROOM-ID's requests and forget its timeline and request markers.
Cached events, room metadata and members are kept."
  (puthash room-id (1+ (gethash room-id gomuks--room-generations 0))
           gomuks--room-generations)
  (dolist (rowid (gethash room-id gomuks--timelines))
    (remhash rowid gomuks--timeline-ids))
  (gomuks--drop-room-keys room-id '(timeline)))

(defun gomuks--forget-room (id)
  "Forget everything cached for room ID after leaving it."
  (gomuks--reset-room-cache id)
  (gomuks--drop-room-keys id '(room))
  (maphash (lambda (row event)
             (when (equal (gomuks--alist 'room_id event) id)
               (gomuks--index-event event row nil)
               (remhash row gomuks--timeline-ids)
               (remhash row gomuks--events))) gomuks--events))

(defun gomuks--cache-token (room-id)
  "Return the current cache and room generations for ROOM-ID."
  (cons gomuks--cache-generation (gethash room-id gomuks--room-generations 0)))

(defun gomuks--cache-current-p (room-id token)
  "Return non-nil when TOKEN still belongs to ROOM-ID's current cache."
  (equal token (gomuks--cache-token room-id)))

(defun gomuks--store-events (events)
  "Store EVENTS by row ID, replacing older copies and updating the indexes.
Events without a row ID are skipped."
  (dolist (event events)
    (when-let* ((rowid (gomuks--alist 'rowid event)))
      (when-let* ((old (gethash rowid gomuks--events)))
        (gomuks--index-event old rowid nil))
      (puthash rowid event gomuks--events)
      (gomuks--index-event event rowid t))))

(defun gomuks--index-event (event rowid add)
  "Add ROWID to EVENT's lookup indexes, or remove it when ADD is nil."
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
  "Return the unique row IDs of EVENTS, keeping their order.
EVENTS may also contain row IDs, so retained views can be refreshed."
  (let ((seen (make-hash-table :test 'equal)) rows)
    (dolist (event events (nreverse rows))
      (let ((row (if (listp event) (gomuks--alist 'rowid event) event)))
        (when (and row (not (gethash row seen)))
          (puthash row t seen) (push row rows))))))

(defun gomuks--events-for-rows (rowids)
  "Return cached events for ROWIDS in order, omitting missing rows."
  (delq nil (mapcar (lambda (rowid) (and rowid (gethash rowid gomuks--events)))
                    rowids)))

(defun gomuks--merge-thread-rows (root old new)
  "Merge OLD and NEW row IDs of the thread under ROOT."
  (gomuks--event-rows
   (gomuks--merge-thread-events root
                                (gomuks--events-for-rows (gomuks--event-rows old))
                                (gomuks--events-for-rows (gomuks--event-rows new)))))

(defun gomuks--store-timeline (id items &optional prepend)
  "Add the new rows in timeline ITEMS to room ID and return them.
PREPEND places them before the rows already known."
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
  "Store member EVENTS for room ID; with ONLY-MISSING, keep known profiles."
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

(defun gomuks--store-receipts (id receipts)
  "Merge public RECEIPTS for room ID without moving readers backwards.
RECEIPTS maps event IDs to receipt lists.  Prefer timeline order when both
positions are cached; otherwise compare receipt timestamps."
  (let ((state (or (gethash id gomuks--receipts)
                   (make-hash-table :test 'equal))))
    (dolist (entry receipts)
      (let ((event-id (gomuks--key-string (car entry))))
        (dolist (receipt (cdr entry))
          (let* ((user (gomuks--alist 'user_id receipt))
                 (thread (gomuks--alist 'thread_id receipt))
                 (key (list user (unless (equal thread "") thread)))
                 (old (gethash key state))
                 (old-event (and old (gomuks--find-event
                                      id (gomuks--alist 'event_id old))))
                 (new-event (gomuks--find-event id event-id))
                 (old-row (or (gomuks--alist 'timeline_rowid old-event)
                              (gethash (gomuks--alist 'rowid old-event)
                                       gomuks--timeline-ids)))
                 (new-row (or (gomuks--alist 'timeline_rowid new-event)
                              (gethash (gomuks--alist 'rowid new-event)
                                       gomuks--timeline-ids))))
            (when (and (stringp user)
                       (equal (gomuks--alist 'receipt_type receipt) "m.read")
                       (or (not old)
                           (if (and (numberp old-row) (numberp new-row))
                               (>= new-row old-row)
                             (>= (or (gomuks--alist 'timestamp receipt) 0)
                                 (or (gomuks--alist 'timestamp old) 0)))))
              (puthash key (cons (cons 'event_id event-id)
                                 (assq-delete-all 'event_id (copy-tree receipt)))
                       state))))))
    (puthash id state gomuks--receipts)))

(defun gomuks--store-image-packs (events &optional only-missing)
  "Store image pack state EVENTS; with ONLY-MISSING, keep known packs.
Use this for room state responses, never for historical timeline events."
  (dolist (event events)
    (let ((id (gomuks--alist 'room_id event))
          (type (gomuks--alist 'type event))
          (row (gomuks--alist 'rowid event)))
      (when (and id row (member type '("m.room.image_pack" "im.ponies.room_emotes")))
        (let* ((state (or (gethash id gomuks--image-pack-state)
                          (make-hash-table :test 'equal)))
               (key (cons type (or (gomuks--alist 'state_key event) ""))))
          (unless (and only-missing (gethash key state))
            (gomuks--store-events (list event))
            (puthash key row state))
          (puthash id state gomuks--image-pack-state))))))

(defun gomuks--store-sync (sync)
  "Store SYNC in the cache and return the IDs of rooms whose views must reset."
  (let ((reset (copy-sequence (gomuks--alist 'left_rooms sync))))
    (when (gomuks--alist 'clear_state sync) (gomuks--reset-cache))
    (dolist (entry (gomuks--alist 'account_data sync))
      (puthash (gomuks--key-string (car entry)) (cdr entry) gomuks--account-data))
    (when-let* ((rules (gomuks--dig sync 'account_data 'm.push_rules)))
      (clrhash gomuks--muted-rooms)
      (dolist (rule (gomuks--dig rules 'content 'global 'room))
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
        (when-let* ((members (gomuks--dig room 'state 'm.room.member)))
          (let ((state (or (gethash id gomuks--member-state)
                           (make-hash-table :test 'equal))))
            (dolist (member members)
              (when (cdr member)
                (puthash (gomuks--key-string (car member)) (cdr member) state)))
            (puthash id state gomuks--member-state)))
        (let ((state (or (gethash id gomuks--image-pack-state)
                         (make-hash-table :test 'equal))))
          (dolist (type '("m.room.image_pack" "im.ponies.room_emotes"))
            (dolist (pack (gomuks--dig room 'state (intern type)))
              (let ((key (cons type (gomuks--key-string (car pack)))))
                (if (cdr pack) (puthash key (cdr pack) state)
                  (remhash key state)))))
          (puthash id state gomuks--image-pack-state))
        (when (gomuks--alist 'reset room)
          (gomuks--reset-room-cache id) (push id reset))
        (gomuks--store-timeline id (gomuks--alist 'timeline room))
        (gomuks--store-receipts id (gomuks--alist 'receipts room))))
    reset))

(defun gomuks--store-decryption (data)
  "Store decrypted events and any room preview metadata from DATA."
  (let ((id (gomuks--alist 'room_id data)))
    (gomuks--store-events (gomuks--alist 'events data))
    (when (assq 'preview_event_rowid data)
      (let ((meta (copy-tree (gethash id gomuks--rooms))))
        (setf (alist-get 'preview_event_rowid meta)
              (gomuks--alist 'preview_event_rowid data))
        (puthash id meta gomuks--rooms)))))

;;;; View buffer state

;; Shared by the room, thread, context, search and reaction views, so that
;; resets and view identities can see all of it.
(defvar gomuks--view-serial 0 "Monotonic identity for new view buffer lifetimes.")
(defvar-local gomuks--view-generation 0 "Identity of this view buffer lifetime.")
(defvar-local gomuks--navigation-serial 0 "Latest explicit context navigation request.")
(defvar-local gomuks--view-dirty nil
  "Non-nil when cached updates have not yet been rendered in this view.")
(defvar-local gomuks--room-id nil
  "Room ID associated with the current Gomuks buffer.")
(defvar-local gomuks--parent-buffer nil
  "Buffer to return to from a thread, reply context or search.")
(defvar-local gomuks--initial-history-requested nil
  "Non-nil after requesting the first page for this room buffer.")
(defvar-local gomuks--history-state 'idle
  "History loading state: idle, loading, exhausted or error.")
(defvar-local gomuks--history-serial 0
  "History request serial; changing it invalidates old completions.")
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
(defvar-local gomuks--search-serial 0 "Latest search page request.")
(defvar-local gomuks--search-query nil
  "Query displayed by the current search buffer.")
(defvar-local gomuks--search-results nil
  "Event row IDs displayed by the current search buffer.")
(defvar-local gomuks--search-next-batch nil
  "Token for the next page of search results.")
(defvar-local gomuks--search-loading nil
  "Non-nil while a search request is pending.")
(defvar-local gomuks--search-error nil
  "Most recent backend search error in this buffer.")
(defvar-local gomuks--reactions-event nil
  "Row ID or Matrix event ID of the message whose reactions this buffer shows.")
(defvar-local gomuks--reactions-key nil
  "Reaction key shown in this buffer.")

;; Composer state, read by the send records that update their draft.
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

;;;; Reading events

(defun gomuks--alist (key object)
  "Return the value of KEY in alist OBJECT, comparing keys with `equal'."
  (alist-get key object nil nil #'equal))

(defun gomuks--dig (object &rest keys)
  "Follow KEYS through nested alists starting at OBJECT."
  (dolist (key keys object)
    (setq object (gomuks--alist key object))))

(defun gomuks--key-string (key)
  "Return KEY as a string when it is a symbol."
  (if (symbolp key) (symbol-name key) key))

(defun gomuks--one-line (text)
  "Return TEXT with line breaks replaced by spaces."
  (replace-regexp-in-string "[\n\r]+" " " text))

(defun gomuks--format-time (format timestamp)
  "Format millisecond TIMESTAMP with `format-time-string' FORMAT."
  (format-time-string format (/ timestamp 1000.0)))

(defun gomuks--next-token (response key previous)
  "Return RESPONSE's pagination token at KEY, unless empty or equal to PREVIOUS."
  (let ((next (gomuks--alist key response)))
    (and (stringp next) (not (string-empty-p next)) (not (equal next previous)) next)))

(defun gomuks--room-name (id)
  "Return the display name of room ID, or ID when unnamed."
  (or (gomuks--alist 'name (gethash id gomuks--rooms)) id))

(defun gomuks--rooms-by-recency (predicate)
  "Return cached (ID . META) rooms satisfying PREDICATE, most recent first."
  (let (rooms)
    (maphash (lambda (id meta) (when (funcall predicate id) (push (cons id meta) rooms)))
             gomuks--rooms)
    (sort rooms (lambda (a b)
                  (> (or (gomuks--alist 'sorting_timestamp (cdr a)) 0)
                     (or (gomuks--alist 'sorting_timestamp (cdr b)) 0))))))

(defun gomuks--event-content (event)
  "Return the decrypted content of EVENT when available."
  (or (gomuks--alist 'decrypted event)
      (gomuks--alist 'content event)))

(defun gomuks--event-type (event)
  "Return EVENT's decrypted type, or its outer type."
  (or (gomuks--alist 'decrypted_type event) (gomuks--alist 'type event)))

(defun gomuks--reply-target (event)
  "Return the event ID that EVENT replies to, if any."
  (gomuks--dig (gomuks--event-content event) 'm.relates_to 'm.in_reply_to 'event_id))

(defun gomuks--thread-root-id (event)
  "Return the root ID for EVENT's thread, or EVENT's own ID."
  (if (equal (gomuks--alist 'relation_type event) "m.thread")
      (gomuks--alist 'relates_to event)
    (gomuks--alist 'event_id event)))

(defun gomuks--thread-relation ()
  "Return the Matrix relation that places a message in this buffer's thread."
  (when gomuks--thread-root
    `((rel_type . "m.thread") (event_id . ,gomuks--thread-root))))

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

(defun gomuks--event-at-point ()
  "Return the message event at point, or signal a user error."
  (or (unless (get-text-property (point) 'gomuks-date)
        (let ((before (max (point-min) (1- (point)))))
          (or (gethash (or (get-text-property (point) 'gomuks-event-rowid)
                           (get-text-property before 'gomuks-event-rowid))
                       gomuks--events)
              (get-text-property (point) 'gomuks-event)
              (get-text-property before 'gomuks-event))))
      (user-error "No message on this line")))

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
        (gomuks--one-line (string-trim name))
      (if (and (stringp sender)
               (string-match "\\`@\\([^:]+\\):" sender))
          (match-string 1 sender)
        (or sender "?")))))

(provide 'gomuks-core)
;;; gomuks-core.el ends here
