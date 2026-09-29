;;; gomuks.el --- Matrix client for the gomuks backend -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Soliprem
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Connect to an already logged-in gomuks backend.  See README.md.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'browse-url)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(declare-function notifications-notify "notifications")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-insert-state "evil-commands")
(declare-function emoji--init "emoji")
(declare-function emoji--read-emoji "emoji")
(declare-function empv-play "empv" (uri))
(declare-function empv-toggle "empv" ())
(declare-function empv-seek "empv" (target &optional type))
(declare-function emms-play-file "emms-source-file" (file))
(declare-function emms-pause "emms" ())
(declare-function emms-seek "emms" (duration))

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
(defcustom gomuks-audio-backend 'empv
  "Use EMPV or EMMS to play audio attachments."
  :type '(choice (const empv) (const emms)) :group 'gomuks)

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
(defvar gomuks--events (make-hash-table :test 'equal)
  "Cached events keyed by backend row ID.")
(defvar gomuks--member-state (make-hash-table :test 'equal)
  "Member event row IDs keyed by room ID and sender ID.")
(defvar gomuks--requested-members (make-hash-table :test 'equal)
  "Member profiles already requested from the backend.")
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
(defvar gomuks--media-cache (make-hash-table :test 'equal)
  "Downloaded media paths keyed by attachment identity.")
(defvar gomuks--media-pending (make-hash-table :test 'equal)
  "Callbacks waiting for each in-progress media download.")
(defvar gomuks--media-failures (make-hash-table :test 'equal)
  "Attachments whose automatic preview download failed.")
(defvar-local gomuks--room-id nil
  "Room ID associated with the current Gomuks buffer.")
(defvar-local gomuks--reactions-event nil
  "Message event whose reactions this buffer shows.")
(defvar-local gomuks--reactions-key nil
  "Reaction key shown in this buffer.")
(defvar-local gomuks--initial-history-requested nil
  "Non-nil after requesting the first page for this room buffer.")
(defvar-local gomuks--thread-root nil
  "Thread root event ID shown by the current buffer.")
(defvar-local gomuks--thread-events nil
  "Events shown in the current thread buffer.")
(defvar-local gomuks--thread-next-batch nil
  "Pagination token for older events in the current thread.")
(defvar-local gomuks--context-target nil
  "Reply target event ID shown by the current context buffer.")
(defvar-local gomuks--context-events nil
  "Events shown around the current reply target.")
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
  "Events displayed by the current search buffer.")
(defvar-local gomuks--search-next-batch nil
  "Token for the next page of search results.")
(defvar-local gomuks--search-loading nil
  "Non-nil while a search request is pending.")
(defvar-local gomuks--search-error nil
  "Most recent backend search error in this buffer.")
(defvar-local gomuks--search-generation 0
  "Serial number used to ignore stale search responses.")

(defvar gomuks-rooms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gomuks-open-room)
    (define-key map (kbd "g") #'gomuks-reconnect)
    (define-key map (kbd "q") #'gomuks-quit)
    map)
  "Keymap for the Gomuks home page.")
(defvar gomuks-room-mode-map (make-sparse-keymap)
  "Keymap for room, thread, and reply context buffers.")
;; Bind after `defvar' so reloading updates the existing map in a daemon.
(define-key gomuks-room-mode-map (kbd "C-c C-s") #'gomuks-compose)
(define-key gomuks-room-mode-map (kbd "C-c C-a") #'gomuks-send-file)
(define-key gomuks-room-mode-map (kbd "C-c C-p") #'gomuks-send-sticker)
(define-key gomuks-room-mode-map (kbd "C-c C-g") #'gomuks-send-gif)
(define-key gomuks-room-mode-map (kbd "C-c C-o") #'gomuks-open-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-w") #'gomuks-save-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-SPC") #'gomuks-audio-toggle)
(define-key gomuks-room-mode-map (kbd "C-c <") #'gomuks-audio-backward)
(define-key gomuks-room-mode-map (kbd "C-c >") #'gomuks-audio-forward)
(define-key gomuks-room-mode-map (kbd "RET") #'gomuks-activate-at-point)
(define-key gomuks-room-mode-map (kbd "o") #'gomuks-open-attachment)
(define-key gomuks-room-mode-map (kbd "d") nil)
(define-key gomuks-room-mode-map (kbd "d d") #'gomuks-redact)
(define-key gomuks-room-mode-map (kbd "D") #'gomuks-save-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-r") #'gomuks-reply)
(define-key gomuks-room-mode-map (kbd "C-c C-e") #'gomuks-edit)
(define-key gomuks-room-mode-map (kbd "C-c C-+") #'gomuks-react)
(define-key gomuks-room-mode-map (kbd "C-c C-d") #'gomuks-redact)
(define-key gomuks-room-mode-map (kbd "C-c C-m") #'gomuks-mark-read)
(define-key gomuks-room-mode-map (kbd "C-c C-u") #'gomuks-copy-sender-id)
(define-key gomuks-room-mode-map (kbd "M-p") #'gomuks-load-history)
(define-key gomuks-room-mode-map (kbd "C-c C-t") #'gomuks-open-thread)
(define-key gomuks-room-mode-map (kbd "C-c C-j") #'gomuks-follow-reply)
(define-key gomuks-room-mode-map (kbd "C-c C-f") #'gomuks-search)
(define-key gomuks-room-mode-map (kbd "g") #'gomuks-reconnect)
(define-key gomuks-room-mode-map (kbd "b") #'gomuks-back)
(define-key gomuks-room-mode-map (kbd "q") #'gomuks-back)
(defvar gomuks-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gomuks-compose-send)
    (define-key map (kbd "C-c C-k") #'gomuks-compose-leave)
    (define-key map (kbd "C-c C-a") #'gomuks-send-file)
    (define-key map (kbd "C-c C-d") #'gomuks-compose-remove-attachment)
    (define-key map (kbd "C-c C-o") #'gomuks-compose-preview-attachment)
    map)
  "Keymap for Gomuks message composer buffers.")
(define-key gomuks-compose-mode-map (kbd "C-c C-p") #'gomuks-send-sticker)
(define-key gomuks-compose-mode-map (kbd "C-c C-g") #'gomuks-send-gif)
(define-key gomuks-compose-mode-map (kbd "C-c C-e") #'gomuks-insert-emoji)
(define-key gomuks-compose-mode-map (kbd "C-c C-v") #'gomuks-paste-image)
(define-key gomuks-compose-mode-map (kbd "C-c C-d") #'gomuks-compose-remove-attachment)
(define-key gomuks-compose-mode-map (kbd "C-c C-o") #'gomuks-compose-preview-attachment)

;; Doom's `map!' uses the user's configured localleader and its insert-state
;; alternate.  Keep this optional so the package also loads in plain Emacs.
(when (fboundp 'map!)
  (eval
   '(map! :map gomuks-room-mode-map :localleader
          "s" #'gomuks-compose
          "a" #'gomuks-send-file
          "p" #'gomuks-send-sticker
          "g" #'gomuks-send-gif
          "o" #'gomuks-open-attachment
          "SPC" #'gomuks-audio-toggle
          "<" #'gomuks-audio-backward
          ">" #'gomuks-audio-forward
          "w" #'gomuks-save-attachment
          "r" #'gomuks-reply
          "e" #'gomuks-edit
          "+" #'gomuks-react
          "d" #'gomuks-redact
          "m" #'gomuks-mark-read
          "u" #'gomuks-copy-sender-id
          "t" #'gomuks-open-thread
          "j" #'gomuks-follow-reply
          "f" #'gomuks-search
          :map gomuks-compose-mode-map :localleader
          "c" #'gomuks-compose-send
          "k" #'gomuks-compose-leave
          "a" #'gomuks-send-file
          "d" #'gomuks-compose-remove-attachment
          "o" #'gomuks-compose-preview-attachment
          "p" #'gomuks-send-sticker
          "g" #'gomuks-send-gif
          "e" #'gomuks-insert-emoji
          "v" #'gomuks-paste-image)))
(defvar gomuks-search-mode-map (make-sparse-keymap)
  "Keymap for Gomuks search results.")
(define-key gomuks-search-mode-map (kbd "RET") #'gomuks-search-open)
(define-key gomuks-search-mode-map (kbd "n") #'gomuks-search-more)
(define-key gomuks-search-mode-map (kbd "q") #'gomuks-search-back)
(define-key gomuks-search-mode-map (kbd "b") #'gomuks-search-back)
(defvar gomuks-reactions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'gomuks-remove-reaction)
    (define-key map (kbd "q") #'gomuks-reactions-quit)
    (define-key map (kbd "b") #'gomuks-reactions-quit)
    map)
  "Keymap for the list of people who reacted to a message.")
(defvar gomuks-attachment-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gomuks-attachment-preview-open)
    (define-key map (kbd "d") #'gomuks-attachment-preview-remove)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for a staged attachment preview.")

(define-derived-mode gomuks-rooms-mode special-mode "Gomuks rooms"
  "Mode for the gomuks room list.")
(define-derived-mode gomuks-room-mode special-mode "Gomuks room"
  "Mode for a gomuks room timeline."
  (add-hook 'post-command-hook #'gomuks--maybe-mark-read nil t))
(define-derived-mode gomuks-compose-mode text-mode "Gomuks compose"
  "Mode for editing a Matrix message draft."
  (setq-local header-line-format
              " Compose a message  •  C-c C-c send  •  C-c C-a attach  •  C-c C-k return")
  (add-hook 'kill-buffer-hook #'gomuks--cleanup-compose-attachment nil t)
  (add-hook 'post-command-hook #'gomuks--maybe-mark-read nil t)
  (visual-line-mode 1))
(define-derived-mode gomuks-search-mode special-mode "Gomuks search"
  "Mode for paginated Gomuks message search results.")
(define-derived-mode gomuks-reactions-mode special-mode "Gomuks reactions"
  "Mode for showing the people who used a reaction.")
(define-derived-mode gomuks-attachment-preview-mode special-mode "Gomuks attachment"
  "Mode for previewing an attachment staged in a draft.")

;; Evil's normal-state maps take precedence over the ordinary major-mode map.
;; Keep Vim motion keys intact; use keys that would edit a read-only buffer.
(with-eval-after-load 'evil
  (evil-set-initial-state 'gomuks-rooms-mode 'normal)
  (evil-set-initial-state 'gomuks-room-mode 'normal)
  (evil-set-initial-state 'gomuks-compose-mode 'insert)
  (evil-set-initial-state 'gomuks-search-mode 'normal)
  (evil-set-initial-state 'gomuks-reactions-mode 'normal)
  (evil-set-initial-state 'gomuks-attachment-preview-mode 'normal)
  (evil-define-key* 'normal gomuks-rooms-mode-map
    (kbd "RET") #'gomuks-open-room
    (kbd "o") #'gomuks-open-room
    (kbd "q") #'gomuks-quit
    (kbd "g r") #'gomuks-reconnect)
  (evil-define-key* 'normal gomuks-room-mode-map
    (kbd "i") #'gomuks-compose
    (kbd "a") #'gomuks-send-file
    (kbd "r") #'gomuks-reply
    (kbd "E") #'gomuks-edit
    (kbd "R") #'gomuks-react
    (kbd "x") #'gomuks-redact
    (kbd "M") #'gomuks-mark-read
    (kbd "U") #'gomuks-copy-sender-id
    (kbd "p") #'gomuks-load-history
    (kbd "T") #'gomuks-open-thread
    (kbd "J") #'gomuks-follow-reply
    (kbd "C-c C-f") #'gomuks-search
    (kbd "C-c C-p") #'gomuks-send-sticker
    (kbd "C-c C-g") #'gomuks-send-gif
    (kbd "o") #'gomuks-open-attachment
    (kbd "d") nil
    (kbd "d d") #'gomuks-redact
    (kbd "D") #'gomuks-save-attachment
    (kbd "RET") #'gomuks-activate-at-point
    (kbd "b") #'gomuks-back
    (kbd "q") #'gomuks-back
    (kbd "g r") #'gomuks-reconnect)
  (evil-define-key* '(normal insert) gomuks-compose-mode-map
    (kbd "C-c C-c") #'gomuks-compose-send
    (kbd "C-c C-k") #'gomuks-compose-leave
    (kbd "C-c C-a") #'gomuks-send-file
    (kbd "C-c C-d") #'gomuks-compose-remove-attachment)
  (evil-define-key* '(normal insert) gomuks-compose-mode-map
    (kbd "C-c C-p") #'gomuks-send-sticker
    (kbd "C-c C-g") #'gomuks-send-gif
    (kbd "C-c C-e") #'gomuks-insert-emoji
    (kbd "C-c C-v") #'gomuks-paste-image
    (kbd "C-c C-o") #'gomuks-compose-preview-attachment)
  (evil-define-key* 'normal gomuks-compose-mode-map
    (kbd "q") #'gomuks-compose-leave)
  (evil-define-key* '(normal motion) gomuks-search-mode-map
    (kbd "RET") #'gomuks-search-open
    (kbd "n") #'gomuks-search-more
    (kbd "q") #'gomuks-search-back
    (kbd "b") #'gomuks-search-back)
  (evil-define-key* '(normal motion) gomuks-reactions-mode-map
    (kbd "d") #'gomuks-remove-reaction
    (kbd "q") #'gomuks-reactions-quit
    (kbd "b") #'gomuks-reactions-quit)
  (evil-define-key* '(normal motion) gomuks-attachment-preview-mode-map
    (kbd "RET") #'gomuks-attachment-preview-open
    (kbd "d") #'gomuks-attachment-preview-remove
    (kbd "q") #'quit-window))

(defun gomuks--url (path)
  "Return the backend URL for relative API PATH."
  (concat (replace-regexp-in-string "/+$" "" gomuks-backend-url)
          "/_gomuks/" path))

(defun gomuks--authorization ()
  "Return the configured backend Basic authorization header, if any."
  (when (and gomuks-username gomuks--password)
    (concat "Basic "
            (base64-encode-string
             (encode-coding-string (concat gomuks-username ":" gomuks--password)
                                   'utf-8) t))))

(defun gomuks--auth-source-credentials ()
  "Return backend credentials stored for `gomuks-backend-url', if any."
  (let* ((url (url-generic-parse-url gomuks-backend-url))
         (host (url-host url))
         (port (or (url-port url) (if (equal (url-type url) "https") 443 80)))
         (entry (and host
                     (car (apply #'auth-source-search
                                 (append (list :host host :port (format "%s" port)
                                               :max 1 :require '(:user :secret))
                                         (when gomuks-username
                                           (list :user gomuks-username)))))))
         (secret (plist-get entry :secret)))
    (when entry
      (cons (plist-get entry :user)
            (if (functionp secret) (funcall secret) secret)))))

(defun gomuks--reset ()
  "Clear cached room, event, and timeline state."
  (setq gomuks--pending ""
        gomuks--user-id nil
        gomuks--rooms (make-hash-table :test 'equal)
        gomuks--events (make-hash-table :test 'equal)
        gomuks--member-state (make-hash-table :test 'equal)
        gomuks--requested-members (make-hash-table :test 'equal)
        gomuks--timelines (make-hash-table :test 'equal)
        gomuks--timeline-ids (make-hash-table :test 'equal)
        gomuks--last-read (make-hash-table :test 'equal))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'gomuks-room-mode)
        (setq gomuks--initial-history-requested nil)))))

(defun gomuks--start-stream ()
  "Start the backend event stream and cancel any scheduled reconnect."
  (when gomuks--reconnect-timer
    (cancel-timer gomuks--reconnect-timer)
    (setq gomuks--reconnect-timer nil))
  (unless (executable-find "curl")
    (user-error "Gomuks.el requires curl for the live event stream"))
  (setq gomuks--connection-status "Connecting…")
  (gomuks--render-rooms)
  (setq gomuks--pending "")
  (let* ((process (make-process
                   :name "gomuks-stream" :buffer nil :connection-type 'pipe
                   :command (list "curl" "--no-buffer" "--silent" "--show-error"
                                  "--fail" "--config" "-" (gomuks--url "sse"))
                   :filter #'gomuks--stream-filter
                   :sentinel #'gomuks--stream-sentinel))
         (authorization (gomuks--authorization)))
    (setq gomuks--stream process)
    ;; Send the credential through stdin; it does not appear in the process command line.
    (process-send-string
     process
     (concat "header = \"Accept: application/jsonl\"\n"
             (when authorization
               (format "header = \"Authorization: %s\"\n" authorization))))
    (process-send-eof process)))

(defun gomuks--stream-filter (_process chunk)
  "Parse complete JSON lines from event stream CHUNK."
  (setq gomuks--pending (concat gomuks--pending chunk))
  (while (string-match "\n" gomuks--pending)
    (let ((line (substring gomuks--pending 0 (match-beginning 0))))
      (setq gomuks--pending (substring gomuks--pending (match-end 0)))
      (unless (or (string-empty-p line) (string= line "null"))
        (condition-case err
            (gomuks--handle-event (json-parse-string line :object-type 'alist
                                                    :array-type 'list
                                                    :false-object nil
                                                    :null-object nil))
          (error (message "gomuks: invalid stream event: %s" err)))))))

(defun gomuks--stream-sentinel (process event)
  "Schedule reconnection when stream PROCESS ends with EVENT."
  (when (eq process gomuks--stream)
    (setq gomuks--stream nil)
    (setq gomuks--connection-status "Disconnected")
    (gomuks--render-rooms)
    (message "gomuks: stream %s" (string-trim event))
    (setq gomuks--reconnect-timer
          (run-at-time gomuks-reconnect-delay nil #'gomuks--start-stream))))

(defun gomuks--alist (key object)
  "Return the value of KEY in alist OBJECT, comparing keys with `equal'."
  (alist-get key object nil nil #'equal))

(defun gomuks--key-string (key)
  "Return KEY as a string when it is a symbol."
  (if (symbolp key) (symbol-name key) key))

(defun gomuks--handle-event (event)
  "Apply one backend stream EVENT to the client state."
  (let ((command (gomuks--alist 'command event))
        (data (gomuks--alist 'data event)))
    (pcase command
      ("sync_complete" (gomuks--apply-sync data))
      ("run_id"
       (setq gomuks--connection-status "Connected")
       (gomuks--render-rooms))
      ("events_decrypted"
       (dolist (item (gomuks--alist 'events data))
         (puthash (gomuks--alist 'rowid item) item gomuks--events))
       (gomuks--render-room (gomuks--alist 'room_id data)))
      ("send_complete"
       (when-let* ((failure (gomuks--alist 'error data)))
         (message "gomuks: send failed: %s" failure)))
      ("client_state"
       (setq gomuks--user-id (gomuks--alist 'user_id data))
       (unless (gomuks--alist 'is_initialized data)
         (message "gomuks: finish Matrix login in the web frontend first"))))))

(defun gomuks--apply-sync (sync)
  "Apply room and notification changes from SYNC."
  (when (gomuks--alist 'clear_state sync) (gomuks--reset))
  (dolist (id (gomuks--alist 'left_rooms sync))
    (remhash id gomuks--rooms)
    (remhash id gomuks--timelines)
    (remhash id gomuks--member-state))
  (dolist (entry (gomuks--alist 'rooms sync))
    (let* ((id (gomuks--key-string (car entry)))
           (room (cdr entry))
           (meta (gomuks--alist 'meta room))
           (timeline (gomuks--alist 'timeline room)))
      (when meta
        (when (and (gomuks--alist 'marked_unread meta)
                   (not (gomuks--alist 'marked_unread
                                       (gethash id gomuks--rooms))))
          (remhash id gomuks--last-read))
        (puthash id meta gomuks--rooms))
      (dolist (item (gomuks--alist 'events room))
        (puthash (gomuks--alist 'rowid item) item gomuks--events))
      (when-let* ((members (gomuks--alist 'm.room.member
                                          (gomuks--alist 'state room))))
        (let ((state (or (gethash id gomuks--member-state)
                         (make-hash-table :test 'equal))))
          (dolist (entry members)
            (puthash (gomuks--key-string (car entry)) (cdr entry) state))
          (puthash id state gomuks--member-state)))
      (when (gomuks--alist 'reset room)
        (puthash id nil gomuks--timelines)
        (puthash id nil gomuks--timeline-ids)
        (when-let* ((buffer (get-buffer (format "*Gomuks: %s*" id))))
          (with-current-buffer buffer
            (setq gomuks--initial-history-requested nil))))
      (when timeline
        (dolist (item timeline)
          (let ((rowid (gomuks--alist 'event_rowid item))
                (timeline-id (gomuks--alist 'timeline_rowid item)))
            (unless (member rowid (gethash id gomuks--timelines))
              (puthash id (append (gethash id gomuks--timelines) (list rowid))
                       gomuks--timelines))
            (puthash rowid timeline-id gomuks--timeline-ids))))
      (dolist (buffer (buffer-list))
        (when (with-current-buffer buffer
                (and (derived-mode-p 'gomuks-room-mode)
                     gomuks--thread-root (equal gomuks--room-id id)))
          (with-current-buffer buffer
            (dolist (item (gomuks--alist 'events room))
              (when (and (equal (gomuks--alist 'relates_to item)
                                gomuks--thread-root)
                         (equal (gomuks--alist 'relation_type item) "m.thread"))
                (setq gomuks--thread-events
                      (gomuks--merge-thread-events
                       gomuks--thread-root gomuks--thread-events (list item)))))
            (gomuks--render-buffer buffer id))))
      (gomuks--render-room id)))
  (gomuks--render-rooms)
  (gomuks--maybe-mark-read)
  (dolist (entry (gomuks--alist 'rooms sync))
    (let ((id (gomuks--key-string (car entry))))
      (dolist (notice (gomuks--alist 'notifications (cdr entry)))
        (when-let* ((event (gethash (gomuks--alist 'event_rowid notice)
                                   gomuks--events)))
          (gomuks--notify id event))))))

(defun gomuks--room-name (id)
  "Return the display name of room ID, or ID when unnamed."
  (or (gomuks--alist 'name (gethash id gomuks--rooms)) id))

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
        (maphash (lambda (id meta) (push (cons id meta) entries)) gomuks--rooms)
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
                " open   " (propertize (if (bound-and-true-p evil-mode)
                                             "g r" "g")
                                         'face 'help-key-binding)
                " reconnect   " (propertize "q" 'face 'help-key-binding)
                " leave\n\n  " (propertize "RECENT ROOMS" 'face 'gomuks-heading-face)
                "\n\n")
        (unless entries
          (insert "  Waiting for rooms from the backend…\n"))
        (dolist (entry entries)
          (let* ((start (point))
                 (id (car entry))
                 (meta (cdr entry))
                 (unread (or (gomuks--alist 'unread_notifications meta) 0))
                 (preview-event (gethash (gomuks--alist 'preview_event_rowid meta)
                                         gomuks--events))
                 (preview (or (gomuks--alist 'body
                                                  (gomuks--event-content preview-event))
                              (gomuks--alist 'topic meta) "")))
            (insert "  "
                    (propertize (format "%-31s"
                                        (truncate-string-to-width
                                         (gomuks--room-name id) 30 nil nil "…"))
                                'face 'gomuks-room-face)
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
  (catch 'found
    (maphash (lambda (_rowid event)
               (when (and (equal (gomuks--alist 'room_id event) room-id)
                          (equal (gomuks--alist 'event_id event) event-id))
                 (throw 'found event)))
             gomuks--events)
    nil))

(defun gomuks--visible-body (event body)
  "Return BODY without Matrix's quoted reply fallback for EVENT."
  (if (and (stringp body) (gomuks--reply-target event)
           (string-prefix-p "> " body)
           (string-match "\n\n" body))
      (substring body (match-end 0))
    body))

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

(defun gomuks--request-missing-members (id events)
  "Fetch room member ID events needed to name senders in EVENTS."
  (when (process-live-p gomuks--stream)
    (let (keys)
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
         "get_specific_room_state" `((keys . ,(vconcat (nreverse keys))))
         (lambda (failure response)
           (if failure
               (dolist (key keys)
                 (remhash (cons id (gomuks--alist 'state_key key))
                          gomuks--requested-members))
             (let ((state (or (gethash id gomuks--member-state)
                              (make-hash-table :test 'equal))))
               (dolist (event response)
                 (when (and (equal (gomuks--alist 'type event) "m.room.member")
                            (equal (gomuks--alist 'room_id event) id))
                   (puthash (gomuks--alist 'rowid event) event gomuks--events)
                   (unless (gethash (gomuks--alist 'state_key event) state)
                     (puthash (gomuks--alist 'state_key event)
                              (gomuks--alist 'rowid event) state))))
               (puthash id state gomuks--member-state)
               (gomuks--render-room id)))))))))

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
        (error nil)))))

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

(defun gomuks--attachment (content)
  "Return attachment details from message CONTENT, if present."
  (let* ((file (gomuks--alist 'file content))
         (mxc (or (gomuks--alist 'url file)
                  (gomuks--alist 'url content)))
         (info (gomuks--alist 'info content)))
    (when (and (stringp mxc) (string-prefix-p "mxc://" mxc))
      (list :mxc mxc :encrypted (and file t)
            :name (or (gomuks--alist 'filename content)
                      (gomuks--alist 'body content) "attachment")
            :mime (gomuks--alist 'mimetype info)
            :size (gomuks--alist 'size info)
            :kind (gomuks--alist 'msgtype content)))))

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

(defun gomuks--media-key (attachment)
  "Return the cache key for ATTACHMENT."
  (list gomuks-backend-url
        (plist-get attachment :mxc)
        (plist-get attachment :encrypted)))

(defun gomuks--cached-media (attachment)
  "Return ATTACHMENT's cached local path if the file still exists."
  (let* ((key (gomuks--media-key attachment))
         (path (gethash key gomuks--media-cache)))
    (if (and path (file-exists-p path))
        path
      (remhash key gomuks--media-cache)
      nil)))

(defun gomuks--media-url (attachment)
  "Return the backend download URL for ATTACHMENT."
  (let ((mxc (plist-get attachment :mxc)))
    (unless (string-match "\\`mxc://\\([^/]+\\)/\\(.+\\)\\'" mxc)
      (user-error "Invalid attachment URL"))
    (gomuks--url
     (format "media/%s/%s?encrypted=%s"
             (url-hexify-string (match-string 1 mxc))
             (url-hexify-string (match-string 2 mxc))
             (if (plist-get attachment :encrypted) "true" "false")))))

(defun gomuks--fetch-media (attachment callback)
  "Fetch ATTACHMENT and call CALLBACK with a local path or nil."
  (let* ((key (gomuks--media-key attachment))
         (cached (gomuks--cached-media attachment))
         (pending (gethash key gomuks--media-pending)))
    (cond
     (cached (funcall callback cached))
     (pending
      (puthash key (cons callback pending) gomuks--media-pending))
     (t
      (unless (executable-find "curl")
        (user-error "Curl is required to load attachments"))
      (remhash key gomuks--media-failures)
      (let* ((extension (file-name-extension
                         (plist-get attachment :name)))
             (path (make-temp-file "gomuks-media-" nil
                                   (if extension (concat "." extension) ".bin")))
             (auth (gomuks--authorization))
             (url (gomuks--media-url attachment)))
        (puthash key (list callback) gomuks--media-pending)
        (let ((process
               (make-process
                :name "gomuks-media" :buffer nil :connection-type 'pipe
                :command (list "curl" "--silent" "--show-error" "--fail"
                               "--location" "--output" path "--config" "-" url)
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((callbacks (gethash key gomuks--media-pending))
                          (ok (= (process-exit-status proc) 0)))
                      (remhash key gomuks--media-pending)
                      (if ok
                          (puthash key path gomuks--media-cache)
                        (delete-file path)
                        (puthash key t gomuks--media-failures)
                        (message "gomuks: attachment download failed"))
                      (dolist (fn callbacks)
                        (funcall fn (when ok path)))))))))
          ;; The auth token stays off the process command line.
          (process-send-string process
                               (if auth
                                   (format "header = \"Authorization: %s\"\n" auth)
                                 ""))
          (process-send-eof process)))))))

(defun gomuks--maybe-preview-image (attachment id)
  "Fetch a small image ATTACHMENT and redraw room ID when ready."
  (let ((key (gomuks--media-key attachment))
        (size (plist-get attachment :size)))
    (when (and gomuks-inline-images (display-images-p)
               (member (plist-get attachment :kind) '("m.image" "m.sticker"))
               (numberp size)
               (<= size gomuks-inline-image-max-bytes)
               (not (gomuks--cached-media attachment))
               (not (gethash key gomuks--media-pending))
               (not (gethash key gomuks--media-failures)))
      (gomuks--fetch-media
       attachment
       (lambda (path)
         (when path (gomuks--render-room id)))))))

(defun gomuks--insert-attachment (event attachment)
  "Insert ATTACHMENT controls for EVENT at point."
  (let* ((name (plist-get attachment :name))
         (size (plist-get attachment :size))
         (kind (or (plist-get attachment :kind) "m.file"))
         (cached (gomuks--cached-media attachment)))
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
    (when (and cached (display-images-p)
               (member kind '("m.image" "m.sticker")))
      (condition-case nil
          (when-let* ((image (create-image cached nil nil
                                          :max-width 360 :max-height 220)))
            (insert "    ")
            (insert-image image "[image preview]")
            (insert "\n"))
        (error nil)))
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
                       (gomuks--attachment content)))
         (body (if (gomuks--alist 'redacted_by event)
                   "[redacted]"
                 (gomuks--visible-body event (gomuks--alist 'body content))))
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
        (when (and body
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
                   (or (not body) (equal body (plist-get attachment :name))))
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
                " older   " (propertize "b" 'face 'help-key-binding)
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
                    (event-id (gomuks--alist 'event_id event)))
          (unless (equal event-id (gethash id gomuks--last-read))
            (puthash id event-id gomuks--last-read)
            (gomuks--post
             "mark_read"
             `((room_id . ,id) (event_id . ,event-id)
               (receipt_type . "m.read"))
             (lambda (failure _response)
               (when failure
                 (when (equal event-id (gethash id gomuks--last-read))
                   (remhash id gomuks--last-read))
                 (message "gomuks: read receipt failed: %s" failure))))))))))

(defun gomuks--pending-attachments ()
  "Return this draft's attachments, migrating an older single entry."
  (when (stringp (car-safe gomuks--compose-attachment))
    (setq gomuks--compose-attachment (list gomuks--compose-attachment)))
  gomuks--compose-attachment)

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

(defun gomuks--show-room (buffer)
  "Display room BUFFER with its composer below it."
  (delete-other-windows)
  (switch-to-buffer buffer)
  (let ((composer-window (split-window (selected-window) -6 'below)))
    (set-frame-parameter nil 'gomuks-composer-window composer-window)
    (set-window-buffer composer-window (gomuks--composer-buffer buffer)))
  (select-window (get-buffer-window buffer)))

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
               (when relation `((relates_to . ,relation))))
       (lambda (failure _response)
         (gomuks--finish-compose-send buffer text failure))))))

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
      (gomuks--maybe-mark-read))))

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
    (dolist (event events)
      (when-let* ((rowid (gomuks--alist 'rowid event)))
        (puthash rowid event gomuks--events)))
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
         (room (get-buffer (format "*Gomuks: %s*" id)))
         (current-position (gomuks--event-id-position target))
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
         (if failure
             (gomuks--post
              "get_event" `((room_id . ,id) (event_id . ,target))
              (lambda (event-failure event)
                (if event-failure
                    (message "gomuks: could not follow reply: %s" event-failure)
                  (gomuks--show-reply-context id target source (list event)))))
           (dolist (event (gomuks--alist 'related_events response))
             (when-let* ((rowid (gomuks--alist 'rowid event)))
               (puthash rowid event gomuks--events)))
           (gomuks--show-reply-context
            id target source
            (append (reverse (gomuks--alist 'before response))
                    (list (gomuks--alist 'event response))
                    (gomuks--alist 'after response))))))))))

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
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (= generation gomuks--search-generation)
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
                   (dolist (event (gomuks--alist 'events response))
                     (when-let* ((rowid (gomuks--alist 'rowid event)))
                       (puthash rowid event gomuks--events))))
                 (gomuks--render-search))))))))))

(defun gomuks-search ()
  "Search the backend's local history for messages in this room."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (let ((query (string-trim (read-string "Search room: ")))
        (source (current-buffer))
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
         (source (current-buffer)))
    (unless target (user-error "This search result has no event ID"))
    (gomuks--post
     "get_event_context" `((room_id . ,id) (event_id . ,target) (limit . 12))
     (lambda (failure response)
       (if failure
           (gomuks--show-reply-context id target source (list event))
         (dolist (related (gomuks--alist 'related_events response))
           (when-let* ((rowid (gomuks--alist 'rowid related)))
             (puthash rowid related gomuks--events)))
         (gomuks--show-reply-context
          id target source
          (append (reverse (gomuks--alist 'before response))
                  (list (gomuks--alist 'event response))
                  (gomuks--alist 'after response))))))))

(defun gomuks-search-back ()
  "Return from search results to the room that opened them."
  (interactive)
  (if (buffer-live-p gomuks--parent-buffer)
      (gomuks--show-room gomuks--parent-buffer)
    (gomuks-home)))

(defun gomuks--request (path data callback &optional content-type)
  "POST DATA to backend PATH and pass the result to CALLBACK.
CONTENT-TYPE defaults to JSON.  CALLBACK receives an error or nil,
followed by the parsed response or nil."
  (let ((url-request-method "POST")
        (url-request-data data)
        (url-request-extra-headers
         (append (list (cons "Content-Type" (or content-type "application/json")))
                 (when-let* ((auth (gomuks--authorization)))
                   (list (cons "Authorization" auth))))))
    (url-retrieve
     (gomuks--url path)
     (lambda (status)
       (let ((response-buffer (current-buffer)))
         (unwind-protect
             (let ((code (or (symbol-value 'url-http-response-status) 0)))
               (goto-char (point-min))
               (let ((body (buffer-substring-no-properties
                            (or (and (re-search-forward "\r?\n\r?\n" nil t)
                                     (point))
                                (point-min))
                            (point-max))))
                 (if (and (null (plist-get status :error)) (= code 200))
                     (condition-case err
                         (funcall callback nil (json-parse-string body :object-type 'alist
                                                                 :array-type 'list
                                                                 :false-object nil
                                                                 :null-object nil))
                       (error (funcall callback (format "Invalid JSON response: %s" err) nil)))
                   (funcall callback (format "HTTP %s: %s" code
                                             (or (plist-get status :error) body)) nil))))
           (when (buffer-live-p response-buffer)
             (kill-buffer response-buffer)))))
     nil t)))

(defun gomuks--post (command data callback)
  "Send JSON DATA to backend COMMAND and call CALLBACK with the result."
  (gomuks--request (concat "exec/" command)
                   (encode-coding-string (json-serialize data) 'utf-8)
                   callback))

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

(defun gomuks-insert-emoji ()
  "Search for an emoji and insert it into the current draft."
  (interactive)
  (unless (derived-mode-p 'gomuks-compose-mode)
    (user-error "Focus a Gomuks draft first"))
  (if (fboundp 'emoji-search)
      (call-interactively #'emoji-search)
    (insert (read-char-by-name "Emoji: " t))))

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
        (event gomuks--reactions-event)
        (key gomuks--reactions-key))
    (unless event-id (user-error "No reaction on this line"))
    (unless (and gomuks--user-id (equal sender gomuks--user-id))
      (user-error "You can only remove your own reaction"))
    (when (yes-or-no-p (format "Remove your %s reaction? " key))
      (gomuks--post
       "redact_event" `((room_id . ,room-id) (event_id . ,event-id))
       (lambda (failure _response)
         (if failure
             (message "gomuks: could not remove reaction: %s" failure)
           (when (and event key (buffer-live-p buffer))
             (with-current-buffer buffer
               (gomuks-show-reactions event key)))))))))

(defun gomuks-show-reactions (event key)
  "Show the people who reacted to EVENT with KEY."
  (let* ((id gomuks--room-id)
         (event-id (gomuks--alist 'event_id event))
         (buffer (get-buffer-create
                  (format "*Gomuks reactions: %s %s*" event-id key))))
    (unless event-id (user-error "This message has no event ID yet"))
    (with-current-buffer buffer
      (gomuks-reactions-mode)
      (setq-local gomuks--room-id id
                  gomuks--reactions-event event
                  gomuks--reactions-key key)
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
                (when (buffer-live-p buffer)
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
           (setq gomuks--user-id (gomuks--alist 'user_id state))
           (funcall load)))))))

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
            (root gomuks--thread-root)
            (token gomuks--thread-next-batch))
        (gomuks--post
         "paginate_manual"
         `((room_id . ,id) (thread_root . ,root) (since . ,token)
           (direction . "b") (limit . 50))
         (lambda (failure response)
           (if failure (message "gomuks: %s" failure)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (dolist (event (gomuks--alist 'events response))
                   (when-let* ((rowid (gomuks--alist 'rowid event)))
                     (puthash rowid event gomuks--events)))
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
         (oldest (car (gethash id gomuks--timelines))))
    (gomuks--post
     "paginate" `((room_id . ,id)
                  (max_timeline_id . ,(or (gethash oldest gomuks--timeline-ids) 0))
                  (limit . 50))
     (lambda (failure response)
       (if failure
           (progn
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq gomuks--initial-history-requested nil)))
             (message "gomuks: %s" failure))
         (let ((new-rows nil))
           (dolist (event (gomuks--alist 'events response))
             (let ((rowid (gomuks--alist 'rowid event)))
               (puthash rowid event gomuks--events)
               (puthash rowid (gomuks--alist 'timeline_rowid event)
                        gomuks--timeline-ids)
               (when (and (gomuks--alist 'timeline_rowid event)
                          (not (member rowid (gethash id gomuks--timelines))))
                 (push rowid new-rows))))
           (puthash id (append new-rows (gethash id gomuks--timelines))
                    gomuks--timelines)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer (gomuks--render-room id)))
           (gomuks--maybe-mark-read)
           (message "gomuks: loaded %d older messages" (length new-rows)))))))))

(defun gomuks--upload-file (file room-id callback)
  "Upload FILE for ROOM-ID, then call CALLBACK with error and content."
  (unless (executable-find "curl")
    (user-error "Curl is required to upload attachments"))
  (let* ((filename (file-name-nondirectory file))
         (encrypted (gomuks--alist 'encryption_event
                                   (gethash room-id gomuks--rooms)))
         (url (gomuks--url
               (format "upload?filename=%s&encrypt=%s"
                       (url-hexify-string filename)
                       (if encrypted "true" "false"))))
         (response-file (make-temp-file "gomuks-upload-response-"))
         (auth (gomuks--authorization))
         (process
          (make-process
           :name "gomuks-upload" :buffer nil :connection-type 'pipe
           :command (list "curl" "--silent" "--show-error" "--fail"
                          "--request" "POST"
                          "--header" "Content-Type: application/octet-stream"
                          "--data-binary" (concat "@" (expand-file-name file))
                          "--output" response-file "--config" "-" url)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let ((failure (unless (= (process-exit-status proc) 0)
                                "upload request failed"))
                     (content nil))
                 (unless failure
                   (condition-case err
                       (setq content
                             (json-parse-string
                              (with-temp-buffer
                                (insert-file-contents response-file)
                                (buffer-string))
                              :object-type 'hash-table :array-type 'array
                              :false-object nil :null-object nil))
                     (error (setq failure (format "invalid upload response: %s" err)))))
                 (delete-file response-file)
                 (funcall callback failure content)))))))
    (process-send-string process
                         (if auth
                             (format "header = \"Authorization: %s\"\n" auth)
                           ""))
    (process-send-eof process)))

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

(defun gomuks-save-attachment (&optional destination event)
  "Save the attachment in EVENT, or at point, to DESTINATION."
  (interactive)
  (let ((attachment (gomuks--attachment
                     (gomuks--event-content (or event (gomuks--event-at-point))))))
    (unless attachment (user-error "No downloadable attachment on this line"))
    (setq destination
          (or destination
              (read-file-name "Save attachment as: " nil nil nil
                              (plist-get attachment :name))))
    (setq destination (expand-file-name destination))
    (when (and (file-exists-p destination)
               (not (yes-or-no-p (format "Overwrite %s? " destination))))
      (user-error "Download cancelled"))
    (gomuks--fetch-media
     attachment
     (lambda (path)
       (when path
         (condition-case err
             (progn
               (copy-file path destination t)
               (message "gomuks: saved %s" destination))
           (error (message "gomuks: save failed: %s" err))))))))

(defun gomuks-open-attachment (&optional event)
  "Load the attachment in EVENT, or at point, and open it."
  (interactive)
  (let* ((attachment (gomuks--attachment
                      (gomuks--event-content (or event (gomuks--event-at-point)))))
         (mime (and attachment (plist-get attachment :mime)))
         (kind (and attachment (plist-get attachment :kind))))
    (unless attachment (user-error "No downloadable attachment on this line"))
    (gomuks--fetch-media
     attachment
     (lambda (path)
       (when path
         (cond
          ((or (string-equal (downcase (or (file-name-extension path) "")) "ogg")
               (equal kind "m.audio")
               (and mime (string-prefix-p "audio/" mime)))
           (pcase gomuks-audio-backend
             ('emms
              (unless (require 'emms-source-file nil t)
                (user-error "EMMS is required to play audio attachments"))
              (emms-play-file path))
             ('empv
              (if (require 'empv nil t)
                  (empv-play path)
                (unless (executable-find "mpv")
                  (user-error "mpv is required to play audio attachments"))
                (start-process "gomuks-audio" nil "mpv" "--no-video" "--" path)))))
          ((or (equal kind "m.video")
               (and mime (string-prefix-p "video/" mime)))
           (browse-url-default-browser (browse-url-file-url path)))
          (t (pop-to-buffer (find-file-noselect path)))))))))

(defun gomuks-audio-toggle ()
  "Pause or resume audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-pause))
    ('empv
     (if (require 'empv nil t)
         (empv-toggle)
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-audio-backward ()
  "Seek five seconds backward in audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-seek -5))
    ('empv
     (if (require 'empv nil t)
         (empv-seek "-5")
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-audio-forward ()
  "Seek five seconds forward in audio playing through the configured backend."
  (interactive)
  (pcase gomuks-audio-backend
    ('emms
     (unless (require 'emms nil t)
       (user-error "EMMS is required for audio controls"))
     (emms-seek 5))
    ('empv
     (if (require 'empv nil t)
         (empv-seek "5")
       (user-error "EMPV is required for audio controls")))))

(defun gomuks-open-thread (&optional event)
  "Open the thread containing EVENT, or the message at point."
  (interactive)
  (let* ((source (current-buffer))
         (event (or event (gomuks--event-at-point)))
         (root (gomuks--thread-root-id event))
         (id gomuks--room-id)
         (root-event (if (equal root (gomuks--alist 'event_id event))
                         event (gomuks--find-event id root)))
         (buffer (get-buffer-create (format "*Gomuks thread: %s*" root)))
         (existing (with-current-buffer buffer
                     (equal gomuks--thread-root root))))
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
        (setq gomuks--thread-next-batch nil)))
    (gomuks--render-buffer buffer id)
    (gomuks--show-room buffer)
    (unless root-event
      (gomuks--post
       "get_event" `((room_id . ,id) (event_id . ,root))
       (lambda (failure fetched)
         (if failure
             (message "gomuks: thread root unavailable: %s" failure)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when-let* ((rowid (gomuks--alist 'rowid fetched)))
                 (puthash rowid fetched gomuks--events))
               (setq gomuks--thread-events
                     (gomuks--merge-thread-events
                      root gomuks--thread-events (list fetched)))
               (gomuks--render-buffer buffer id)))))))
    (unless existing
      (gomuks--post
       "paginate_manual"
       `((room_id . ,id) (thread_root . ,root) (direction . "b") (limit . 50))
       (lambda (failure response)
         (if failure (message "gomuks: %s" failure)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (dolist (event (gomuks--alist 'events response))
                 (when-let* ((rowid (gomuks--alist 'rowid event)))
                   (puthash rowid event gomuks--events)))
               (dolist (related (gomuks--alist 'related_events response))
                 (when-let* ((rowid (gomuks--alist 'rowid related)))
                   (puthash rowid related gomuks--events)))
               (setq gomuks--thread-events
                     (gomuks--merge-thread-events
                      root gomuks--thread-events
                      (gomuks--alist 'events response))
                     gomuks--thread-next-batch
                     (let ((next (gomuks--alist 'next_batch response)))
                       (unless (string-empty-p (or next "")) next)))
               (gomuks--render-room id)))))))))

;;;###autoload
(defun gomuks ()
  "Connect to gomuks and show its room list."
  (interactive)
  (unless (or gomuks--skip-auth-source gomuks--password
              (equal gomuks-username ""))
    (when-let* ((credentials (gomuks--auth-source-credentials)))
      (setq gomuks-username (car credentials)
            gomuks--password (cdr credentials))))
  (unless gomuks-username
    (setq gomuks-username (read-string "Gomuks backend username (empty if disabled): ")))
  (unless (or (string-empty-p gomuks-username) gomuks--password)
    (setq gomuks--password (read-passwd "Gomuks backend password: ")))
  (gomuks-home)
  (unless (process-live-p gomuks--stream)
    (gomuks--start-stream)))

(defun gomuks-disconnect ()
  "Stop the live stream and forget the backend password."
  (interactive)
  (when gomuks--reconnect-timer
    (cancel-timer gomuks--reconnect-timer)
    (setq gomuks--reconnect-timer nil))
  (let ((process gomuks--stream))
    (setq gomuks--stream nil)
    (when (process-live-p process) (delete-process process)))
  (setq gomuks--connection-status "Disconnected")
  (gomuks--render-rooms)
  (setq gomuks--password nil)
  (message "gomuks: disconnected"))

(defun gomuks-reconnect (&optional change-credentials)
  "Reconnect and reload rooms from the backend.
With prefix argument CHANGE-CREDENTIALS, ask for a new username too."
  (interactive "P")
  (gomuks-disconnect)
  (when change-credentials
    (setq gomuks-username nil gomuks--skip-auth-source t))
  (clrhash gomuks--media-failures)
  (gomuks--reset)
  (gomuks))

(defun gomuks-change-credentials ()
  "Ask for backend credentials again, then reconnect."
  (interactive)
  (gomuks-reconnect t))

(defun gomuks--clear-media-cache ()
  "Delete downloaded media files and clear their cache entries."
  (maphash (lambda (_key path)
             (when (file-exists-p path)
               (delete-file path)))
           gomuks--media-cache)
  (clrhash gomuks--media-cache))

(add-hook 'kill-emacs-hook #'gomuks--clear-media-cache)
(add-function :after after-focus-change-function #'gomuks--maybe-mark-read)

(provide 'gomuks)
;;; gomuks.el ends here
