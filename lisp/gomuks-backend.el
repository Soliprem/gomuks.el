;;; gomuks-backend.el --- Backend communication with gomuks service -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-
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

(require 'gomuks-core)
(declare-function gomuks--render-rooms "gomuks-view")
(declare-function gomuks--render-room "gomuks-view" (id))
(declare-function gomuks--render-buffer "gomuks-view" (buffer id))
(declare-function gomuks--maybe-mark-read "gomuks-view")
(declare-function gomuks--notify "gomuks-view" (id event))
(declare-function gomuks--reset-views "gomuks-view" (&optional room-id))

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
                     (let (response failure)
                       (condition-case err
                           (setq response
                                 (json-parse-string body :object-type 'alist
                                                    :array-type 'list
                                                    :false-object nil
                                                    :null-object nil))
                         (error (setq failure (format "Invalid JSON response: %s" err))))
                       (funcall callback failure response))
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

(defun gomuks--stream-filter (process chunk)
  "Parse complete JSON lines from event stream CHUNK."
  (when (eq process gomuks--stream)
    (setq gomuks--pending (concat gomuks--pending chunk))
    (while (and (eq process gomuks--stream) (string-match "\n" gomuks--pending))
      (let ((line (substring gomuks--pending 0 (match-beginning 0))))
	(setq gomuks--pending (substring gomuks--pending (match-end 0)))
	(unless (or (string-empty-p line) (string= line "null"))
          (condition-case err
              (gomuks--handle-event (json-parse-string line :object-type 'alist
                                                       :array-type 'list
                                                       :false-object nil
                                                       :null-object nil))
            (error (message "gomuks: invalid stream event: %s" err))))))))

(defun gomuks--stream-sentinel (process event)
  "Schedule reconnection when stream PROCESS ends with EVENT."
  (when (eq process gomuks--stream)
    (setq gomuks--stream nil)
    (setq gomuks--connection-status "Disconnected")
    (gomuks--render-rooms)
    (message "gomuks: stream %s" (string-trim event))
    (setq gomuks--reconnect-timer
          (run-at-time gomuks-reconnect-delay nil #'gomuks--start-stream))))

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
       (gomuks--store-events (gomuks--alist 'events data))
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
  (when (gomuks--alist 'clear_state sync)
    (gomuks--reset-cache)
    (gomuks--reset-views))
  (when-let* ((rules (gomuks--alist 'm.push_rules
                                   (gomuks--alist 'account_data sync))))
    (let ((muted (make-hash-table :test 'equal)))
      (dolist (rule (gomuks--alist 'room
                                  (gomuks--alist 'global
                                                  (gomuks--alist 'content rules))))
        (when (and (gomuks--alist 'enabled rule)
                   (not (member "notify" (gomuks--alist 'actions rule))))
          (puthash (gomuks--alist 'rule_id rule) t muted)))
      (setq gomuks--muted-rooms muted)))
  (dolist (id (gomuks--alist 'left_rooms sync))
    (gomuks--reset-room-cache id)
    (gomuks--reset-views id)
    (remhash id gomuks--rooms)
    (remhash id gomuks--timelines)
    (remhash id gomuks--member-state)
    (remhash id gomuks--mention-members-loaded))
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
      (gomuks--store-events (gomuks--alist 'events room))
      (when-let* ((members (gomuks--alist 'm.room.member
                                          (gomuks--alist 'state room))))
        (let ((state (or (gethash id gomuks--member-state)
                         (make-hash-table :test 'equal))))
          (dolist (entry members)
            (puthash (gomuks--key-string (car entry)) (cdr entry) state))
          (puthash id state gomuks--member-state)))
      (when (gomuks--alist 'reset room)
        (gomuks--reset-room-cache id)
        (gomuks--reset-views id))
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



(provide 'gomuks-backend)
;;; gomuks-backend.el ends here
