;;; gomuks-backend.el --- Backend event coordination -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; The live event stream and backend credentials.  Stream events are stored
;; in the cache, then announced with `gomuks--announce'.

;;; Code:

(require 'gomuks-transport)
(require 'auth-source)

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
  (gomuks--announce 'status)
  (setq gomuks--pending "")
  (let ((authorization (gomuks--authorization)) process)
    (condition-case err
        (progn
          (setq process (make-process
                         :name "gomuks-stream" :buffer nil :connection-type 'pipe
                         :coding 'utf-8-unix :noquery t
                         :command (list "curl" "--no-buffer" "--silent" "--show-error"
                                        "--fail" "--connect-timeout" "10"
                                        "--speed-limit" "1" "--speed-time" "90"
                                        "--config" "-" (gomuks--url "sse"))
                         :filter #'gomuks--stream-filter
                         :sentinel #'gomuks--stream-sentinel))
          (setq gomuks--stream process)
          ;; Send the credential through stdin; it does not appear in the process command line.
          (process-send-string
           process
           (concat "header = \"Accept: application/jsonl\"\n"
                   (when authorization
                     (format "header = \"Authorization: %s\"\n" authorization))))
          (process-send-eof process))
      (error
       (when (eq process gomuks--stream) (setq gomuks--stream nil))
       (when (process-live-p process) (delete-process process))
       (setq gomuks--connection-status "Disconnected")
       (gomuks--announce 'status)
       (signal (car err) (cdr err))))))

(defun gomuks--stop-stream ()
  "Stop the event stream and cancel any scheduled reconnect."
  (when gomuks--reconnect-timer
    (cancel-timer gomuks--reconnect-timer)
    (setq gomuks--reconnect-timer nil))
  (let ((process gomuks--stream))
    (setq gomuks--stream nil)
    (when (process-live-p process) (delete-process process))))

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
    (gomuks--announce 'status)
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
       (gomuks--announce 'status))
      ("events_decrypted"
       (gomuks--store-decryption data)
       (gomuks--announce 'decrypted (gomuks--alist 'room_id data)))
      ("send_complete" (gomuks--announce 'send-complete data))
      ("client_state"
       (gomuks--use-account (gomuks--alist 'user_id data))
       (unless (gomuks--alist 'is_initialized data)
         (message "gomuks: finish Matrix login in the web frontend first"))))))

(defun gomuks--use-account (user-id)
  "Switch to USER-ID, discarding everything cached for a different account."
  (when (and gomuks--user-id (not (equal gomuks--user-id user-id)))
    (gomuks--reset-cache)
    (gomuks--announce 'account))
  (setq gomuks--user-id user-id))

(defun gomuks--apply-sync (sync)
  "Store SYNC, then announce it with the rooms whose views must reset."
  (gomuks--announce 'sync sync (gomuks--store-sync sync)))

(provide 'gomuks-backend)
;;; gomuks-backend.el ends here
