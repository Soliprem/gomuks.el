;;; gomuks-transport.el --- Bounded request and process lifecycles -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Each operation has one terminal path: cleanup, then exactly one callback.
;; This module knows no room buffers or drafts. The SSE stream has its own owner.

;;; Code:

(require 'gomuks-core)
(defvar gomuks--operations (make-hash-table :test 'eq)
  "Live ordinary requests and transfers, keyed by operation record.")
(cl-defstruct (gomuks--operation (:constructor gomuks--make-operation))
  done timer cancel cleanup callback)

(defun gomuks--begin-operation (callback timeout cleanup)
  "Own a new operation with CALLBACK, TIMEOUT and terminal CLEANUP."
  (let ((op (gomuks--make-operation :callback callback :cleanup cleanup)))
    (puthash op t gomuks--operations)
    (setf (gomuks--operation-timer op)
          (run-at-time timeout nil #'gomuks--finish-operation op
                       "request deadline exceeded" nil t))
    op))

(defun gomuks--finish-operation (op failure value &optional cancel)
  "Terminate OP once with FAILURE and VALUE; abort its handle when CANCEL."
  (unless (gomuks--operation-done op)
    (setf (gomuks--operation-done op) t)
    (remhash op gomuks--operations)
    (when (gomuks--operation-timer op)
      (cancel-timer (gomuks--operation-timer op)))
    (unwind-protect
        (when (and cancel (gomuks--operation-cancel op))
          (funcall (gomuks--operation-cancel op)))
      (unwind-protect
          (when (gomuks--operation-cleanup op)
            (funcall (gomuks--operation-cleanup op)))
        (funcall (gomuks--operation-callback op) failure value)))))

(defun gomuks--cancel-operations ()
  "Cancel all ordinary operations and release each consumer's busy state."
  (let (pending)
    (maphash (lambda (op _) (push op pending)) gomuks--operations)
    (dolist (op pending)
      (condition-case err
          (gomuks--finish-operation op "disconnected" nil t)
        (error (message "gomuks: cancellation callback failed: %s"
                        (error-message-string err)))))))

(defun gomuks--request (path data callback &optional content-type)
  "POST DATA to PATH within a deadline, calling CALLBACK once with error/value."
  (let* ((response-buffer nil)
         (op (gomuks--begin-operation
              callback gomuks-request-timeout
              (lambda ()
                (when (buffer-live-p response-buffer)
                  (kill-buffer response-buffer)))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers
          (append (list (cons "Content-Type" (or content-type "application/json")))
                  (when-let* ((auth (gomuks--authorization)))
                    (list (cons "Authorization" auth))))))
    (condition-case err
        (let ((buffer
               (url-retrieve
                (gomuks--url path)
                (lambda (status)
                  (setq response-buffer (current-buffer))
                  (if (gomuks--operation-done op)
                      (when (buffer-live-p response-buffer) (kill-buffer response-buffer))
                    (let ((code (or (bound-and-true-p url-http-response-status) 0))
                          failure response)
                      (goto-char (point-min))
                      (let ((body (buffer-substring-no-properties
                                   (if (re-search-forward "\r?\n\r?\n" nil t)
                                       (point) (point-min)) (point-max))))
                        (if (and (null (plist-get status :error)) (= code 200))
                            (condition-case parse-error
                                (setq response
                                      (json-parse-string body :object-type 'alist
                                                         :array-type 'list
                                                         :false-object nil :null-object nil))
                              (error (setq failure (format "Invalid JSON response: %s"
                                                           parse-error))))
                          (setq failure (format "HTTP %s: %s" code
                                                (if (string-empty-p (string-trim body))
                                                    (plist-get status :error)
                                                  (or (ignore-errors
                                                        (alist-get
                                                         'error
                                                         (json-parse-string
                                                          body :object-type 'alist)))
                                                      (string-trim body)))))))
                      (gomuks--finish-operation op failure response))))
                nil t)))
          (when (bufferp buffer)
            (setq response-buffer buffer)
            (setf (gomuks--operation-cancel op)
                  (lambda ()
                    (when (buffer-live-p buffer)
                      (when-let* ((process (get-buffer-process buffer)))
                        (delete-process process)))))
            (when (gomuks--operation-done op)
              (when (buffer-live-p buffer) (kill-buffer buffer))))
          op)
      (error
       ;; A consumer failure is already terminal; never report it to it again.
       (if (gomuks--operation-done op) (signal (car err) (cdr err))
         (gomuks--finish-operation op (concat "request could not start: " (error-message-string err)) nil t))
       op))))

(defun gomuks--post (command data callback)
  "Encode JSON DATA and request backend COMMAND, then call CALLBACK."
  (gomuks--request (concat "exec/" command)
                   (encode-coding-string (json-serialize data) 'utf-8) callback))

(defun gomuks--curl-file (url arguments callback &optional limit authorization)
  "Download URL to an owned file with ARGUMENTS; CALLBACK receives error/path.
LIMIT caps actual bytes, including chunked or misleading responses. Successful
file ownership transfers to CALLBACK. AUTHORIZATION is sent through stdin."
  (let* ((path (make-temp-file "gomuks-transfer-"))
         (bytes 0) (keep nil) process
         (op (gomuks--begin-operation
              callback gomuks-transfer-timeout
              (lambda () (unless keep (ignore-errors (delete-file path)))))))
    (condition-case err
        (progn
          (unless (executable-find "curl") (error "Curl is required for media transfers"))
          (setq process
                (make-process
                 :name "gomuks-transfer" :buffer nil :connection-type 'pipe
                 :coding 'binary :noquery t
                 :command
                 (append (list "curl" "--silent" "--fail" "--location"
                               "--connect-timeout" "10" "--max-time"
                               (number-to-string gomuks-transfer-timeout))
                         arguments (list "--output" "-" "--config" "-" url))
                 :filter
                 (lambda (_process chunk)
                   (unless (gomuks--operation-done op)
                     (setq bytes (+ bytes (string-bytes chunk)))
                     (if (and limit (> bytes limit))
                         (gomuks--finish-operation op "download exceeds byte limit" nil t)
                       (condition-case write-error
                           (let ((coding-system-for-write 'no-conversion))
                             (write-region chunk nil path t 'silent))
                         (error (gomuks--finish-operation
                                 op (error-message-string write-error) nil t))))))
                 :sentinel
                 (lambda (proc _event)
                   (when (and (memq (process-status proc) '(exit signal))
                              (not (gomuks--operation-done op)))
                     (setq keep (= (process-exit-status proc) 0))
                     (gomuks--finish-operation
                      op (unless keep "media transfer failed") (and keep path))))))
          (setf (gomuks--operation-cancel op)
                (lambda () (when (process-live-p process) (delete-process process))))
          (process-send-string process
                               (if authorization
                                   (format "header = \"Authorization: %s\"\n" authorization) ""))
          (process-send-eof process)
          op)
      (error
       (if (gomuks--operation-done op) (signal (car err) (cdr err))
         (gomuks--finish-operation op (error-message-string err) nil t))
       op))))

(declare-function gomuks--url "gomuks-backend" (path))
(declare-function gomuks--authorization "gomuks-backend" ())
(provide 'gomuks-transport)
;;; gomuks-transport.el ends here
