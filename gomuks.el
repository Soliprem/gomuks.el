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

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name
                "lisp" (file-name-directory
                        (or load-file-name
                            (bound-and-true-p byte-compile-current-file)
                            buffer-file-name)))))

(require 'gomuks-ui)

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

(add-hook 'kill-emacs-hook #'gomuks--clear-media-cache)
(add-function :after after-focus-change-function #'gomuks--maybe-mark-read)

(provide 'gomuks)
;;; gomuks.el ends here
