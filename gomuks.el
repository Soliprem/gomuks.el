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
  (gomuks--load-hidden-rooms)
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

(defun gomuks--release-session ()
  "Cancel requests, make pending sends recoverable and drop downloaded media."
  (gomuks--cancel-operations)
  (gomuks--disconnect-sends)
  (gomuks--clear-media-cache))

(defun gomuks-disconnect ()
  "Stop the live stream and forget the backend password."
  (interactive)
  (gomuks--stop-stream)
  (cl-incf gomuks--cache-generation)
  (gomuks--release-session)
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
  (setq gomuks--user-id nil)
  (gomuks--reset-cache)
  (gomuks--reset-views)
  (gomuks))

(defun gomuks-change-credentials ()
  "Ask for backend credentials again, then reconnect."
  (interactive)
  (gomuks-reconnect t))

(defun gomuks--shutdown ()
  "Cancel operations, then release staged, retry and downloaded temporary files."
  (gomuks--stop-stream)
  (cl-incf gomuks--cache-generation)
  (gomuks--cancel-operations)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'gomuks-compose-mode) (gomuks--cleanup-compose-attachment))))
  (gomuks--cleanup-sends)
  (gomuks--clear-media-cache))

(defun gomuks--route-change (event &optional data extra)
  "Update the views, sends and sticker pickers affected by EVENT.
DATA and EXTRA are described in `gomuks--change-functions'."
  (pcase event
    ('status (gomuks--render-rooms))
    ('room (gomuks--update-room-views data))
    ('media (gomuks--update-room-views data)
            (gomuks--refresh-sticker-pickers data))
    ('decrypted (gomuks--update-room-views data)
                (gomuks--render-rooms))
    ('sync (gomuks--show-sync data extra)
           (let ((account-data (gomuks--alist 'account_data data)))
             (gomuks--refresh-sticker-pickers
              nil (or (assq 'm.image_pack.rooms account-data)
                      (assq 'im.ponies.emote_rooms account-data)))))
    ('send-complete (gomuks--complete-send data))
    ('account (gomuks--reset-views)
              (gomuks--release-session))))

(add-hook 'gomuks--change-functions #'gomuks--route-change)
(add-hook 'kill-emacs-hook #'gomuks--shutdown)
(add-function :after after-focus-change-function #'gomuks--maybe-mark-read)

(provide 'gomuks)
;;; gomuks.el ends here
