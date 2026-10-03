;;; gomuks-content.el --- Message transformations -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Pure payload transformations and read-only effective-content queries.

;;; Code:

(require 'gomuks-core)

(defun gomuks--draft-mentions (text)
  "Return Matrix mention metadata for completed mentions in TEXT."
  (let ((start 0) ids)
    (while (string-match "https://matrix\\.to/#/\\(%40[^)[:space:]]+\\)" text start)
      (let ((end (match-end 0))
            (id (url-unhex-string (match-string 1 text))))
        (when (and (string-match-p "\\`@[^:]+:.+\\'" id)
                   (not (member id ids)))
          (push id ids))
        (setq start end)))
    (when ids
      `((user_ids . ,(vconcat (nreverse ids)))))))

(defun gomuks--effective-content (event)
  "Query the current edited content of EVENT without modifying the cache."
  (let* ((current (or (gethash (gomuks--alist 'rowid event) gomuks--events) event))
         (edit (gethash (gomuks--alist 'last_edit_rowid current) gomuks--events)))
    (if (gomuks--alist 'redacted_by current) nil
      (if edit
          (or (gomuks--alist 'm.new_content (gomuks--event-content edit))
              (gomuks--event-content edit))
        (gomuks--event-content current)))))

(defun gomuks--message-payload (room-id text relation &optional content)
  "Transform ROOM-ID, TEXT, RELATION and CONTENT into one send payload."
  (append `((room_id . ,room-id) (text . ,text))
          (when content `((base_content . ,content)))
          (when relation `((relates_to . ,relation)))
          (when-let* ((mentions (gomuks--draft-mentions text)))
            `((mentions . ,mentions)))))

(defun gomuks--edit-source (event)
  "Query the latest saved Markdown for EVENT, falling back to its visible body."
  (let* ((current (or (gethash (gomuks--alist 'rowid event) gomuks--events) event))
         (edit (gethash (gomuks--alist 'last_edit_rowid current) gomuks--events))
         (source (gomuks--alist 'edit_source
                                (gomuks--alist 'local_content (or edit current))))
         (body (gomuks--alist 'body (gomuks--effective-content current))))
    (or source
        (if (and (gomuks--reply-target current) (stringp body)
                 (string-prefix-p "> " body) (string-match "\n\n" body))
            (substring body (match-end 0)) body))))

(provide 'gomuks-content)
;;; gomuks-content.el ends here
