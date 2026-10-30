;;; gomuks-content.el --- Message content helpers -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Build outgoing payloads and read the current (edited) content of events.

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
  "Return EVENT's content after its latest edit, or nil when redacted."
  (let* ((current (or (gethash (gomuks--alist 'rowid event) gomuks--events) event))
         (edit (gethash (gomuks--alist 'last_edit_rowid current) gomuks--events)))
    (if (gomuks--alist 'redacted_by current) nil
      (if edit
          (or (gomuks--alist 'm.new_content (gomuks--event-content edit))
              (gomuks--event-content edit))
        (gomuks--event-content current)))))

(defun gomuks--message-payload (room-id text relation &optional content)
  "Return the send_message payload for TEXT in ROOM-ID.
RELATION and base CONTENT are included when non-nil."
  (append `((room_id . ,room-id) (text . ,text))
          (when content `((base_content . ,content)))
          (when relation `((relates_to . ,relation)))
          (when-let* ((mentions (gomuks--draft-mentions text)))
            `((mentions . ,mentions)))))

(defun gomuks--visible-body (event body)
  "Return BODY without the quoted reply fallback Matrix adds to replies."
  (if (and (stringp body) (gomuks--reply-target event)
           (string-prefix-p "> " body) (string-match "\n\n" body))
      (substring body (match-end 0)) body))

(defun gomuks--edit-source (event)
  "Return the Markdown last used to write EVENT, or its visible body."
  (let* ((current (or (gethash (gomuks--alist 'rowid event) gomuks--events) event))
         (edit (gethash (gomuks--alist 'last_edit_rowid current) gomuks--events))
         (source (gomuks--alist 'edit_source
                                (gomuks--alist 'local_content (or edit current))))
         (body (gomuks--alist 'body (gomuks--effective-content current))))
    (or source (gomuks--visible-body current body))))

(provide 'gomuks-content)
;;; gomuks-content.el ends here
