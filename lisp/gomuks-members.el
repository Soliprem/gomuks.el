;;; gomuks-members.el --- Member profile requests -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Fetch the member profiles needed for sender names and @-completion,
;; asking for each one at most once per room cache.

;;; Code:

(require 'gomuks-backend)

(defun gomuks--request-missing-members (id events)
  "Request, in one batch, the sender profiles EVENTS need in room ID."
  (when (process-live-p gomuks--stream)
    (let ((keys nil) (token (gomuks--cache-token id)))
      (dolist (event events)
        (when-let* ((sender (gomuks--alist 'sender event)))
          (let* ((state (gethash id gomuks--member-state))
                 (row (and state (gethash sender state))) (key (cons id sender)))
            (unless (or (and row (gethash row gomuks--events))
                        (gethash key gomuks--requested-members))
              (puthash key t gomuks--requested-members)
              (push `((room_id . ,id) (type . "m.room.member") (state_key . ,sender)) keys)))))
      (when keys
        (gomuks--post
         "get_specific_room_state" `((keys . ,(vconcat (reverse keys))))
         (lambda (failure response)
           (when (gomuks--cache-current-p id token)
             (if failure
                 (dolist (key keys)
                   (remhash (cons id (gomuks--alist 'state_key key)) gomuks--requested-members))
               (gomuks--store-members id response t)
               (gomuks--announce 'room id)))))))))

(defun gomuks--request-mention-members (id)
  "Request room ID's member list for completion, once per room cache."
  (when (and id (not (gethash id gomuks--mention-members-loaded)))
    (let ((token (gomuks--cache-token id)))
      (puthash id 'loading gomuks--mention-members-loaded)
      (gomuks--post
       "get_room_state"
       (append `((room_id . ,id) (include_members . t))
               (unless (gomuks--alist 'has_member_list (gethash id gomuks--rooms))
                 '((fetch_members . t))))
       (lambda (failure events)
         (when (gomuks--cache-current-p id token)
           (if failure (remhash id gomuks--mention-members-loaded)
             (gomuks--store-members id events)
             (puthash id t gomuks--mention-members-loaded))))))))

(provide 'gomuks-members)
;;; gomuks-members.el ends here
