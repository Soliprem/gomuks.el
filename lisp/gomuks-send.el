;;; gomuks-send.el --- Submission and delivery ownership -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; A send record owns its snapshot and temporary files until confirmed delivery.
;; Phases: uploading -> submitting -> queued -> delivered, or failed/uncertain.
;; Backend acceptance is not delivery. Retry uses the accepted transaction ID.

;;; Code:

(require 'gomuks-media)
(require 'gomuks-content)
(declare-function gomuks--update-composer-header "gomuks-compose" ())
(declare-function gomuks--delete-temp-attachments "gomuks-compose" (attachments))
(declare-function gomuks-compose-mode "gomuks-ui" ())
(cl-defstruct (gomuks--send (:constructor gomuks--make-send))
  id phase buffer room backend account text relation remaining attachment
  caption content event failure timer sticker token (attempt 0))
(defvar gomuks--sends (make-hash-table :test 'eql) "Owned undelivered send records.")
(defvar gomuks--send-serial 0 "Monotonic local submission identifier.")
(defvar gomuks--early-completions nil "Last 64 delivery events, for completion-before-ack races.")
(defvar-local gomuks--compose-job nil "Send record currently associated with this draft.")

(defun gomuks--set-send-phase (send phase &optional failure)
  "Write SEND's PHASE and FAILURE and update its draft's busy indicator."
  (setf (gomuks--send-phase send) phase (gomuks--send-failure send) failure)
  (when (buffer-live-p (gomuks--send-buffer send))
    (with-current-buffer (gomuks--send-buffer send)
      (setq gomuks--compose-sending (memq phase '(uploading submitting queued checking)))
      (gomuks--update-composer-header))))

(defun gomuks--send-failed (send failure &optional uncertain)
  "Retain SEND and its files for recovery after FAILURE."
  (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
  (gomuks--set-send-phase send (if uncertain 'uncertain 'failed) failure)
  (message "gomuks: %s; C-c C-y opens send recovery" failure))

(defun gomuks--send-key (event)
  "Transform EVENT into its delivery correlation key."
  (or (gomuks--alist 'transaction_id event) (gomuks--alist 'rowid event)))

(defun gomuks--send-record-event (send event)
  "Write SEND's EVENT into the current account's cache when it still owns it."
  (when (and (equal (gomuks--send-backend send) gomuks-backend-url)
             (equal (gomuks--send-account send) gomuks--user-id)
             (gomuks--cache-current-p (gomuks--send-room send) (gomuks--send-token send)))
    (gomuks--store-events (list event))
    (when-let* ((timeline (gomuks--alist 'timeline_rowid event)))
      (gomuks--store-timeline (gomuks--send-room send)
                              `(((event_rowid . ,(gomuks--alist 'rowid event))
                                 (timeline_rowid . ,timeline)))))
    (gomuks--render-room (gomuks--send-room send))))

(defun gomuks--send-accepted (send failure event)
  "Handle HTTP acceptance for SEND; keep its snapshot until delivery."
  (cond
   (failure (gomuks--send-failed
             send failure (not (or (string-match-p "\\`HTTP [45][0-9][0-9]" failure)
                                   (string-prefix-p "request could not start:" failure)))))
   ((not (gomuks--alist 'rowid event))
    (gomuks--send-failed send "backend returned no pending event identity" t))
   (t
    (setf (gomuks--send-event send) event)
    (gomuks--send-record-event send event)
    (gomuks--set-send-phase send 'queued)
    (setf (gomuks--send-timer send)
          (run-at-time gomuks-transfer-timeout nil #'gomuks--send-failed send
                       "delivery confirmation timed out" t))
    (if-let* ((completed (cl-find (gomuks--send-key event) gomuks--early-completions
                                  :key (lambda (data)
                                         (gomuks--send-key (gomuks--alist 'event data)))
                                  :test #'equal)))
        (progn
          (setq gomuks--early-completions (delq completed gomuks--early-completions))
          (gomuks--send-result send completed))
      (when-let* ((id (gomuks--alist 'event_id event)))
        (unless (string-prefix-p "~" id)
          (gomuks--send-result send `((event . ,event)))))))))

(defun gomuks--send-submit (send)
  "Procedure: submit SEND's current payload and handle synchronous start failure."
  (gomuks--set-send-phase send 'submitting)
  (let ((attempt (cl-incf (gomuks--send-attempt send))))
    (condition-case err
	(gomuks--post
	 "send_message"
	 (gomuks--message-payload (gomuks--send-room send) (gomuks--send-caption send)
				  (gomuks--send-relation send) (gomuks--send-content send))
	 (lambda (failure event)
           (when (and (= attempt (gomuks--send-attempt send))
                      (eq (gomuks--send-phase send) 'submitting))
             (gomuks--send-accepted send failure event))))
      (error (when (and (= attempt (gomuks--send-attempt send))
			(eq (gomuks--send-phase send) 'submitting))
               (gomuks--send-failed send (error-message-string err)))))))

(defun gomuks--send-next (send)
  "Procedure: upload and submit the next attachment, or the text snapshot."
  (let ((attachment (car (gomuks--send-remaining send)))
        (attempt (cl-incf (gomuks--send-attempt send))))
    (setf (gomuks--send-attachment send) attachment
          (gomuks--send-event send) nil
          (gomuks--send-content send) nil
          (gomuks--send-caption send)
          (if (cdr (gomuks--send-remaining send)) "" (gomuks--send-text send)))
    (if (not attachment) (gomuks--send-submit send)
      (gomuks--set-send-phase send 'uploading)
      (condition-case err
          (gomuks--upload-file
           (car attachment) (gomuks--send-room send)
           (lambda (failure content)
             (when (and (= attempt (gomuks--send-attempt send))
                        (eq (gomuks--send-phase send) 'uploading))
               (if failure (gomuks--send-failed send failure)
		 (when (gomuks--send-sticker send) (puthash "msgtype" "m.sticker" content))
		 (setf (gomuks--send-content send) content)
		 (gomuks--send-submit send)))))
        (error (gomuks--send-failed send (error-message-string err)))))))

(defun gomuks--start-send (buffer room text relation attachments &optional sticker)
  "Procedure: snapshot one submission and transfer its file ownership to a record."
  (let ((send (gomuks--make-send
               :id (cl-incf gomuks--send-serial) :buffer buffer :room room
               :backend gomuks-backend-url :account gomuks--user-id :text text
               :relation (copy-tree relation) :remaining (copy-sequence attachments)
               :token (gomuks--cache-token room)
               :sticker sticker)))
    (puthash (gomuks--send-id send) send gomuks--sends)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (setq gomuks--compose-job send)))
    (gomuks--send-next send)
    send))

(defun gomuks--send-result (send data)
  "Write a correlated delivery result for SEND and continue its ordered batch."
  (when (memq (gomuks--send-phase send) '(queued uncertain failed checking))
    (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
    (let ((event (gomuks--alist 'event data))
          (failure (or (gomuks--alist 'error data)
                       (gomuks--alist 'send_error (gomuks--alist 'event data))))
          (buffer (gomuks--send-buffer send)))
      (setf (gomuks--send-event send) event)
      (gomuks--send-record-event send event)
      (if failure (gomuks--send-failed send failure)
        (when-let* ((attachment (gomuks--send-attachment send)))
          (gomuks--delete-temp-attachments (list attachment))
          (setf (gomuks--send-remaining send) (cdr (gomuks--send-remaining send)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (setq gomuks--compose-attachment (delq attachment gomuks--compose-attachment)))))
        (if (gomuks--send-remaining send) (gomuks--send-next send)
          (gomuks--set-send-phase send 'delivered)
          (remhash (gomuks--send-id send) gomuks--sends)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (equal (gomuks--send-text send) (string-trim-right (buffer-string)))
                (erase-buffer) (set-buffer-modified-p nil))
              (when (eq gomuks--compose-job send) (setq gomuks--compose-job nil))))
          (message "gomuks: message delivered"))))))

(defun gomuks--complete-send (data)
  "Procedure: correlate a stream delivery DATA with its retained submission."
  (let* ((event (gomuks--alist 'event data)) (key (gomuks--send-key event)) found)
    (when (and event (gethash (gomuks--alist 'room_id event) gomuks--rooms))
      (gomuks--store-events (list event))
      (gomuks--render-room (gomuks--alist 'room_id event)))
    (when key
      (maphash (lambda (_id send)
                 (when (and (equal key (gomuks--send-key (gomuks--send-event send)))
                            (equal (gomuks--send-backend send) gomuks-backend-url)
                            (equal (gomuks--send-account send) gomuks--user-id))
                   (setq found send))) gomuks--sends)
      (if (and found (not (eq (gomuks--send-phase found) 'submitting)))
          (gomuks--send-result found data)
        (push data gomuks--early-completions)
        (when (> (length gomuks--early-completions) 64)
          (setcdr (nthcdr 63 gomuks--early-completions) nil))))))

(defun gomuks--disconnect-sends ()
  "Make queued deliveries recoverable without sending anything automatically."
  (maphash (lambda (_id send)
             (when (memq (gomuks--send-phase send) '(queued submitting uploading checking))
               (cl-incf (gomuks--send-attempt send))
               (gomuks--send-failed send "disconnected before delivery confirmation" t)))
           gomuks--sends)
  (setq gomuks--early-completions nil))

(defun gomuks--retry-send (send)
  "Procedure: retry SEND using the backend transaction when already accepted."
  (unless (and (equal (gomuks--send-backend send) gomuks-backend-url)
               (equal (gomuks--send-account send) gomuks--user-id))
    (user-error "This send belongs to a different account or backend"))
  (when (memq (gomuks--send-phase send) '(queued uploading submitting checking))
    (user-error "This send is still pending"))
  (when (and (eq (gomuks--send-phase send) 'uncertain) (not (gomuks--send-event send)))
    (user-error "Acceptance is unknown; inspect the room, then restore or discard this send"))
  (if (and (eq (gomuks--send-phase send) 'uncertain) (gomuks--send-event send))
      (gomuks--reconcile-send send)
    (if-let* ((txn (gomuks--alist 'transaction_id (gomuks--send-event send))))
	(let ((attempt (cl-incf (gomuks--send-attempt send))))
          (setf (gomuks--send-token send) (gomuks--cache-token (gomuks--send-room send)))
          (gomuks--set-send-phase send 'submitting)
          (condition-case err
              (gomuks--post "resend_event" `((transaction_id . ,txn))
                            (lambda (failure event)
                              (when (and (= attempt (gomuks--send-attempt send))
					 (eq (gomuks--send-phase send) 'submitting))
				(gomuks--send-accepted send failure event))))
            (error (gomuks--send-failed send (error-message-string err)))))
      (when (gomuks--send-event send)
	(user-error "Backend supplied no transaction ID; restore this draft before resubmitting"))
      (if (gomuks--send-content send) (gomuks--send-submit send) (gomuks--send-next send)))))

(defun gomuks-retry-send ()
  "Choose and retry a retained failed send without duplicating accepted events."
  (interactive)
  (let (choices)
    (maphash (lambda (_id send)
               (when (memq (gomuks--send-phase send) '(failed uncertain))
                 (push (cons (format "%d: %s · %s · %s" (gomuks--send-id send)
                                     (gomuks--room-name (gomuks--send-room send))
                                     (gomuks--send-phase send)
                                     (truncate-string-to-width (gomuks--send-text send) 50)) send)
                       choices))) gomuks--sends)
    (unless choices (user-error "No failed sends to recover"))
    (let ((send (or (and (memq gomuks--compose-job (mapcar #'cdr choices)) gomuks--compose-job)
                    (cdr (assoc (completing-read "Retry send: " choices nil t) choices)))))
      (gomuks--retry-send send))))

(defun gomuks-restore-send ()
  "Restore a retained send snapshot to a draft, including its undelivered files."
  (interactive)
  (let (choices)
    (maphash (lambda (_id send)
               (when (memq (gomuks--send-phase send) '(failed uncertain))
                 (push (cons (format "%d: %s" (gomuks--send-id send)
                                     (gomuks--room-name (gomuks--send-room send))) send) choices)))
             gomuks--sends)
    (unless choices (user-error "No failed sends to restore"))
    (let* ((send (cdr (assoc (completing-read "Restore draft: " choices nil t) choices)))
           (buffer (get-buffer-create (format "*Gomuks recovered draft: %d*" (gomuks--send-id send)))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'gomuks-compose-mode) (gomuks-compose-mode))
        (unless (> (buffer-size) 0) (insert (gomuks--send-text send)))
        (setq gomuks--room-id (gomuks--send-room send)
              gomuks--compose-relation (gomuks--send-relation send)
              gomuks--compose-attachment (gomuks--send-remaining send)
              gomuks--compose-job send)
        (setf (gomuks--send-buffer send) buffer)
        (gomuks--update-composer-header))
      (pop-to-buffer buffer))))

(defun gomuks--reconcile-send (send)
  "Procedure: look up an uncertain accepted SEND before retrying its transaction."
  (let ((attempt (cl-incf (gomuks--send-attempt send))))
    (gomuks--set-send-phase send 'checking)
    (condition-case err
        (gomuks--post
         "get_event_by_rowid" `((event_rowid . ,(gomuks--alist 'rowid (gomuks--send-event send))))
         (lambda (failure event)
           (when (and (= attempt (gomuks--send-attempt send))
                      (eq (gomuks--send-phase send) 'checking))
             (cond
              (failure (gomuks--send-failed send failure t))
              ((and (stringp (gomuks--alist 'event_id event))
                    (not (string-prefix-p "~" (gomuks--alist 'event_id event))))
               (gomuks--set-send-phase send 'queued)
               (gomuks--send-result send `((event . ,event))))
              (t
               (gomuks--set-send-phase send 'failed)
               (gomuks--retry-send send))))))
      (error (gomuks--send-failed send (error-message-string err) t)))))

(defun gomuks-discard-send ()
  "Forget a failed submission's delivery tracking after confirmation.
A live draft keeps its text and attachments; closed drafts release their
owned temporary files. Check the room before resubmitting an uncertain send."
  (interactive)
  (let (choices)
    (maphash (lambda (_id send)
               (when (memq (gomuks--send-phase send) '(failed uncertain))
                 (push (cons (format "%d: %s" (gomuks--send-id send)
                                     (gomuks--room-name (gomuks--send-room send))) send) choices)))
             gomuks--sends)
    (unless choices (user-error "No failed sends to discard"))
    (let ((send (or (and (memq gomuks--compose-job (mapcar #'cdr choices)) gomuks--compose-job)
                    (cdr (assoc (completing-read "Forget send tracking: " choices nil t) choices)))))
      (when (yes-or-no-p "Forget this send's tracking? Check delivery before submitting its draft again. ")
        (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
        (remhash (gomuks--send-id send) gomuks--sends)
        (if (buffer-live-p (gomuks--send-buffer send))
            (with-current-buffer (gomuks--send-buffer send)
              (setq gomuks--compose-job nil gomuks--compose-sending nil)
              (gomuks--update-composer-header))
          (gomuks--delete-temp-attachments (gomuks--send-remaining send)))))))

(defun gomuks--cleanup-sends ()
  "Release temporary retry files when the Emacs session ends."
  (maphash (lambda (_id send)
             (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
             (gomuks--delete-temp-attachments (gomuks--send-remaining send))) gomuks--sends)
  (clrhash gomuks--sends))

(provide 'gomuks-send)
;;; gomuks-send.el ends here
