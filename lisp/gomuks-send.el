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

(defun gomuks--send-record-event (send event &optional stream)
  "Procedure: commit EVENT and invalidate once; return non-nil when committed.
STREAM may update a known room independently of SEND's cache lifetime.
Timeline membership still requires SEND to own the current room cache."
  (let* ((owned (and send
                     (equal (gomuks--send-backend send) gomuks-backend-url)
                     (equal (gomuks--send-account send) gomuks--user-id)
                     (gomuks--cache-current-p (gomuks--send-room send)
                                              (gomuks--send-token send))))
         (id (if stream (gomuks--alist 'room_id event) (gomuks--send-room send))))
    (when (and event (if stream (gethash id gomuks--rooms) owned))
      (gomuks--store-events (list event))
      (when-let* ((timeline (and owned (gomuks--alist 'timeline_rowid event))))
        (gomuks--store-timeline (gomuks--send-room send)
                                `(((event_rowid . ,(gomuks--alist 'rowid event))
                                   (timeline_rowid . ,timeline)))))
      (gomuks--update-room-views id)
      t)))

(defun gomuks--send-request (send phase outcome start)
  "Procedure: own one SEND request in PHASE and report its tagged OUTCOME.
START receives a completion callback. The transition owner validates attempts
and phases; synchronous start failures enter that same owner."
  (gomuks--set-send-phase send phase)
  (let ((attempt (cl-incf (gomuks--send-attempt send))))
    (condition-case err
        (funcall start (lambda (failure value)
                         (gomuks--send-step send outcome value failure attempt)))
      (error (gomuks--send-step send 'start-failed phase
                               (error-message-string err) attempt)))))

(defun gomuks--send-step (send action &optional value failure attempt)
  "Procedure: interpret SEND's ACTION against its phase and perform its effects.
VALUE and FAILURE carry the outcome; ATTEMPT rejects stale completions.
The stored-delivery action marks an event already committed by the stream.
This owns mutable workflow state; it is not a pure transformation."
  (when (or (null attempt) (= attempt (gomuks--send-attempt send)))
    (pcase (cons (gomuks--send-phase send) action)
      (`(,_ . next)
       (let ((attachment (car (gomuks--send-remaining send))))
         (setf (gomuks--send-attachment send) attachment
               (gomuks--send-event send) nil (gomuks--send-content send) nil
               (gomuks--send-caption send)
               (if (cdr (gomuks--send-remaining send)) "" (gomuks--send-text send)))
         (if (not attachment) (gomuks--send-step send 'submit)
           (gomuks--send-request
            send 'uploading 'uploaded
            (lambda (complete)
              (gomuks--upload-file (car attachment) (gomuks--send-room send) complete))))))
      (`(,_ . submit)
       (gomuks--send-request
        send 'submitting 'accepted
        (lambda (complete)
          (gomuks--post
           "send_message"
           (gomuks--message-payload (gomuks--send-room send) (gomuks--send-caption send)
                                   (gomuks--send-relation send) (gomuks--send-content send))
           complete))))
      (`(uploading . uploaded)
       (if failure (gomuks--send-failed send failure)
         (when (gomuks--send-sticker send) (puthash "msgtype" "m.sticker" value))
         (setf (gomuks--send-content send) value)
         (gomuks--send-step send 'submit)))
      (`(submitting . accepted)
       (cond
        (failure (gomuks--send-failed
                  send failure (not (or (string-match-p "\\`HTTP [45][0-9][0-9]" failure)
                                        (string-prefix-p "request could not start:" failure)))))
        ((not (gomuks--alist 'rowid value))
         (gomuks--send-failed send "backend returned no pending event identity" t))
        (t
         (setf (gomuks--send-event send) value)
         (gomuks--send-record-event send value)
         (gomuks--set-send-phase send 'queued)
         (setf (gomuks--send-timer send)
               (run-at-time gomuks-transfer-timeout nil #'gomuks--send-step send
                            'timeout nil nil (gomuks--send-attempt send)))
         (if-let* ((completed (cl-find (gomuks--send-key value) gomuks--early-completions
                                      :key (lambda (data)
                                             (gomuks--send-key (gomuks--alist 'event data)))
                                      :test #'equal)))
             (progn
               (setq gomuks--early-completions (delq completed gomuks--early-completions))
               (gomuks--send-step send 'delivery completed))
           (when-let* ((id (gomuks--alist 'event_id value)))
             (unless (string-prefix-p "~" id)
               (gomuks--send-step send 'delivery `((event . ,value)))))))))
      ((and `(,phase . ,(or 'delivery 'stored-delivery))
            (guard (memq phase '(queued uncertain failed checking))))
       (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
       (let ((event (gomuks--alist 'event value))
             (error (or (gomuks--alist 'error value)
                        (gomuks--alist 'send_error (gomuks--alist 'event value))))
             (buffer (gomuks--send-buffer send)))
         (setf (gomuks--send-event send) event)
         (unless (eq action 'stored-delivery) (gomuks--send-record-event send event))
         (if error (gomuks--send-failed send error)
           (when-let* ((attachment (gomuks--send-attachment send)))
             (gomuks--delete-temp-attachments (list attachment))
             (setf (gomuks--send-remaining send) (cdr (gomuks--send-remaining send)))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq gomuks--compose-attachment (delq attachment gomuks--compose-attachment)))))
           (if (gomuks--send-remaining send) (gomuks--send-step send 'next)
             (gomuks--set-send-phase send 'delivered)
             (remhash (gomuks--send-id send) gomuks--sends)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (equal (gomuks--send-text send) (string-trim-right (buffer-string)))
                   (erase-buffer) (set-buffer-modified-p nil))
                 (when (eq gomuks--compose-job send) (setq gomuks--compose-job nil))))
             (message "gomuks: message delivered")))))
      (`(checking . checked)
       (cond
        (failure (gomuks--send-failed send failure t))
        ((and (stringp (gomuks--alist 'event_id value))
              (not (string-prefix-p "~" (gomuks--alist 'event_id value))))
         (gomuks--send-step send 'delivery `((event . ,value))))
        (t (gomuks--set-send-phase send 'failed) (gomuks--send-step send 'retry))))
      (`(,phase . retry)
       (unless (and (equal (gomuks--send-backend send) gomuks-backend-url)
                    (equal (gomuks--send-account send) gomuks--user-id))
         (user-error "This send belongs to a different account or backend"))
       (when (memq phase '(queued uploading submitting checking))
         (user-error "This send is still pending"))
       (cond
        ((eq phase 'uncertain)
         (unless (gomuks--send-event send)
           (user-error "Acceptance is unknown; inspect the room, then restore or discard this send"))
         (gomuks--send-request
          send 'checking 'checked
          (lambda (complete)
            (gomuks--post "get_event_by_rowid"
                          `((event_rowid . ,(gomuks--alist 'rowid (gomuks--send-event send))))
                          complete))))
        ((gomuks--alist 'transaction_id (gomuks--send-event send))
         (setf (gomuks--send-token send) (gomuks--cache-token (gomuks--send-room send)))
         (gomuks--send-request
          send 'submitting 'accepted
          (lambda (complete)
            (gomuks--post "resend_event"
                          `((transaction_id . ,(gomuks--alist 'transaction_id (gomuks--send-event send))))
                          complete))))
        ((gomuks--send-event send)
         (user-error "Backend supplied no transaction ID; restore this draft before resubmitting"))
        (t (gomuks--send-step send (if (gomuks--send-content send) 'submit 'next)))))
      ((and `(,phase . disconnect) (guard (memq phase '(queued submitting uploading checking))))
       (cl-incf (gomuks--send-attempt send))
       (gomuks--send-failed send "disconnected before delivery confirmation" t))
      (`(queued . timeout)
       (gomuks--send-failed send "delivery confirmation timed out" t))
      ((and `(,phase . start-failed) (guard (eq phase value)))
       (gomuks--send-failed send failure (eq phase 'checking))))))

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
    (gomuks--send-step send 'next)
    send))

(defun gomuks--complete-send (data)
  "Procedure: correlate a stream delivery DATA with its retained submission."
  (let* ((event (gomuks--alist 'event data)) (key (gomuks--send-key event)) found recorded)
    (when key
      (maphash (lambda (_id send)
                 (when (and (equal key (gomuks--send-key (gomuks--send-event send)))
                            (equal (gomuks--send-backend send) gomuks-backend-url)
                            (equal (gomuks--send-account send) gomuks--user-id))
                   (setq found send))) gomuks--sends))
    (setq recorded (gomuks--send-record-event found event t))
    (when key
      (if (and found (not (eq (gomuks--send-phase found) 'submitting)))
          (gomuks--send-step found (if recorded 'stored-delivery 'delivery) data)
        (push data gomuks--early-completions)
        (when (> (length gomuks--early-completions) 64)
          (setcdr (nthcdr 63 gomuks--early-completions) nil))))))

(defun gomuks--disconnect-sends ()
  "Make queued deliveries recoverable without sending anything automatically."
  (maphash (lambda (_id send) (gomuks--send-step send 'disconnect)) gomuks--sends)
  (setq gomuks--early-completions nil))

(defun gomuks--retry-send (send)
  "Procedure: retry SEND through its transition owner."
  (gomuks--send-step send 'retry))

(defun gomuks--choose-send (prompt &optional prefer-draft)
  "Procedure: choose a recoverable send with PROMPT, optionally PREFER-DRAFT."
  (let (choices)
    (maphash (lambda (_id send)
               (when (memq (gomuks--send-phase send) '(failed uncertain))
                 (push (cons (format "%d: %s · %s · %s" (gomuks--send-id send)
                                     (gomuks--room-name (gomuks--send-room send))
                                     (gomuks--send-phase send)
                                     (truncate-string-to-width (gomuks--send-text send) 50)) send)
                       choices))) gomuks--sends)
    (unless choices (user-error "No failed sends to recover"))
    (or (and prefer-draft (memq gomuks--compose-job (mapcar #'cdr choices))
             gomuks--compose-job)
        (cdr (assoc (completing-read prompt choices nil t) choices)))))

(defun gomuks-retry-send ()
  "Choose and retry a retained failed send without duplicating accepted events."
  (interactive)
  (gomuks--retry-send (gomuks--choose-send "Retry send: " t)))

(defun gomuks-restore-send ()
  "Restore a retained send snapshot to a draft, including its undelivered files."
  (interactive)
  (let* ((send (gomuks--choose-send "Restore draft: "))
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
    (pop-to-buffer buffer)))

(defun gomuks-discard-send ()
  "Forget a failed submission's delivery tracking after confirmation.
A live draft keeps its text and attachments; closed drafts release their
owned temporary files. Check the room before resubmitting an uncertain send."
  (interactive)
  (let ((send (gomuks--choose-send "Forget send tracking: " t)))
    (when (yes-or-no-p "Forget this send's tracking? Check delivery before submitting its draft again. ")
      (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
      (remhash (gomuks--send-id send) gomuks--sends)
      (if (buffer-live-p (gomuks--send-buffer send))
          (with-current-buffer (gomuks--send-buffer send)
            (setq gomuks--compose-job nil gomuks--compose-sending nil)
            (gomuks--update-composer-header))
        (gomuks--delete-temp-attachments (gomuks--send-remaining send))))))

(defun gomuks--cleanup-sends ()
  "Release temporary retry files when the Emacs session ends."
  (maphash (lambda (_id send)
             (when (gomuks--send-timer send) (cancel-timer (gomuks--send-timer send)))
             (gomuks--delete-temp-attachments (gomuks--send-remaining send))) gomuks--sends)
  (clrhash gomuks--sends))

(provide 'gomuks-send)
;;; gomuks-send.el ends here
