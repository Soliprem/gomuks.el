;;; gomuks-reactions.el --- Reaction query and removal workflows -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Reacting to messages, the reaction hover popup and the list of people
;; who used a reaction.

;;; Code:

(require 'gomuks-view)
(declare-function gomuks-reactions-mode "gomuks-ui" ())
(declare-function emoji--init "emoji" ())
(declare-function emoji--read-emoji "emoji" ())

(defun gomuks--reactions-with-key (reactions key)
  "Return the unredacted REACTIONS using KEY, at most one per sender."
  (let ((seen (make-hash-table :test 'equal)) found)
    (dolist (reaction reactions (nreverse found))
      (let ((sender (gomuks--alist 'sender reaction)))
        (when (and (stringp sender) (not (gethash sender seen))
                   (not (gomuks--alist 'redacted_by reaction))
                   (equal key (gomuks--dig (gomuks--event-content reaction)
                                           'm.relates_to 'key)))
          (puthash sender t seen)
          (push reaction found))))))

(defun gomuks--reaction-help-text (id event key count entry)
  "Describe EVENT's KEY reactions in room ID from cached ENTRY and COUNT."
  (gomuks--hover-text
   (concat
    (format "%s — %d reaction%s\n" key count (if (= count 1) "" "s"))
    (cond
     ((plist-get entry :pending) "Loading people who reacted…")
     ((plist-get entry :failure) (concat "Could not load reactions: " (plist-get entry :failure)))
     (t
      (let ((people (mapcar (lambda (reaction)
                              (gomuks--person-info id (gomuks--alist 'sender reaction)
                                                   (gomuks--alist 'timestamp reaction)))
                            (gomuks--reactions-with-key (plist-get entry :events) key))))
        (if people (string-join (sort people #'string-lessp) "\n") "No reactions found."))))
    "\nRET shows the reaction list; d there removes yours.")
   (list 'reaction (gomuks--alist 'event_id event) key)))

(defun gomuks--reaction-help (event key count)
  "Describe EVENT's KEY reactions, requesting people only on hover.
Coalesce requests for all reaction keys on a message in this view."
  (let* ((id gomuks--room-id) (event-id (gomuks--alist 'event_id event))
         (token (gomuks--cache-token id))
         (buffer (current-buffer)) (view (gomuks--view-identity))
         (counts (gomuks--alist 'reactions event))
         (entry (and gomuks--hover-reactions (gethash event-id gomuks--hover-reactions))))
    (unless (and entry (eq event (plist-get entry :event))
                 (equal token (plist-get entry :token)) (equal counts (plist-get entry :counts)))
      (unless gomuks--hover-reactions (setq gomuks--hover-reactions (make-hash-table :test 'equal)))
      (setq entry (list :event event :token token :counts (copy-tree counts)
                        :pending t :failure nil :events nil))
      (puthash event-id entry gomuks--hover-reactions)
      (let ((complete
             (lambda (failure response)
               (when (and (gomuks--cache-current-p id token) (gomuks--view-current-p buffer view))
                 (with-current-buffer buffer
                   (when (eq entry (gethash event-id gomuks--hover-reactions))
                     (setf (plist-get entry :pending) nil
                           (plist-get entry :failure) failure
                           (plist-get entry :events) response)
                     (unless failure
                       (gomuks--store-events response)
                       (gomuks--request-missing-members id response))
                     (when (and (eq (car gomuks--hover-target) 'reaction)
                                (equal (cadr gomuks--hover-target) event-id))
                       (let* ((hover-key (nth 2 gomuks--hover-target))
                              (hover-count (or (cdr (assoc-string hover-key counts)) count)))
                         (gomuks--show-hover-help
                          (gomuks--reaction-help-text id event hover-key hover-count entry))))))))))
        (if event-id
            (gomuks--post "get_related_events"
                          `((room_id . ,id) (event_id . ,event-id) (relation_type . "m.annotation"))
                          complete)
          (funcall complete "This message has no event ID yet" nil))))
    (gomuks--reaction-help-text id event key count entry)))

(defun gomuks--read-reaction ()
  "Read an emoji reaction by name, or raw text with a prefix argument."
  (if (and (not current-prefix-arg)
           (require 'emoji nil t)
           (fboundp 'emoji--read-emoji))
      (progn (emoji--init) (car (emoji--read-emoji)))
    (if current-prefix-arg
        (read-string "Reaction: ")
      (char-to-string (read-char-by-name "Reaction emoji: ")))))

(defun gomuks-react (key)
  "React to the message at point with KEY."
  (interactive (list (gomuks--read-reaction)))
  (when (string-empty-p (string-trim key))
    (user-error "Reaction is empty"))
  (let ((event-id (gomuks--alist 'event_id (gomuks--event-at-point))))
    (unless event-id (user-error "This message has no event ID yet"))
    (gomuks--post
     "send_event"
     `((room_id . ,gomuks--room-id) (type . "m.reaction")
       (content . ((m.relates_to . ((rel_type . "m.annotation")
                                    (event_id . ,event-id) (key . ,key))))))
     (lambda (failure _response)
       (message "gomuks: %s" (or failure "reaction queued"))))))

(defun gomuks-reactions-quit ()
  "Close the reaction list and return to the message."
  (interactive)
  (quit-window t))

(defun gomuks-remove-reaction ()
  "Redact your reaction on the current line."
  (interactive)
  (unless (derived-mode-p 'gomuks-reactions-mode)
    (user-error "Open a reaction list first"))
  (let ((event-id (get-text-property (point) 'gomuks-reaction-event-id))
        (sender (get-text-property (point) 'gomuks-reaction-sender))
        (room-id gomuks--room-id)
        (buffer (current-buffer))
        (generation (gomuks--cache-token gomuks--room-id))
        (view (gomuks--view-identity))
        (event (gomuks--reaction-target))
        (key gomuks--reactions-key))
    (unless event-id (user-error "No reaction on this line"))
    (unless (and gomuks--user-id (equal sender gomuks--user-id))
      (user-error "You can only remove your own reaction"))
    (when (yes-or-no-p (format "Remove your %s reaction? " key))
      (gomuks--post
       "redact_event" `((room_id . ,room-id) (event_id . ,event-id))
       (lambda (failure _response)
         (when (and (gomuks--cache-current-p room-id generation)
                    (gomuks--view-current-p buffer view))
           (if failure
               (message "gomuks: could not remove reaction: %s" failure)
             (when (and event key (buffer-live-p buffer))
               (with-current-buffer buffer
                 (gomuks-show-reactions event key))))))))))

(defun gomuks-show-reactions (event key)
  "Show the people who reacted to EVENT with KEY."
  (let* ((id gomuks--room-id)
         (event-id (gomuks--alist 'event_id event))
         (generation (gomuks--cache-token id))
         view
         (buffer (get-buffer-create
                  (format "*Gomuks reactions: %s %s*" event-id key))))
    (unless event-id (user-error "This message has no event ID yet"))
    (with-current-buffer buffer
      (gomuks-reactions-mode)
      (setq-local gomuks--room-id id
                  gomuks--reactions-event (or (gomuks--alist 'rowid event) event-id)
                  gomuks--reactions-key key)
      (setq view (gomuks--view-identity))
      (setq-local header-line-format
                  (concat " " (gomuks--room-name id) "  ·  " key
                          " reactions  ·  d remove yours  ·  q close"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading reactions…\n")))
    (select-window
     (display-buffer buffer '((display-buffer-below-selected)
                              (window-height . 10))))
    (let ((load
           (lambda ()
             (gomuks--post
              "get_related_events"
              `((room_id . ,id) (event_id . ,event-id)
                (relation_type . "m.annotation"))
              (lambda (failure response)
                (when (and (gomuks--cache-current-p id generation)
                           (gomuks--view-current-p buffer view))
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (if failure
                          (insert (propertize
                                   (format "Could not load reactions: %s\n" failure)
                                   'face 'error))
                        (gomuks--store-events response)
                        (let ((reactions (gomuks--reactions-with-key response key)))
                          (dolist (reaction reactions)
                            (let ((sender (gomuks--alist 'sender reaction))
                                  (start (point)))
                              (insert (gomuks--sender-name id reaction)
                                      "  " (propertize sender 'face 'shadow)
                                      (if (equal sender gomuks--user-id)
                                          "  [d remove]" "") "\n")
                              (add-text-properties
                               start (point)
                               (list 'gomuks-reaction-event-id (gomuks--alist 'event_id reaction)
                                     'gomuks-reaction-sender sender))))
                          (unless reactions
                            (insert "No reactions found.\n")))))
                    (goto-char (point-min)))))))))
      (if gomuks--user-id
          (funcall load)
        (gomuks--post
         "get_state" '()
         (lambda (_failure state)
           (when (and (gomuks--cache-current-p id generation)
                      (gomuks--view-current-p buffer view))
             (unless gomuks--user-id
               (setq gomuks--user-id (gomuks--alist 'user_id state)))
             (funcall load))))))))

(provide 'gomuks-reactions)
;;; gomuks-reactions.el ends here
