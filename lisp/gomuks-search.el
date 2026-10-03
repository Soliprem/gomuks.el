;;; gomuks-search.el --- Room search workflows -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Feature procedures capture room and view ownership before issuing effects.

;;; Code:

(require 'gomuks-view)
(declare-function gomuks-search-mode "gomuks-ui" ())

(defun gomuks--search-fetch (&optional more)
  "Procedure: request one search page, suppressing concurrent and repeated pages."
  (unless gomuks--search-loading
    (if (and more (not gomuks--search-next-batch))
        (message "gomuks: no more search results")
      (let* ((buffer (current-buffer)) (id gomuks--room-id)
             (query gomuks--search-query) (view (gomuks--view-identity))
             (serial (cl-incf gomuks--search-serial))
             (cache-token (gomuks--cache-token id))
             (token (and more gomuks--search-next-batch))
             (complete
              (lambda (failure response)
                (when (and (gomuks--cache-current-p id cache-token)
                           (gomuks--view-current-p buffer view))
                  (with-current-buffer buffer
                    (when (and (= serial gomuks--search-serial) gomuks--search-loading)
                      (setq gomuks--search-loading nil gomuks--search-error failure)
                      (unless failure
                        (gomuks--store-events (gomuks--alist 'events response))
                        (setq gomuks--search-results
                              (gomuks--event-rows (append (and more gomuks--search-results)
                                                          (gomuks--alist 'events response)))
                              gomuks--search-next-batch
                              (let ((next (gomuks--alist 'next_batch response)))
                                (and (stringp next) (not (string-empty-p next))
                                     (not (equal next token)) next))))
                      (gomuks--render-search)))))))
        (setq gomuks--search-loading t gomuks--search-error nil)
        (gomuks--render-search)
        (condition-case err
            (gomuks--post "search_local"
                          (append `((search_term . ,query) (room_ids . ,(vector id))
                                    (limit . 50) (sort_by_time . t))
                                  (when token `((next_batch . ,token)))) complete)
          (error (funcall complete (error-message-string err) nil)))))))

(defun gomuks-search ()
  "Search the backend's local history for messages in this room."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (let ((query (string-trim (read-string "Search room: " nil nil gomuks--search-query)))
        (source (if (derived-mode-p 'gomuks-search-mode)
                    gomuks--parent-buffer (current-buffer)))
        (id gomuks--room-id))
    (when (string-empty-p query) (user-error "Search query is empty"))
    (let ((buffer (get-buffer-create (format "*Gomuks search: %s*" id))))
      (with-current-buffer buffer
        (unless (derived-mode-p 'gomuks-search-mode)
          (gomuks-search-mode))
        (setq gomuks--room-id id
              gomuks--parent-buffer source
              gomuks--search-query query
              gomuks--search-results nil
              gomuks--search-next-batch nil
              gomuks--search-loading nil
              gomuks--search-error nil
              gomuks--search-generation (1+ gomuks--search-generation))
        (gomuks--render-search))
      (delete-other-windows)
      (switch-to-buffer buffer)
      (with-current-buffer buffer (gomuks--search-fetch)))))

(defun gomuks-search-more ()
  "Load the next page of results in this search buffer."
  (interactive)
  (gomuks--search-fetch t))

(defun gomuks-search-open ()
  "Open the search result at point with nearby messages."
  (interactive)
  (let* ((event (gomuks--event-at-point))
         (id gomuks--room-id)
         (target (gomuks--alist 'event_id event))
         (generation (gomuks--cache-token id))
         (view (gomuks--view-identity))
         (navigation (cl-incf gomuks--navigation-serial))
         (source (current-buffer)))
    (unless target (user-error "This search result has no event ID"))
    (gomuks--post
     "get_event_context" `((room_id . ,id) (event_id . ,target) (limit . 12))
     (lambda (failure response)
       (when (and (gomuks--cache-current-p id generation)
                  (gomuks--navigation-current-p source view navigation))
         (if failure
             (gomuks--show-reply-context id target source (list (or (gethash (gomuks--alist 'rowid event) gomuks--events) event)))
           (gomuks--store-events (gomuks--alist 'related_events response))
           (gomuks--show-reply-context
            id target source
            (append (reverse (gomuks--alist 'before response))
                    (list (gomuks--alist 'event response))
                    (gomuks--alist 'after response)))))))))

(defun gomuks-search-back ()
  "Return from search results to the room that opened them."
  (interactive)
  (if (buffer-live-p gomuks--parent-buffer)
      (gomuks--show-room gomuks--parent-buffer)
    (gomuks-home)))

(provide 'gomuks-search)
;;; gomuks-search.el ends here
