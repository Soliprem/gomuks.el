;;; gomuks-core.el --- Shared definitions for gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-
;;
;; Copyright (C) 2026 Francesco Prem Solidoro
;;
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Maintainer: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Created: settembre 30, 2026
;; Modified: settembre 30, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/Soliprem/gomuks.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'browse-url)
(require 'json)
(require 'dom)
(require 'shr)
(require 'subr-x)
(require 'url)
(require 'url-http)
(defgroup gomuks nil "Frontend for the gomuks Matrix backend." :group 'applications)
(defcustom gomuks-backend-url "http://localhost:29325"
  "Base URL of the gomuks backend."
  :type 'string :group 'gomuks)
(defcustom gomuks-username nil
  "Backend basic-auth username; nil uses auth-source or prompts."
  :type '(choice (const nil) string) :group 'gomuks)
(defcustom gomuks-reconnect-delay 5
  "Seconds before reconnecting after the event stream closes."
  :type 'number :group 'gomuks)
(defcustom gomuks-desktop-notifications t
  "Show desktop notifications for events flagged by the backend."
  :type 'boolean :group 'gomuks)
(defcustom gomuks-echo-area-notifications 'in-gomuks
  "When to show incoming message notifications in the Emacs echo area.
The default shows them only while a Gomuks buffer is selected."
  :type '(choice (const :tag "Only while in Gomuks" in-gomuks)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'gomuks)
(defcustom gomuks-inline-images t
  "Show small image attachments inside room timelines."
  :type 'boolean :group 'gomuks)
(defcustom gomuks-inline-image-max-bytes (* 4 1024 1024)
  "Largest image automatically downloaded for an inline preview."
  :type 'integer :group 'gomuks)
(defcustom gomuks-audio-backend 'empv
  "Use EMPV or EMMS to play audio attachments."
  :type '(choice (const empv) (const emms)) :group 'gomuks)

(defface gomuks-title-face
  '((t :inherit bold :height 1.6)) "Home page title." :group 'gomuks)
(defface gomuks-heading-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Section headings." :group 'gomuks)
(defface gomuks-room-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Room names." :group 'gomuks)
(defface gomuks-sender-face
  '((t :inherit font-lock-type-face :weight bold))
  "Message senders." :group 'gomuks)
(defface gomuks-unread-face
  '((t :inherit warning :weight bold))
  "Unread counters." :group 'gomuks)

(defvar gomuks--password nil
  "Password used for backend basic authentication.")
(defvar gomuks--user-id nil
  "Matrix user ID reported by the backend.")
(defvar gomuks--skip-auth-source nil
  "Non-nil after manually overriding stored credentials this session.")
(defvar gomuks--stream nil
  "Process receiving the Gomuks event stream.")
(defvar gomuks--reconnect-timer nil
  "Timer scheduled to reconnect the event stream.")
(defvar gomuks--pending ""
  "Incomplete line currently buffered from the event stream.")
(defvar gomuks--rooms (make-hash-table :test 'equal)
  "Room metadata keyed by room ID.")
(defvar gomuks--events (make-hash-table :test 'equal)
  "Cached events keyed by backend row ID.")
(defvar gomuks--member-state (make-hash-table :test 'equal)
  "Member event row IDs keyed by room ID and sender ID.")
(defvar gomuks--requested-members (make-hash-table :test 'equal)
  "Member profiles already requested from the backend.")
(defvar gomuks--mention-members-loaded (make-hash-table :test 'equal)
  "Rooms whose member list has been requested for mention completion.")
(defvar gomuks--timelines (make-hash-table :test 'equal)
  "Timeline event row IDs keyed by room ID.")
(defvar gomuks--timeline-ids (make-hash-table :test 'equal)
  "Timeline row IDs keyed by event row ID.")
(defvar gomuks--last-read (make-hash-table :test 'equal)
  "Last read receipt event ID sent for each room.")
(defvar gomuks--previous-window-config nil
  "Window configuration saved before opening the home page.")
(defvar gomuks--connection-status "Disconnected"
  "Status text displayed on the home page.")
(defvar gomuks--media-cache (make-hash-table :test 'equal)
  "Downloaded media paths keyed by attachment identity.")
(defvar gomuks--media-pending (make-hash-table :test 'equal)
  "Callbacks waiting for each in-progress media download.")
(defvar gomuks--media-failures (make-hash-table :test 'equal)
  "Attachments whose automatic preview download failed.")
(defvar-local gomuks--room-id nil
  "Room ID associated with the current Gomuks buffer.")
(defvar-local gomuks--reactions-event nil
  "Message event whose reactions this buffer shows.")
(defvar-local gomuks--reactions-key nil
  "Reaction key shown in this buffer.")
(defvar-local gomuks--initial-history-requested nil
  "Non-nil after requesting the first page for this room buffer.")
(defvar-local gomuks--thread-root nil
  "Thread root event ID shown by the current buffer.")
(defvar-local gomuks--thread-events nil
  "Events shown in the current thread buffer.")
(defvar-local gomuks--thread-next-batch nil
  "Pagination token for older events in the current thread.")
(defvar-local gomuks--context-target nil
  "Reply target event ID shown by the current context buffer.")
(defvar-local gomuks--context-events nil
  "Events shown around the current reply target.")
(defvar-local gomuks--parent-buffer nil
  "Buffer to return to from a thread or reply context.")
(defvar-local gomuks--compose-relation nil
  "Matrix relation attached to the current draft.")
(defvar-local gomuks--compose-context nil
  "Label describing the current draft's reply or edit target.")
(defvar-local gomuks--compose-sending nil
  "Non-nil while the current draft is being sent.")
(defvar-local gomuks--compose-attachment nil
  "Pending attachments as a list of (FILE TEMPORARY LABEL) entries.")
(defvar-local gomuks--compose-source nil
  "Room buffer associated with the current composer.")
(defvar-local gomuks--preview-source nil
  "Composer buffer associated with this attachment preview.")
(defvar-local gomuks--preview-attachment nil
  "Attachment entry displayed in this preview.")
(defvar-local gomuks--search-query nil
  "Query displayed by the current search buffer.")
(defvar-local gomuks--search-results nil
  "Events displayed by the current search buffer.")
(defvar-local gomuks--search-next-batch nil
  "Token for the next page of search results.")
(defvar-local gomuks--search-loading nil
  "Non-nil while a search request is pending.")
(defvar-local gomuks--search-error nil
  "Most recent backend search error in this buffer.")
(defvar-local gomuks--search-generation 0
  "Serial number used to ignore stale search responses.")

(defun gomuks--alist (key object)
  "Return the value of KEY in alist OBJECT, comparing keys with `equal'."
  (alist-get key object nil nil #'equal))

(defun gomuks--key-string (key)
  "Return KEY as a string when it is a symbol."
  (if (symbolp key) (symbol-name key) key))

(defun gomuks--room-name (id)
  "Return the display name of room ID, or ID when unnamed."
  (or (gomuks--alist 'name (gethash id gomuks--rooms)) id))

(defun gomuks--event-content (event)
  "Return the decrypted content of EVENT when available."
  (or (gomuks--alist 'decrypted event)
      (gomuks--alist 'content event)))

(defun gomuks--reply-target (event)
  "Return the event ID that EVENT replies to, if any."
  (gomuks--alist
   'event_id
   (gomuks--alist 'm.in_reply_to
                   (gomuks--alist 'm.relates_to
                                   (gomuks--event-content event)))))

(defun gomuks--thread-root-id (event)
  "Return the root ID for EVENT's thread, or EVENT's own ID."
  (if (equal (gomuks--alist 'relation_type event) "m.thread")
      (gomuks--alist 'relates_to event)
    (gomuks--alist 'event_id event)))

(defun gomuks--merge-thread-events (root old new)
  "Merge OLD and NEW thread events, with ROOT first and no duplicates."
  (let ((seen (make-hash-table :test 'equal)) merged)
    (dolist (event (append new old))
      (when-let* ((id (gomuks--alist 'event_id event)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push event merged))))
    (sort merged
          (lambda (a b)
            (let ((a-id (gomuks--alist 'event_id a))
                  (b-id (gomuks--alist 'event_id b)))
              (cond ((equal a-id root) t)
                    ((equal b-id root) nil)
                    (t (< (or (gomuks--alist 'timestamp a) 0)
                          (or (gomuks--alist 'timestamp b) 0)))))))))

(defun gomuks--find-event (room-id event-id)
  "Find EVENT-ID in ROOM-ID among cached events."
  (catch 'found
    (maphash (lambda (_rowid event)
               (when (and (equal (gomuks--alist 'room_id event) room-id)
                          (equal (gomuks--alist 'event_id event) event-id))
                 (throw 'found event)))
             gomuks--events)
    nil))

(defun gomuks--sender-name (id event)
  "Return EVENT's display name in room ID, falling back to its MXID localpart."
  (let* ((sender (gomuks--alist 'sender event))
         (profile (gomuks--alist 'com.beeper.per_message_profile
                                 (gomuks--event-content event)))
         (state (gethash id gomuks--member-state))
         (rowid (and sender state (gethash sender state)))
         (member (and rowid (gethash rowid gomuks--events)))
         (name (or (gomuks--alist 'displayname profile)
                   (gomuks--alist 'displayname (gomuks--event-content member)))))
    (if (and (stringp name) (not (string-empty-p (string-trim name))))
        (replace-regexp-in-string "[\n\r]+" " " (string-trim name))
      (if (and (stringp sender)
               (string-match "\\`@\\([^:]+\\):" sender))
          (match-string 1 sender)
        (or sender "?")))))



(provide 'gomuks-core)
;;; gomuks-core.el ends here
