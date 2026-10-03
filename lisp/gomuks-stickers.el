;;; gomuks-stickers.el --- Synced Matrix sticker packs -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Browse room image packs and the user's subscribed packs, then send existing media.

;;; Code:

(require 'gomuks-view)
(require 'outline)

(defvar-local gomuks--sticker-source nil "Buffer that opened this picker.")
(defvar-local gomuks--sticker-source-view nil "Identity of the source view.")
(defvar-local gomuks--sticker-token nil "Room cache lifetime owning this picker.")
(defvar-local gomuks--sticker-loading 0 "Number of pending pack requests.")
(defvar-local gomuks--sticker-errors nil "Failures from the latest pack requests.")
(defvar-local gomuks--expanded-sticker-packs nil "Keys of the expanded packs in this picker.")

(defvar gomuks-stickers-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    map)
  "Button keymap that keeps the picker's folding and navigation bindings.")

(defun gomuks--sticker-pack-keys (id)
  "Return unique (ROOM-ID . STATE-KEY) packs available in room ID."
  (let ((seen (make-hash-table :test 'equal)) keys
        (subscriptions (gomuks--alist
                        'rooms (gomuks--alist
                                'content (or (gethash "m.image_pack.rooms" gomuks--account-data)
                                             (gethash "im.ponies.emote_rooms" gomuks--account-data))))))
    (dolist (room subscriptions)
      (dolist (pack (cdr room))
        (let ((key (cons (gomuks--key-string (car room))
                         (gomuks--key-string (car pack)))))
          (unless (gethash key seen)
            (puthash key t seen) (push key keys)))))
    (when-let* ((state (gethash id gomuks--image-pack-state)))
      (maphash (lambda (key _row)
                 (let ((pack (cons id (cdr key))))
                   (unless (gethash pack seen)
                     (puthash pack t seen) (push pack keys)))) state))
    (nreverse keys)))

(defun gomuks--sticker-packs (id)
  "Query sticker content from room ID's available image packs.
Each result is (PACK-KEY PACK-NAME . CONTENTS).
PACK-KEY is (ROOM-ID . STATE-KEY).  Image usage overrides pack usage."
  (let (packs)
    (dolist (key (gomuks--sticker-pack-keys id))
      (let* ((state (gethash (car key) gomuks--image-pack-state))
             (row (and state (or (gethash (cons "m.room.image_pack" (cdr key)) state)
                                 (gethash (cons "im.ponies.room_emotes" (cdr key)) state))))
             (event (and row (gethash row gomuks--events)))
             (content (unless (gomuks--alist 'redacted_by event)
                        (gomuks--event-content event)))
             (pack (gomuks--alist 'pack content))
             (name (or (gomuks--alist 'display_name pack)
                       (if (string-empty-p (cdr key)) (gomuks--room-name (car key))
                         (format "%s: %s" (gomuks--room-name (car key)) (cdr key)))))
             (seen (make-hash-table :test 'equal)) stickers)
        (dolist (entry (gomuks--alist 'images content))
          (let* ((image (cdr entry))
                 (url (gomuks--alist 'url image))
                 (usage (or (gomuks--alist 'usage image) (gomuks--alist 'usage pack))))
            (when (and (stringp url) (string-prefix-p "mxc://" url)
                       (or (null usage) (member "sticker" usage))
                       (not (gethash url seen)))
              (puthash url t seen)
              (push (append `((msgtype . "m.sticker")
                              (body . ,(or (gomuks--alist 'body image)
                                           (gomuks--key-string (car entry))))
                              (url . ,url))
                            (when-let* ((info (gomuks--alist 'info image)))
                              `((info . ,info))))
                    stickers))))
        (when stickers (push (cons key (cons name (nreverse stickers))) packs))))
    (sort packs (lambda (a b) (string-lessp (cadr a) (cadr b))))))

(defun gomuks--sticker-source-current-p ()
  "Return non-nil while this picker still belongs to its source room view."
  (and (gomuks--cache-current-p gomuks--room-id gomuks--sticker-token)
       (gomuks--view-current-p gomuks--sticker-source gomuks--sticker-source-view)))

(defun gomuks--send-picked-sticker (content)
  "Send existing sticker CONTENT into this picker's source, preserving its draft."
  (unless (gomuks--sticker-source-current-p)
    (user-error "The source room changed; reopen the sticker picker"))
  (with-current-buffer gomuks--sticker-source
    (when (equal (gomuks--alist 'rel_type gomuks--compose-relation) "m.replace")
      (user-error "Leave the edit draft before sending a sticker"))
    (gomuks--send-related
     "" (or gomuks--compose-relation
            (when gomuks--thread-root
              `((rel_type . "m.thread") (event_id . ,gomuks--thread-root))))
     content))
  (quit-window))

(defun gomuks--render-stickers ()
  "Render this picker's cached catalogue without starting requests."
  (let ((inhibit-read-only t)
        (position (point))
        (windows (mapcar (lambda (window) (cons window (window-start window)))
                         (get-buffer-window-list (current-buffer) nil t)))
        (packs (and (gomuks--sticker-source-current-p)
                    (gomuks--sticker-packs gomuks--room-id))))
    (outline-show-all)
    (erase-buffer)
    (insert (propertize "STICKERS" 'face 'gomuks-heading-face)
            "  ·  " (gomuks--room-name gomuks--room-id)
            "\nTAB fold/next   RET fold/send   n/p next/previous   "
            (if (bound-and-true-p evil-mode) "g r" "g")
            " refresh   q close\n\n")
    (cond
     ((not (gomuks--sticker-source-current-p))
      (insert "The source room changed; close and reopen the picker.\n"))
     ((> gomuks--sticker-loading 0) (insert "Loading sticker packs…\n\n"))
     ((not packs) (insert "No sticker packs available. Add packs in a Matrix client, then refresh.\n")))
    (dolist (failure gomuks--sticker-errors)
      (insert (propertize (concat failure "\n") 'face 'error)))
    (dolist (pack packs)
      (let ((start (point))
            (expanded (member (car pack) gomuks--expanded-sticker-packs)))
        (insert "* ")
        (insert-text-button
         (format "%s %s (%d)" (if expanded "▾" "▸")
                 (replace-regexp-in-string "[\n\r]+" " " (cadr pack)) (length (cddr pack)))
         'face 'gomuks-heading-face 'follow-link t 'keymap gomuks-stickers-button-map
         'action (lambda (button)
                   (goto-char (button-start button))
                   (gomuks-stickers-toggle-pack)))
        (put-text-property start (point) 'gomuks-sticker-pack-key (car pack))
        (insert "\n\n"))
      (dolist (content (cddr pack))
        (let* ((start (point))
               (name (replace-regexp-in-string "[\n\r]+" " " (gomuks--alist 'body content)))
               (attachment (gomuks--attachment content))
               (cached (gomuks--cached-media attachment))
               (preview (when (and (member (car pack) gomuks--expanded-sticker-packs)
                                   gomuks-inline-images cached (display-images-p)
                                   (gomuks--preview-file-p cached))
                          (condition-case nil
                              (create-image cached nil nil :max-width 128 :max-height 128)
                            (error nil))))
               (action (lambda (_button) (gomuks--send-picked-sticker content))))
          (insert "  ")
          (when (and gomuks-inline-images (display-images-p))
            (insert-text-button "▧" 'display preview 'follow-link t
                                'help-echo name 'action action 'keymap gomuks-stickers-button-map)
            (insert "  "))
          (insert-text-button name 'follow-link t 'action action 'keymap gomuks-stickers-button-map)
          (insert "\n\n")
          (put-text-property start (- (point) 2) 'gomuks-sticker-content content))))
    (outline-hide-body)
    (goto-char (point-min))
    (while (re-search-forward outline-regexp nil t)
      (when (member (get-text-property (line-beginning-position) 'gomuks-sticker-pack-key)
                    gomuks--expanded-sticker-packs)
        (outline-show-subtree)))
    (goto-char (min position (point-max)))
    (dolist (entry windows)
      (when (window-live-p (car entry))
        (set-window-start (car entry) (min (cdr entry) (point-max)) t)))))

(defun gomuks--preview-visible-stickers (&rest _)
  "Schedule bounded previews for the stickers visible in this picker."
  (when (and (derived-mode-p 'gomuks-stickers-mode) (gomuks--sticker-source-current-p))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let ((position (window-start window)) (end (window-end window t)))
        (while (< position end)
          (if (invisible-p position)
              (setq position (next-single-char-property-change position 'invisible nil end))
            (when-let* ((content (get-text-property position 'gomuks-sticker-content)))
              (gomuks--maybe-preview-image (gomuks--attachment content) gomuks--room-id))
            (setq position (next-single-property-change position 'gomuks-sticker-content nil end))))))))

(defun gomuks-stickers-toggle-pack ()
  "Expand or collapse the pack at point, loading previews when expanded."
  (interactive)
  (outline-back-to-heading t)
  (let ((key (get-text-property (line-beginning-position) 'gomuks-sticker-pack-key)))
    (if (member key gomuks--expanded-sticker-packs)
        (setq gomuks--expanded-sticker-packs (delete key gomuks--expanded-sticker-packs))
      (push key gomuks--expanded-sticker-packs)))
  (forward-char 2)
  (gomuks--render-stickers)
  (gomuks--preview-visible-stickers))

(defun gomuks-stickers-activate ()
  "Toggle a pack heading or send the sticker at point."
  (interactive)
  (if (outline-on-heading-p t) (gomuks-stickers-toggle-pack)
    (gomuks-activate-at-point)))

(defun gomuks-stickers-next-button (&optional count)
  "Move to the next visible pack or sticker, skipping folded content.
With a negative COUNT, move backwards."
  (interactive "p")
  (let ((origin (point)) first)
    (catch 'done
      (while (forward-button (if (< (or count 1) 0) -1 1) t nil t)
        (when (eq (point) first)
          (goto-char origin)
          (throw 'done nil))
        (unless first (setq first (point)))
        (unless (invisible-p (point)) (throw 'done t)))
      (goto-char origin))))

(defun gomuks-stickers-previous-button ()
  "Move to the previous visible pack or sticker."
  (interactive)
  (gomuks-stickers-next-button -1))

(defun gomuks-stickers-tab ()
  "Toggle a pack heading, or move to the next visible sticker."
  (interactive)
  (if (outline-on-heading-p t) (gomuks-stickers-toggle-pack)
    (gomuks-stickers-next-button)))

(defun gomuks--refresh-sticker-pickers (&optional id fetch)
  "Refresh visible sticker pickers, optionally only for source room ID.
FETCH reloads pack metadata after a subscription change."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'gomuks-stickers-mode)
                 (or (null id) (equal id gomuks--room-id))
                 (get-buffer-window buffer t))
        (if (and fetch (gomuks--sticker-source-current-p))
            (gomuks-stickers-refresh)
          (gomuks--render-stickers)
          (gomuks--preview-visible-stickers))))))

(defun gomuks-stickers-refresh ()
  "Fetch room packs and packs subscribed to from other Matrix clients."
  (interactive)
  (unless (gomuks--sticker-source-current-p)
    (user-error "The source room changed; reopen the sticker picker"))
  (let* ((buffer (current-buffer)) (id gomuks--room-id)
         (token gomuks--sticker-token)
         (serial (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
         (requests (list (list "get_room_state" `((room_id . ,id)))))
         (pack-state (make-hash-table :test 'equal))
         keys room-tokens)
    (maphash (lambda (room state)
               (maphash (lambda (key row)
                          (puthash (cons room key) (cons row (gethash row gomuks--events))
                                   pack-state)) state))
             gomuks--image-pack-state)
    (dolist (key (gomuks--sticker-pack-keys id))
      (unless (equal (car key) id)
        (push (cons (car key) (gomuks--cache-token (car key))) room-tokens)
        (dolist (type '("m.room.image_pack" "im.ponies.room_emotes"))
          (push `((room_id . ,(car key)) (type . ,type) (state_key . ,(cdr key))) keys))))
    (when keys
      (push (list "get_specific_room_state" `((keys . ,(vconcat (nreverse keys))))) requests))
    (setq gomuks--sticker-loading (length requests) gomuks--sticker-errors nil)
    (gomuks--render-stickers)
    (dolist (request requests)
      (let ((complete
             (lambda (failure events)
               (when (and (buffer-live-p buffer) (gomuks--cache-current-p id token)
                          (= serial (buffer-local-value 'gomuks--view-generation buffer)))
                 (with-current-buffer buffer
                   (when (gomuks--sticker-source-current-p)
                     (cl-decf gomuks--sticker-loading)
                     (if failure (push (concat "Could not load packs: " failure) gomuks--sticker-errors)
                       (gomuks--store-image-packs
                        (cl-remove-if-not
                         (lambda (event)
                           (let* ((room (gomuks--alist 'room_id event))
                                  (key (cons (gomuks--alist 'type event)
                                             (or (gomuks--alist 'state_key event) "")))
                                  (state (gethash room gomuks--image-pack-state))
                                  (row (and state (gethash key state))))
                             (and (or (equal room id)
                                      (when-let* ((owner (assoc room room-tokens)))
                                        (gomuks--cache-current-p room (cdr owner))))
                                  (equal (gethash (cons room key) pack-state)
                                         (and row (cons row (gethash row gomuks--events)))))))
                         events)))
                     (gomuks--render-stickers)
                     (gomuks--preview-visible-stickers)))))))
        (condition-case err
            (gomuks--post (car request) (cadr request) complete)
          (error (funcall complete (error-message-string err) nil)))))))

(defvar gomuks-stickers-mode-map (make-sparse-keymap)
  "Keymap for selecting a synced Matrix sticker.")
(define-key gomuks-stickers-mode-map (kbd "RET") #'gomuks-stickers-activate)
(define-key gomuks-stickers-mode-map (kbd "TAB") #'gomuks-stickers-tab)
(define-key gomuks-stickers-mode-map (kbd "<backtab>") #'gomuks-stickers-previous-button)
(define-key gomuks-stickers-mode-map (kbd "n") #'gomuks-stickers-next-button)
(define-key gomuks-stickers-mode-map (kbd "p") #'gomuks-stickers-previous-button)
(define-key gomuks-stickers-mode-map (kbd "g") #'gomuks-stickers-refresh)
(define-key gomuks-stickers-mode-map (kbd "q") #'quit-window)
(define-key gomuks-stickers-button-map (kbd "RET") #'gomuks-stickers-activate)
(define-key gomuks-stickers-button-map (kbd "TAB") #'gomuks-stickers-tab)
(define-key gomuks-stickers-button-map (kbd "<backtab>") #'gomuks-stickers-previous-button)

(define-derived-mode gomuks-stickers-mode special-mode "Gomuks stickers"
  "Browse folded sticker packs, expanding headings with TAB or RET.
Use n and p to navigate visible buttons, and RET to send a sticker."
  (setq-local outline-regexp "^\\* ")
  (setq-local outline-level (lambda () 1))
  (outline-minor-mode 1)
  (add-hook 'post-command-hook #'gomuks--preview-visible-stickers nil t)
  (add-hook 'window-scroll-functions
            (lambda (window _start)
              (with-current-buffer (window-buffer window)
                (gomuks--preview-visible-stickers))) nil t))

(defun gomuks-pick-sticker ()
  "Open a picker for this room's packs and the user's subscribed stickers."
  (interactive)
  (unless gomuks--room-id (user-error "Open a room first"))
  (let ((source (current-buffer)) (id gomuks--room-id)
        (view (gomuks--view-identity)) (token (gomuks--cache-token gomuks--room-id))
        (buffer (get-buffer-create (format "*Gomuks stickers: %s*" gomuks--room-id))))
    (with-current-buffer buffer
      (gomuks-stickers-mode)
      (setq gomuks--room-id id gomuks--sticker-source source
            gomuks--sticker-source-view view gomuks--sticker-token token))
    (pop-to-buffer buffer)
    (gomuks-stickers-refresh)))

(provide 'gomuks-stickers)
;;; gomuks-stickers.el ends here
