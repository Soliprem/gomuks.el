;;; gomuks-ui.el --- Modes and keymaps for gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Modes and key bindings for the gomuks client.

;;; Code:

(require 'gomuks-view)
(require 'gomuks-search)
(require 'gomuks-reactions)
(declare-function gomuks-quit "gomuks")
(declare-function gomuks-reconnect "gomuks")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(defvar gomuks-rooms-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gomuks-open-room)
    (define-key map (kbd "g") #'gomuks-reconnect)
    (define-key map (kbd "q") #'gomuks-quit)
    (define-key map (kbd "C-k") #'gomuks-switch-room)
    map)
  "Keymap for the Gomuks home page.")
(define-key gomuks-rooms-mode-map (kbd "m") #'gomuks-toggle-mute)
(define-key gomuks-rooms-mode-map (kbd "C-c C-h") #'gomuks-switch-hidden-room)
(define-key gomuks-rooms-mode-map (kbd "C-c C-x") #'gomuks-toggle-hidden-room)
(defvar gomuks-room-mode-map (make-sparse-keymap)
  "Keymap for room, thread, and reply context buffers.")
;; Bind after `defvar' so reloading updates the existing map in a daemon.
(define-key gomuks-room-mode-map (kbd "C-c C-s") #'gomuks-compose)
(define-key gomuks-room-mode-map (kbd "C-k") #'gomuks-switch-room)
(define-key gomuks-room-mode-map (kbd "C-c C-h") #'gomuks-switch-hidden-room)
(define-key gomuks-room-mode-map (kbd "C-c C-x") #'gomuks-toggle-hidden-room)
(define-key gomuks-room-mode-map (kbd "C-c C-a") #'gomuks-send-file)
(define-key gomuks-room-mode-map (kbd "C-c C-p") #'gomuks-send-sticker)
(define-key gomuks-room-mode-map (kbd "C-c C-g") #'gomuks-send-gif)
(define-key gomuks-room-mode-map (kbd "C-c C-o") #'gomuks-open-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-w") #'gomuks-save-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-SPC") #'gomuks-audio-toggle)
(define-key gomuks-room-mode-map (kbd "C-c <") #'gomuks-audio-backward)
(define-key gomuks-room-mode-map (kbd "C-c >") #'gomuks-audio-forward)
(define-key gomuks-room-mode-map (kbd "RET") #'gomuks-activate-at-point)
(define-key gomuks-room-mode-map (kbd "o") #'gomuks-open-attachment)
(define-key gomuks-room-mode-map (kbd "d") nil)
(define-key gomuks-room-mode-map (kbd "d d") #'gomuks-redact)
(define-key gomuks-room-mode-map (kbd "D") #'gomuks-save-attachment)
(define-key gomuks-room-mode-map (kbd "C-c C-r") #'gomuks-reply)
(define-key gomuks-room-mode-map (kbd "C-c C-e") #'gomuks-edit)
(define-key gomuks-room-mode-map (kbd "C-c C-+") #'gomuks-react)
(define-key gomuks-room-mode-map (kbd "C-c C-d") #'gomuks-redact)
(define-key gomuks-room-mode-map (kbd "C-c C-m") #'gomuks-mark-read)
(define-key gomuks-room-mode-map (kbd "C-c C-n") #'gomuks-toggle-mute)
(define-key gomuks-room-mode-map (kbd "C-c C-u") #'gomuks-copy-sender-id)
(define-key gomuks-room-mode-map (kbd "M-p") #'gomuks-load-history)
(define-key gomuks-room-mode-map (kbd "C-c C-t") #'gomuks-open-thread)
(define-key gomuks-room-mode-map (kbd "C-c C-j") #'gomuks-follow-reply)
(define-key gomuks-room-mode-map (kbd "C-c C-f") #'gomuks-search)
(define-key gomuks-room-mode-map (kbd "g") #'gomuks-reconnect)
(define-key gomuks-room-mode-map (kbd "b") #'gomuks-back)
(define-key gomuks-room-mode-map (kbd "q") #'gomuks-back)
(defvar gomuks-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'gomuks-compose-send)
    (define-key map (kbd "C-c C-k") #'gomuks-compose-leave)
    (define-key map (kbd "C-c C-a") #'gomuks-send-file)
    (define-key map (kbd "C-c C-d") #'gomuks-compose-remove-attachment)
    (define-key map (kbd "C-c C-o") #'gomuks-compose-preview-attachment)
    map)
  "Keymap for Gomuks message composer buffers.")
(define-key gomuks-compose-mode-map (kbd "C-c C-p") #'gomuks-send-sticker)
(define-key gomuks-compose-mode-map (kbd "C-c C-g") #'gomuks-send-gif)
(define-key gomuks-compose-mode-map (kbd "C-c C-e") #'gomuks-insert-emoji)
(define-key gomuks-compose-mode-map (kbd "C-c C-v") #'gomuks-paste-image)
(define-key gomuks-compose-mode-map (kbd "C-k") #'gomuks-switch-room)
(define-key gomuks-compose-mode-map (kbd "C-c C-h") #'gomuks-switch-hidden-room)
(define-key gomuks-compose-mode-map (kbd "C-c C-n") #'gomuks-toggle-mute)

;; Doom's `map!' uses the user's configured localleader and its insert-state
;; alternate.  Keep this optional so the package also loads in plain Emacs.
(when (fboundp 'map!)
  (eval
   '(map! :map gomuks-room-mode-map :localleader
          "s" #'gomuks-compose
          "a" #'gomuks-send-file
          "p" #'gomuks-send-sticker
          "g" #'gomuks-send-gif
          "o" #'gomuks-open-attachment
          "SPC" #'gomuks-audio-toggle
          "<" #'gomuks-audio-backward
          ">" #'gomuks-audio-forward
          "w" #'gomuks-save-attachment
          "r" #'gomuks-reply
          "e" #'gomuks-edit
          "+" #'gomuks-react
          "d" #'gomuks-redact
          "m" #'gomuks-mark-read
          "n" #'gomuks-toggle-mute
          "u" #'gomuks-copy-sender-id
          "t" #'gomuks-open-thread
          "j" #'gomuks-follow-reply
          "f" #'gomuks-search
          :map gomuks-compose-mode-map :localleader
          "c" #'gomuks-compose-send
          "k" #'gomuks-compose-leave
          "a" #'gomuks-send-file
          "d" #'gomuks-compose-remove-attachment
          "o" #'gomuks-compose-preview-attachment
          "p" #'gomuks-send-sticker
          "g" #'gomuks-send-gif
          "e" #'gomuks-insert-emoji
          "n" #'gomuks-toggle-mute
          "v" #'gomuks-paste-image)))
(define-key gomuks-room-mode-map (kbd "C-c C-y") #'gomuks-retry-send)
(define-key gomuks-compose-mode-map (kbd "C-c C-y") #'gomuks-retry-send)

(defvar gomuks-search-mode-map (make-sparse-keymap)
  "Keymap for Gomuks search results.")
(define-key gomuks-search-mode-map (kbd "RET") #'gomuks-search-open)
(define-key gomuks-search-mode-map (kbd "n") #'gomuks-search-more)
(define-key gomuks-search-mode-map (kbd "q") #'gomuks-search-back)
(define-key gomuks-search-mode-map (kbd "b") #'gomuks-search-back)
(define-key gomuks-search-mode-map (kbd "C-k") #'gomuks-switch-room)
(define-key gomuks-search-mode-map (kbd "C-c C-h") #'gomuks-switch-hidden-room)
(defvar gomuks-reactions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'gomuks-remove-reaction)
    (define-key map (kbd "q") #'gomuks-reactions-quit)
    (define-key map (kbd "b") #'gomuks-reactions-quit)
    map)
  "Keymap for the list of people who reacted to a message.")
(defvar gomuks-attachment-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gomuks-attachment-preview-open)
    (define-key map (kbd "d") #'gomuks-attachment-preview-remove)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for a staged attachment preview.")

(define-derived-mode gomuks-rooms-mode special-mode "Gomuks rooms"
  "Mode for the gomuks room list."
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-room-mode special-mode "Gomuks room"
  "Mode for a gomuks room timeline."
  (setq gomuks--view-generation (cl-incf gomuks--view-serial))
  (add-hook 'post-command-hook #'gomuks--maybe-mark-read nil t)
  (add-hook 'post-command-hook #'gomuks--refresh-visible-views nil t))
(define-derived-mode gomuks-compose-mode text-mode "Gomuks compose"
  "Mode for editing a Matrix message draft."
  (setq-local header-line-format
              " Compose a message  •  C-c C-c send  •  C-c C-a attach  •  C-c C-k return")
  (setq-local completion-ignore-case t)
  (add-hook 'kill-buffer-hook #'gomuks--cleanup-compose-attachment nil t)
  (add-hook 'post-command-hook #'gomuks--maybe-mark-read nil t)
  (add-hook 'completion-at-point-functions #'gomuks--mention-capf nil t)
  (visual-line-mode 1))
(define-derived-mode gomuks-search-mode special-mode "Gomuks search"
  "Mode for paginated Gomuks message search results."
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-reactions-mode special-mode "Gomuks reactions"
  "Mode for showing the people who used a reaction."
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-attachment-preview-mode special-mode "Gomuks attachment"
  "Mode for previewing an attachment staged in a draft.")

;; Evil's normal-state maps take precedence over the ordinary major-mode map.
;; Keep Vim motion keys intact; use keys that would edit a read-only buffer.
(with-eval-after-load 'evil
  (evil-set-initial-state 'gomuks-rooms-mode 'normal)
  (evil-set-initial-state 'gomuks-room-mode 'normal)
  (evil-set-initial-state 'gomuks-compose-mode 'insert)
  (evil-set-initial-state 'gomuks-search-mode 'normal)
  (evil-set-initial-state 'gomuks-reactions-mode 'normal)
  (evil-set-initial-state 'gomuks-attachment-preview-mode 'normal)
  (evil-define-key* 'normal gomuks-rooms-mode-map
		    (kbd "RET") #'gomuks-open-room
		    (kbd "o") #'gomuks-open-room
		    (kbd "m") #'gomuks-toggle-mute
		    (kbd "C-c C-h") #'gomuks-switch-hidden-room
		    (kbd "C-c C-x") #'gomuks-toggle-hidden-room
		    (kbd "C-k") #'gomuks-switch-room
		    (kbd "q") #'gomuks-quit
		    (kbd "g r") #'gomuks-reconnect)
  (evil-define-key* 'normal gomuks-room-mode-map
		    (kbd "C-k") #'gomuks-switch-room
		    (kbd "C-c C-h") #'gomuks-switch-hidden-room
		    (kbd "C-c C-x") #'gomuks-toggle-hidden-room
		    (kbd "i") #'gomuks-compose
		    (kbd "a") #'gomuks-send-file
		    (kbd "r") #'gomuks-reply
		    (kbd "E") #'gomuks-edit
		    (kbd "R") #'gomuks-react
		    (kbd "x") #'gomuks-redact
		    (kbd "M") #'gomuks-mark-read
		    (kbd "C-c C-n") #'gomuks-toggle-mute
		    (kbd "U") #'gomuks-copy-sender-id
		    (kbd "p") #'gomuks-load-history
		    (kbd "T") #'gomuks-open-thread
		    (kbd "J") #'gomuks-follow-reply
		    (kbd "C-c C-f") #'gomuks-search
		    (kbd "C-c C-p") #'gomuks-send-sticker
		    (kbd "C-c C-g") #'gomuks-send-gif
		    (kbd "o") #'gomuks-open-attachment
		    (kbd "d") nil
		    (kbd "d d") #'gomuks-redact
		    (kbd "D") #'gomuks-save-attachment
		    (kbd "RET") #'gomuks-activate-at-point
		    (kbd "b") #'gomuks-back
		    (kbd "q") #'gomuks-back
		    (kbd "g r") #'gomuks-reconnect)
  (evil-define-key* '(normal insert) gomuks-compose-mode-map
		    (kbd "C-k") #'gomuks-switch-room
		    (kbd "C-c C-h") #'gomuks-switch-hidden-room
		    (kbd "C-c C-c") #'gomuks-compose-send
		    (kbd "C-c C-k") #'gomuks-compose-leave
		    (kbd "C-c C-a") #'gomuks-send-file
		    (kbd "C-c C-d") #'gomuks-compose-remove-attachment
		    (kbd "C-c C-n") #'gomuks-toggle-mute
		    (kbd "C-c C-p") #'gomuks-send-sticker
		    (kbd "C-c C-g") #'gomuks-send-gif
		    (kbd "C-c C-e") #'gomuks-insert-emoji
		    (kbd "C-c C-v") #'gomuks-paste-image
		    (kbd "C-c C-o") #'gomuks-compose-preview-attachment)
  (evil-define-key* 'normal gomuks-compose-mode-map
		    (kbd "q") #'gomuks-compose-leave)
  (evil-define-key* '(normal motion) gomuks-search-mode-map
		    (kbd "C-k") #'gomuks-switch-room
		    (kbd "C-c C-h") #'gomuks-switch-hidden-room
		    (kbd "RET") #'gomuks-search-open
		    (kbd "n") #'gomuks-search-more
		    (kbd "q") #'gomuks-search-back
		    (kbd "b") #'gomuks-search-back)
  (evil-define-key* '(normal motion) gomuks-reactions-mode-map
		    (kbd "d") #'gomuks-remove-reaction
		    (kbd "q") #'gomuks-reactions-quit
		    (kbd "b") #'gomuks-reactions-quit)
  (evil-define-key* '(normal motion) gomuks-attachment-preview-mode-map
		    (kbd "RET") #'gomuks-attachment-preview-open
		    (kbd "d") #'gomuks-attachment-preview-remove
		    (kbd "q") #'quit-window))

(add-hook 'window-buffer-change-functions #'gomuks--refresh-visible-views)

(provide 'gomuks-ui)
;;; gomuks-ui.el ends here
