;;; gomuks-ui.el --- Modes and keymaps for gomuks -*- lexical-binding: t; package-lint-main-file: "../gomuks.el"; -*-

;; Copyright (C) 2026 Francesco Prem Solidoro
;; Author: Francesco Prem Solidoro <francesco.solidoro@studio.unibo.it>
;; Keywords: comm
;; URL: https://github.com/Soliprem/gomuks.el

;;; Commentary:
;; Major modes, and one table holding every key binding: plain Emacs,
;; Evil and Doom's localleader.

;;; Code:

(require 'gomuks-view)
(require 'gomuks-search)
(require 'gomuks-reactions)
(require 'gomuks-stickers)
(declare-function gomuks-reconnect "gomuks")
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key* "evil-core")

(defvar gomuks-rooms-mode-map (make-sparse-keymap)
  "Keymap for the Gomuks home page.")
(defvar gomuks-room-mode-map (make-sparse-keymap)
  "Keymap for room, thread, and reply context buffers.")
(defvar gomuks-compose-mode-map (make-sparse-keymap)
  "Keymap for Gomuks message composer buffers.")
(defvar gomuks-search-mode-map (make-sparse-keymap)
  "Keymap for Gomuks search results.")
(defvar gomuks-reactions-mode-map (make-sparse-keymap)
  "Keymap for the list of people who reacted to a message.")
(defvar gomuks-attachment-preview-mode-map (make-sparse-keymap)
  "Keymap for a staged attachment preview.")

(defconst gomuks--bindings
  '((gomuks-rooms-mode-map normal
     ("RET" gomuks-open-room t)
     (nil gomuks-open-room "o")
     ("m" gomuks-toggle-mute t)
     ("C-k" gomuks-switch-room t)
     ("C-c C-h" gomuks-switch-hidden-room t)
     ("C-c C-x" gomuks-toggle-hidden-room t)
     ("g" gomuks-reconnect "g r")
     ("q" gomuks-quit t))
    (gomuks-room-mode-map normal
     ("C-c C-s" gomuks-compose "i" "s")
     ("C-k" gomuks-switch-room t)
     ("C-c C-h" gomuks-switch-hidden-room t)
     ("C-c C-x" gomuks-toggle-hidden-room t)
     ("C-c C-a" gomuks-send-file "a" "a")
     ("C-c C-p" gomuks-pick-sticker t "p")
     ("C-c C-g" gomuks-send-gif t "g")
     ("C-c C-i" gomuks-toggle-inline-images t "i")
     ("C-c C-o" gomuks-open-attachment nil "o")
     ("o" gomuks-open-attachment t)
     ("C-c C-w" gomuks-save-attachment nil "w")
     ("D" gomuks-save-attachment t)
     ("C-c C-SPC" gomuks-audio-toggle nil "SPC")
     ("C-c <" gomuks-audio-backward nil "<")
     ("C-c >" gomuks-audio-forward nil ">")
     ("RET" gomuks-activate-at-point t)
     ("C-c C-r" gomuks-reply "r" "r")
     ("C-c C-e" gomuks-edit "E" "e")
     ("C-c C-+" gomuks-react "R" "+")
     ("C-c C-d" gomuks-redact "x" "d")
     ("d d" gomuks-redact t)
     ("C-c C-m" gomuks-mark-read "M" "m")
     ("C-c C-n" gomuks-toggle-mute t "n")
     ("C-c C-u" gomuks-copy-sender-id "U" "u")
     ("M-p" gomuks-load-history "p")
     ("C-c C-t" gomuks-open-thread "T" "t")
     ("C-c C-j" gomuks-follow-reply "J" "j")
     ("C-c C-f" gomuks-search t "f")
     ("C-c C-y" gomuks-retry-send)
     ("g" gomuks-reconnect "g r")
     ("b" gomuks-back t)
     ("q" gomuks-back t))
    (gomuks-compose-mode-map (normal insert)
     ("C-c C-c" gomuks-compose-send t "c")
     ("C-c C-k" gomuks-compose-leave t "k")
     ("C-c C-a" gomuks-send-file t "a")
     ("C-c C-d" gomuks-compose-remove-attachment t "d")
     ("C-c C-o" gomuks-compose-preview-attachment t "o")
     ("C-c C-p" gomuks-pick-sticker t "p")
     ("C-c C-g" gomuks-send-gif t "g")
     ("C-c C-i" gomuks-toggle-inline-images t "i")
     ("C-c C-e" gomuks-insert-emoji t "e")
     ("C-c C-v" gomuks-paste-image t "v")
     ("C-c C-n" gomuks-toggle-mute t "n")
     ("C-c C-y" gomuks-retry-send)
     ("C-k" gomuks-switch-room t)
     ("C-c C-h" gomuks-switch-hidden-room t))
    (gomuks-compose-mode-map normal
     (nil gomuks-compose-leave "q"))
    (gomuks-search-mode-map (normal motion)
     ("RET" gomuks-search-open t)
     ("n" gomuks-search-more t)
     ("q" gomuks-search-back t)
     ("b" gomuks-search-back t)
     ("C-k" gomuks-switch-room t)
     ("C-c C-h" gomuks-switch-hidden-room t))
    (gomuks-reactions-mode-map (normal motion)
     ("d" gomuks-remove-reaction t)
     ("q" gomuks-reactions-quit t)
     ("b" gomuks-reactions-quit t))
    (gomuks-attachment-preview-mode-map (normal motion)
     ("RET" gomuks-attachment-preview-open t)
     ("d" gomuks-attachment-preview-remove t)
     ("q" quit-window t))
    (gomuks-stickers-mode-map (normal motion)
     ("RET" gomuks-stickers-activate t)
     ("TAB" gomuks-stickers-tab t)
     ("<backtab>" gomuks-stickers-previous-button t)
     ("n" gomuks-stickers-next-button t)
     ("p" gomuks-stickers-previous-button t)
     ("g" gomuks-stickers-refresh "g r")
     ("q" quit-window t))
    (gomuks-stickers-button-map nil
     ("RET" gomuks-stickers-activate)
     ("TAB" gomuks-stickers-tab)
     ("<backtab>" gomuks-stickers-previous-button)))
  "Every Gomuks key binding, grouped as (KEYMAP EVIL-STATES . BINDINGS).
Each binding is (KEY COMMAND EVIL LEADER).  KEY is bound in KEYMAP unless
nil.  EVIL is t to bind KEY in EVIL-STATES too, or another key to bind
there instead.  LEADER is a key under Doom's localleader.")

(defun gomuks--bind-keys (function)
  "Call FUNCTION with each keymap, its Evil states and one binding."
  (dolist (group gomuks--bindings)
    (dolist (binding (cddr group))
      (funcall function (symbol-value (car group)) (cadr group) binding))))

;; Binding into existing maps means reloading the file updates a running Emacs.
(gomuks--bind-keys
 (lambda (map _states binding)
   (when (car binding)
     (define-key map (kbd (car binding)) (cadr binding)))))

;; Doom's `map!' uses the user's configured localleader and its insert-state
;; alternate.  Keep this optional so the package also loads in plain Emacs.
(when (fboundp 'map!)
  (let (forms)
    (dolist (group gomuks--bindings)
      (let ((keys (cl-loop for (_key command _evil leader) in (cddr group)
                           when leader append (list leader `#',command))))
        (when keys
          (setq forms (append forms (list :map (car group) :localleader) keys)))))
    (eval `(map! ,@forms) t)))

(define-derived-mode gomuks-rooms-mode special-mode "Gomuks rooms"
  "Mode for the gomuks room list."
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-room-mode special-mode "Gomuks room"
  "Mode for a gomuks room timeline."
  (visual-line-mode 1)
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
  (visual-line-mode 1)
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-reactions-mode special-mode "Gomuks reactions"
  "Mode for showing the people who used a reaction."
  (visual-line-mode 1)
  (setq gomuks--view-generation (cl-incf gomuks--view-serial)))
(define-derived-mode gomuks-attachment-preview-mode special-mode "Gomuks attachment"
  "Mode for previewing an attachment staged in a draft.")

;; Evil's state maps take precedence over the major-mode map, so keys that
;; Evil uses in normal state are bound again there.
(with-eval-after-load 'evil
  (dolist (mode '(gomuks-rooms-mode gomuks-room-mode gomuks-search-mode
                  gomuks-reactions-mode gomuks-attachment-preview-mode
                  gomuks-stickers-mode))
    (evil-set-initial-state mode 'normal))
  (evil-set-initial-state 'gomuks-compose-mode 'insert)
  (gomuks--bind-keys
   (lambda (map states binding)
     (pcase-let ((`(,key ,command ,evil) binding))
       (when (and states evil)
         (evil-define-key* states map
                           (kbd (if (eq evil t) key evil)) command))))))

(add-hook 'window-buffer-change-functions #'gomuks--refresh-visible-views)

(provide 'gomuks-ui)
;;; gomuks-ui.el ends here
