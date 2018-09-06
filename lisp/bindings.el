(require 'my-functions)

(global-set-key (kbd "C-`") #'push-mark-no-activate) ; Push current position to mark ring
(global-set-key (kbd "M-`") 'jump-to-mark) ; Pop last mark from mark ring and jump to it
(define-key global-map [remap exchange-point-and-mark]
  #'exchange-point-and-mark-no-activate) ; Don't change region activation state when swapping point and mark
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Set up bindings to quickly open special files
(bind-key* "C-c i" #'find-init-file)
(global-set-key (kbd "C-c j") #'dired-lisp-dir)

(global-set-key (kbd "C-x F") 'ido-sudo-find-file) ; Open file as root

;; TODO: Fix kill-region-yank-and-indent
;; (global-set-key (kbd "C-y") #'kill-region-yank-and-indent)

;; To easily close windows and bury buffers
;;(global-set-key (kbd "s-q") #'quit-window)

;; Make it easier to use macro bindings when fn keys are default
(global-set-key (kbd "M-<f4>") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<f5>") 'kmacro-start-macro-or-insert-counter)

(global-set-key (kbd "C-c o") 'swap-windows)
(define-key ctl-x-4-map "t" #'toggle-frame-split)

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; evil-numbers is used to increment/decrement numbers in region/at point
(require 'evil-numbers)
(global-set-key (kbd "C-c +") #'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") #'evil-numbers/dec-at-pt)

;; TODO: add some way of closing the window if no errors
;; And start gdb if not running
(global-set-key (kbd "C-c C") #'cmake-ide-compile)

;; Fix backspace when paredit is used in slime
;; TODO: Is this used? (should at least be changed to sly)
;; (defun override-slime-repl-bindings-with-paredit ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil)
;;   (define-key slime-repl-mode-map
;;     (kbd "M-S-r") #'slime-repl-previous-matching-input))

;; Define some easier keys to traverse sexps with paredit
(with-eval-after-load "paredit"
  (define-key paredit-mode-map (kbd "DEL")
    #'paredit-backward-delete-or-delete-region))
