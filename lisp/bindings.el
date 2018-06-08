(require 'my-functions)

(global-set-key (kbd "C-`") #'push-mark-no-activate) ; Push current position to mark ring
(global-set-key (kbd "M-`") 'jump-to-mark) ; Pop last mark from mark ring and jump to it
(define-key global-map [remap exchange-point-and-mark]
  #'exchange-point-and-mark-no-activate) ; Don't change region activation state when swapping point and mark
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Set up bindings to quickly open special files
(global-set-key (kbd "C-c i") #'find-init-file)
(global-set-key (kbd "C-c j")
		(lambda ()
		  (interactive)
		  (dired "~/.emacs.d/lisp")))
(global-set-key (kbd "C-c t") #'find-todo-file)

;; Open eshell quickly
(global-set-key (kbd "C-c e") #'eshell)

;; Better M-x (on top of Ido)
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "M-X") #'smex-major-mode-commands)

;; Set up multiple cursors (was added by clj-refactor)
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-x F") 'ido-sudo-find-file) ; Open file as root

;; Don't complete with enter or space  
(with-eval-after-load 'company
  ;; <return> is for gui Emacs; RET is for terminal Emacs
  ;; [tab] is for gui and "TAB" is for terminal
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-selection)
  (define-key company-active-map [tab] 'company-complete-common-or-selection))

;; TODO: Fix kill-region-yank-and-indent
;; (global-set-key (kbd "C-y") #'kill-region-yank-and-indent)

;; To easily close windows and bury buffers
(global-set-key (kbd "s-q") #'quit-window)

;; Some misc key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Make it easier to use macro bindings when fn keys are default
(global-set-key (kbd "M-<f4>") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<f5>") 'kmacro-start-macro-or-insert-counter)

(global-set-key (kbd "C-c o") 'swap-windows)
(define-key ctl-x-4-map "t" #'toggle-frame-split)

;; Steve Yegge told me to add these :P
;; Allows M-x if Alt key is not available
(global-set-key (kbd "C-x C-m") #'smex) ; smex improves execute-extended-command
(global-set-key (kbd "C-c C-m") #'smex)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; evil-numbers is used to increment/decrement numbers in region/at point
(require 'evil-numbers)
(global-set-key (kbd "C-c +") #'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") #'evil-numbers/dec-at-pt)

;; TODO: add some way of closing the window if no errors
;; And start gdb if not running
(global-set-key (kbd "C-c C") #'cmake-ide-compile)

;; Remove sly bindings I don't want
(with-eval-after-load 'sly
  (dolist (key '("E" "I" "x" "i"))
    (define-key sly-prefix-map (kbd key) nil)))

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
