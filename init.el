;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load-theme 'zenburn t)

;; Make undo easier to use
(global-undo-tree-mode)

;; Enable melpa repository
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

;; Disable menu, tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable column number in modeline
 (setq column-number-mode t)

;; Don't use tabs and use 4 spaces for indentation
;; Also use tab to complete
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-always-indent 'complete)

;; Make Emacs split window horizontally by default
(setq split-height-threshold nil
	  split-width-threshold 120)

;; Use nyan-mode to replace scroll bar
(nyan-mode 1)
(setq nyan-wavy-trail t)

;; Set the window title to something better
(setq frame-title-format '("%b - Emacs"))

;; Faster than the default scp (according to Emacs wiki)
(setq tramp-default-method "ssh")

;; Enable Interactively Do Things
(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1) ; And disable annoying auto file search

(setq-default tab-width 4)

;; Some misc key bindings
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-delete-by-moving-to-trash nil)

;; Use .m as matlab instead of objective-c
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Open the clfswm config file in lisp mode
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))

;; Hide backup files and autosave files in dired
(require 'dired-x)
(setq-default dired-omit-files-p t)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook    #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (enable-paredit-mode)
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(show-paren-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" default)))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(ido-enable-flex-matching t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (tao-theme alchemist elixir-mode apropospriate-theme glsl-mode clj-refactor geiser zenburn-theme undo-tree haskell-mode csharp-mode paredit use-package parinfer magit nyan-mode cider slime)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

