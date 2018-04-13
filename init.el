;;; init.el -- My Emacs initalization

;;; Commentary:
;;
;; All my Emacs settings

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Set up recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Set up multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Set up highlighting of cursor/line
(blink-cursor-mode -1)
(global-hl-line-mode 1)

;; Flycheck stuff
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Set up rust lsp stuff
(require 'lsp-ui)
(require 'lsp-rust)
(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'rust-mode-hook #'lsp-rust-enable)

;; I'm not quite sure what the "hover text" is
;; but it intefers with the normal buffer text
(setq lsp-ui-sideline-show-hover nil)

(require 'company-lsp)
(push 'company-lsp company-backends)
(add-hook 'after-init-hook 'global-company-mode)

(load-file "~/.emacs.d/theme.el")

;; Make undo easier to use
(global-undo-tree-mode)

;; Enable melpa repository
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

;; Enable column number in modeline
(setq column-number-mode t)

;; Don't use tabs and use 4 spaces for indentation
;; Also use tab to complete
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-always-indent 'complete
              tab-width 4)

;; Make Emacs split window horizontally by default
(setq split-height-threshold nil
	  split-width-threshold 120)

;; Faster than the default scp (according to Emacs wiki)
(require 'tramp)
(setq tramp-default-method "ssh")

;; Enable Interactively Do Things
(require 'ido)
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1) ; And disable annoying auto file search

;; Hide backup files and autosave files in dired
(require 'dired-x)
(setq-default dired-omit-files-p t)

;; Some misc key bindings
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(require 'magit)
(setq magit-delete-by-moving-to-trash nil)

;; Use .m as matlab instead of objective-c
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Open the clfswm config file in lisp mode and xmobar in haskell mode
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".xmobarrc" . haskell-mode))

(require 'slime)
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
  "Enable paredit and clj-refactor in clojure."
  (require 'paredit)
  (enable-paredit-mode)
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'init)
;;; init.el ends here
