;; Added by Package.el. Do not remove
(package-initialize)

;; Set up recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Make sure dir-locals.el is reloaded if the major mode changes
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

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
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; flycheck-clang marks warnings in header files as errors for some reason
(setq-default flycheck-disabled-checkers '(c/c++-clang))

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
;; Don't complete with enter or space
(with-eval-after-load 'company
  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection))

;; Keep backup files in a separate folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

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
;; And disable annoying auto file search
(setq ido-auto-merge-work-directories-length -1)

;; Hide dotfiles, backup files and autosave files in dired
(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(defun enable-dired-omit-mode ()
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)

;; But not for 'recover-session'
(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-mode-hook dired-mode-hook))
    (remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
    ad-do-it))

;; Does not work properly yet
(require 'em-prompt)
(setq eshell-prompt-regexp "[#$] "
      eshell-prompt-function
      (lambda ()
        (concat (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) "\n# " "\n$ "))))

;; Some misc key bindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(defun swap-windows ()
  "Swap the buffer in the current window with the one in the next."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-buffer)
    (set-window-buffer (next-window) this-buffer)
    (select-window (next-window))))
(global-set-key (kbd "C-c o") 'swap-windows)

;; Steve Yegge told me to add these :P
;; Allows M-x if Alt key is not available
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; Enable set-goal-column
(put 'set-goal-column 'disabled nil)

(require 'magit)
(setq magit-delete-by-moving-to-trash nil)

;; Use .m as matlab instead of objective-c
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Open the clfswm config file in lisp mode and xmobar in haskell mode
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".xmobarrc" . haskell-mode))
(add-to-list 'auto-mode-alist '("Makefile2" . makefile-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load "~/.emacs.d/hasklig.el")
(load "~/.emacs.d/c++-stuff.el")
(load-file "~/.emacs.d/theme.el")

(defun dirlocals-flycheck-fix ()
  (when (and buffer-file-name
	     (string= (file-name-nondirectory buffer-file-name) ".dir-locals.el"))
    (flycheck-mode -1)))

;; Disable flycheck for .dir-local files
(add-hook 'emacs-lisp-mode-hook #'dirlocals-flycheck-fix)

(require 'slime)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
