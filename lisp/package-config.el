
;;; Utils

;; Necessary to prevent warnings about undeclared functions during byte compilation
;; (eval-when-compile
;;   (setq use-package-expand-minimally byte-compile-current-file))

(use-package aurel
  :defer t)

(use-package ediff
  :defer t
  :functions ediff-window-setup-plain
  :config
  (setq ediff-window-setup-function #'ediff-window-setup-plain)) ; Prevent ediff from using a separate frame for instructions

;; TODO: add iterative reverse history search
;; Check comint-history-isearch-backward-regexp.
(use-package eshell
  :defer t
  :config
  (setq eshell-hist-ignoredups t
	eshell-cmpl-ignore-case t
	eshell-prompt-regexp "[#$] "
	;; To make sudo work better in eshell
	eshell-prefer-lisp-functions t
	;; Use a separate line for eshell working directory
	;; Seems to cause some sort of problem with the history though
	;; (when used in combination with "flush output" or whatever?)
	eshell-prompt-function (lambda ()
				 (require 'em-dirs)
				 (concat (abbreviate-file-name (eshell/pwd))
					 (if (= (user-uid) 0) "\n# " "\n$ "))))
  ;; TODO Create an lls command to run ls locally in tramp eshell
  (defun eshell/lcd (&optional directory)
    (eval-and-compile
      (require 'em-dirs)
      (require 'tramp))
    (if (file-remote-p default-directory)
	(with-parsed-tramp-file-name default-directory nil
	  (eshell/cd (tramp-make-tramp-file-name
		      method
		      user
		      domain
		      host
		      port
		      (or directory "")
		      hop)))
      (eshell/cd directory))))

(use-package evil-numbers) ; Binds "C-c +" and "C-c -" to increase decrease numbers in region
(use-package fish-mode :defer t)

(use-package flycheck
  :config
  (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list ; Don't pop up a new window for errors if there's already a list
	flycheck-emacs-lisp-load-path 'inherit))

;; Show git diff in fringe
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode 1))

;; TODO: Maybe switch ido to helm
;; helm-apropos is really cool
(use-package ido
  :config
  (setq ido-enable-flex-matching t                ; Fuzzy matching
	ido-auto-merge-work-directories-length -1 ; And disable annoying auto file search
	ido-create-new-buffer 'always ; Create new buffers without confirmation
	ido-use-virtual-buffers t)
  (ido-mode t))

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-delete-by-moving-to-trash nil ; Delete files directly from magit
	))

;; Media player daemon that can be used to control mopidy
;; TODO enable again when mopidy setup is complete
(use-package mpdel
  :delight
  :config
  (mpdel-mode)

  ;; Kill client process when emacs quits
  (libmpdel-ensure-connection)
  (set-process-query-on-exit-flag (libmpdel--process) nil)
  )

;; Org
(use-package org-mime
  :defer t
  :after org)

;; GTD setup inspired by https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(defvar gtd-inbox-file "~/.emacs.d/personal-org/gtd/inbox.org")
(defvar gtd-projects-file "~/.emacs.d/personal-org/gtd/projects.org")
(defvar gtd-reminder-file "~/.emacs.d/personal-org/gtd/reminder.org")
(defvar gtd-someday-file "~/.emacs.d/personal-org/gtd/someday.org")

(use-package org
  :ensure org-plus-contrib
  :defer t
  :bind
  (("C-c l" . 'org-store-link)
   ("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   ("C-c b" . 'org-switchb))
  :config
  (setq org-directory "~/.emacs.d/personal-org/"
	org-default-notes-file (concat org-directory "/notes.org")
	org-agenda-files (list gtd-inbox-file gtd-projects-file gtd-reminder-file)
	org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline gtd-inbox-file "Tasks")
				 "* TODO %i%?")
                              ("T" "Reminder" entry
                               (file+headline gtd-reminder-file "Reminder")
                               "* %i%? \n %U"))
	org-refile-targets `((,gtd-projects-file :maxlevel . 3)
			     (,gtd-someday-file :level . 1)
			     (,gtd-reminder-file :maxlevel . 2))))

;; To enter passwords in minibuffer instead of separate window
(use-package pinentry
  :demand t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package org-journal
  :init
  (defun insert-org-journal-password ()
    (interactive)
    (let ((pass (get-org-journal-password)))
      (when pass
	(insert pass))))
  :bind* (("C-c P" . insert-org-journal-password))
  :config
  (setq org-journal-dir "~/.emacs.d/personal-org/dagbok"
	;; org-journal-enable-encryption t

	;; variables that are actually from other packages but used for encryption
	;; org-tags-exclude-from-inheritance (quote ("crypt"))
	)
  :custom
  (org-journal-file-format "%Y-%m-%d"))

;; Make undo easier to use
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet)

;; Better M-x (on top of Ido)
(use-package smex
  :bind
  (("M-x" . #'smex)
   ("M-X" . #'smex-major-mode-commands)
   ;; Steve Yegge told me to add these :P
   ;; Allows M-x if Alt key is not available
   ("C-x C-m" . #'smex)
   ("C-c C-m" . #'smex)))

;; Show what keys can be pressed in the middle of a sequence
;; (use-package which-key
;;   :delight
;;   :config
;;   (which-key-mode 1))

(use-package company
  :delight
  :config
  (setq
   ;; Company seems to work poorly with sly and gud/gdb
   ;; TODO: check with sly again
   company-global-modes '(not gud-mode lisp-mode sly-mrepl-mode)
   company-idle-delay nil)

  ;;(add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode)

  :bind (:map company-mode-map
  	 ("M-<tab>" . company-manual-begin)
  	 ("M-TAB" . company-manual-begin)))

;;; Programming languages

;; TODO: add these
;; flycheck-clojure
;; flycheck-crystal
;; flycheck-elixir
;; flycheck-elm

(use-package csharp-mode
  :defer t
  :config
  (use-package omnisharp
    :after csharp-mode
    :init
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-omnisharp))
    :hook (csharp-mode . omnisharp-mode)
    :bind ((:map csharp-mode-map	
	 ("M-." . omnisharp-go-to-definition)))))

(use-package clojure-mode
  :defer t
  :config
  (use-package cider)
  (use-package clj-refactor))

(use-package crystal-mode :defer t)
(use-package elixir-mode
  :defer t
  :config
  (use-package alchemist :defer t))
(use-package fsharp-mode :defer t)
(use-package geiser :defer t) ; Scheme IDE
(use-package glsl-mode :defer t)
(use-package haskell-mode :defer t)
(use-package julia-mode
  :defer t
  :config
  (use-package flycheck-julia :config (flycheck-julia-setup))
  (use-package julia-repl))
(use-package markdown-mode :defer t)

(use-package paredit
  :defer t
  :bind
  ((:map paredit-mode-map
	 ("\\" . nil)) ; Remove annoying \ escape
   ))

(use-package rust-mode
  :defer t
  :config
  (use-package lsp-mode)
  (use-package lsp-rust)
  (use-package lsp-ui)
  (use-package company-lsp))

;; Faster than flex completion. Seems to mess stuff up though
;;'(sly-complete-symbol-function (quote sly-simple-complete-symbol))
(use-package sly ; Sylvester the Cat's Common Lisp IDE
  :defer t
  :bind
  ((:map sly-prefix-map
	 ("E" . nil)
	 ("I" . nil)
	 ("i" . nil)
	 ("x" . nil)))
  :config
  (use-package sly-quicklisp))

(use-package toml-mode :defer t)
(use-package yaml-mode :defer t)
