
;; TODO: Organize this file somehow

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

(use-package flycheck)
;; flycheck-clojure
;; flycheck-crystal
;; flycheck-elixir
;; flycheck-elm

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

;; Org
(use-package org-mime
  :defer t)

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
  (setq org-agenda-files (list gtd-inbox-file gtd-projects-file gtd-reminder-file)
	org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline gtd-inbox-file "Tasks")
				 "* TODO %i%?")
                              ("T" "Reminder" entry
                               (file+headline gtd-reminder-file "Reminder")
                               "* %i%? \n %U"))
	org-refile-targets `((,gtd-projects-file :maxlevel . 3)
			     (,gtd-someday-file :level . 1)
			     (,gtd-reminder-file :maxlevel . 2))))

;; TODO: Maybe switch ido to helm
;; helm-apropos is really cool
(use-package ido
  :config
  (ido-mode t))

(use-package julia-mode
  :defer t)

(use-package julia-repl
  :after julia-mode
  :defer t)

(use-package flycheck-julia
  :after (julia-mode flycheck)
  :defer t
  :config
  (flycheck-julia-setup))

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
	org-journal-enable-encryption t

	;; variables that are actually from other packages but used for encryption
	;; NOTE: encryption disabled for auto-save
	org-crypt-disable-auto-save nil ; TODO change to encrypt if I can make bitwarden work with org-crypt
	org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom
  (org-journal-file-format "%Y-%m-%d"))

;; Faster than flex completion. Seems to mess stuff up though
;;'(sly-complete-symbol-function (quote sly-simple-complete-symbol))
(use-package sly ; Sylvester the Cat's Common Lisp IDE
  :defer t
  :bind
  ((:map sly-prefix-map
	 ("E" . nil)
	 ("I" . nil)
	 ("i" . nil)
	 ("x" . nil))))

(use-package sly-quicklisp
  :defer t)

(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet)

(use-package fish-mode
  :defer t)

;; Show git diff in fringe
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode 1))

(use-package evil-numbers)

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
(use-package which-key
  :config
  (which-key-mode 1))

(use-package company
  :config
  (setq
   ;; Company seems to work poorly with sly and gud/gdb
   ;; TODO: check with sly again
   company-global-modes '(not gud-mode lisp-mode sly-mrepl-mode)
   company-idle-delay nil)

  ;;(add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode)

  :bind (:map company-mode-map
  	 ("M-<tab>" . company-complete-common)
  	 ("M-TAB" . company-complete-common))
  )

(use-package company-lsp
  :defer t)
(use-package toml-mode
  :defer t)
(use-package lsp-mode
  :defer t)
(use-package lsp-rust
  :defer t)
(use-package lsp-ui
  :defer t)
(use-package yaml-mode
  :defer t)
(use-package rust-mode
  :defer t)
(use-package markdown-mode
  :defer t)
(use-package crystal-mode
  :defer t)
(use-package alchemist
  :defer t)
(use-package elixir-mode
  :defer t)
(use-package fsharp-mode)
(use-package glsl-mode
  :defer t)
(use-package clj-refactor
  :defer t)

;; Scheme IDE
(use-package geiser
  :defer t)

;; Media player daemon that can be used to control mopidy
;; TODO enable again when mopidy setup is complete
(use-package mpdel
  :config
  (mpdel-mode)

  ;; Kill client process when emacs quits
  (libmpdel-ensure-connection)
  (set-process-query-on-exit-flag (libmpdel--process) nil)
  )

;; Make undo easier to use
(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package haskell-mode
  :defer t)
(use-package csharp-mode
  :defer t)
(use-package omnisharp
  :after csharp-mode
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  :hook (csharp-mode . omnisharp-mode)
  :bind ((:map csharp-mode-map
	       ("M-." . omnisharp-go-to-definition))))
(use-package paredit
  :defer t)
(use-package cider
  :defer t)
