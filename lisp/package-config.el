;;; Utils

;; Necessary to prevent warnings about undeclared functions during byte compilation
;; (eval-when-compile
;;   (setq use-package-expand-minimally byte-compile-current-file))

(use-package aurel
  :defer t)

(use-package company
  :delight
  :config
  (setq
   ;; Company seems to work poorly with sly and gud/gdb
   ;; TODO: check with sly again
   company-global-modes '(not gud-mode lisp-mode sly-mrepl-mode)
   company-idle-delay 0)

  ;;(add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode)

  ;; :bind (:map company-mode-map
  ;;             ("M-<tab>" . company-complete-common-or-cycle)
  ;;             ("M-TAB" . company-complete-common-or-cycle))
  )

(use-package dired-du
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-du-size-format t))

(use-package ediff
  :defer t
  :functions ediff-window-setup-plain
  :config
  (setq ediff-window-setup-function #'ediff-window-setup-plain)) ; Prevent ediff from using a separate frame for instructions

;; TODO: add iterative reverse history search
;; Check comint-history-isearch-backward-regexp.
(use-package eshell
  :defer t
  :bind* ("C-c e" . eshell)
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp) ; To make eshell use eshell/sudo instead of /usr/bin/sudo
  (setq eshell-hist-ignoredups t
        eshell-prefer-lisp-functions t ; Make sudo work better in eshell
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions nil ; Complete common part first and then list possible completions
        ;; Use a separate line for eshell working directory
        ;; Seems to cause some sort of problem with the history though
        ;; (when used in combination with "flush output" or whatever?)
        eshell-prompt-function (lambda ()
                                 (require 'em-dirs)
                                 (concat (abbreviate-file-name (eshell/pwd))
                                         (if (= (user-uid) 0) "\n# " "\n$ ")))
        eshell-prompt-regexp "[#$] ")
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

(use-package fish-completion
  :if (executable-find "fish")
  :config
  (global-fish-completion-mode))

(use-package fish-mode :defer t)

(use-package flycheck
  :config
  (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list ; Don't pop up a new window for errors if there's already a list
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-ghc-args '("-dynamic")))

;; Show git diff in fringe
;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode 1))

(use-package hippie-exp
  :bind ("M-/" . 'hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; TODO: Maybe switch ido to helm
;; helm-apropos is really cool
(use-package ido
  :config
  (setq ido-enable-flex-matching t                ; Fuzzy matching
        ido-auto-merge-work-directories-length -1 ; And disable annoying auto file search
        ido-create-new-buffer 'always ; Create new buffers without confirmation
        ido-use-virtual-buffers t)
  (ido-mode t))

(use-package idris-mode)

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status)
  :config
  (setq magit-delete-by-moving-to-trash nil ; Delete files directly from magit
        ))

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

;; To enter passwords in minibuffer instead of separate window
(use-package pinentry
  :demand t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;; Prettier modeline
(use-package powerline
  :config
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size face0 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face0 'l))
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (funcall separator-left face0 face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 face0)
				     (powerline-raw " " face0)
				     (powerline-raw "%6p" face0 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face0 face2))
				     (powerline-fill face0 0)
				     )))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(use-package recentf
  :init
  (setq recentf-max-menu-items 150)
  :config
  (recentf-mode 1))

;; Better M-x (on top of Ido)
(use-package smex
  :bind
  (("M-x" . #'smex)
   ("M-X" . #'smex-major-mode-commands)
   ;; Steve Yegge told me to add these :P
   ;; Allows M-x if Alt key is not available
   ("C-x C-m" . #'smex)
   ("C-c C-m" . #'smex)))

;; Make undo easier to use
(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package windmove
  :bind* (("s-h" . windmove-left)
          ("s-j" . windmove-down)
          ("s-k" . windmove-up)
          ("s-l" . windmove-right))
  :config
  ;; To move to other frames
  (add-to-list 'load-path "~/.emacs.d/unpackaged")
  (require 'framemove)
  (setq framemove-hook-into-windmove t))

(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet)

;; Show what keys can be pressed in the middle of a sequence
(use-package which-key
  :delight
  :config
  (which-key-mode 1))

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
                 ("M-." . omnisharp-go-to-definition))
           ;; (:map company-mode-map
           ;;       ("." . (lambda ()
           ;;                (interactive)
           ;;                (insert ".")
           ;;                (company-manual-begin))))
    )))

(use-package clojure-mode
  :defer t
  :config
  (use-package cider)
  (use-package clj-refactor)
  (setq cider-repl-use-pretty-printing t))

(use-package crystal-mode :defer t)
(use-package elixir-mode
  :defer t
  :config
  (use-package alchemist :defer t))
(use-package fsharp-mode :defer t)
(use-package geiser :defer t) ; Scheme IDE
(use-package glsl-mode :defer t)

(use-package haskell-mode
  :defer t
  :bind
  ;; (:map haskell-mode-map
  ;;       ("M-." . haskell-mode-jump-to-def))
  :config
  (use-package intero
    :hook (haskell-mode . intero-mode)))

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
  (use-package eglot
    :hook (rust-mode . eglot)))

;; ;; Set up rust lsp stuff
;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-ui)
;;   (require 'lsp-rust)
;;   (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

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
  (use-package sly-quicklisp)
  (setq inferior-lisp-program "sbcl"  ; Use sbcl for CL repls
        ))

(use-package toml-mode :defer t)
(use-package yaml-mode :defer t)
