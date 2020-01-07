;;; -*- lexical-binding: t -*-

;; Added by Package.el. Do not remove
(package-initialize)

;;; Initialization
(setq
 ;; Give emacs 100mb memory to use before trying to collect garbage
 gc-cons-threshold 100000000
 ;; Fix emacs 26 being slow
 x-wait-for-event-timeout nil
 global-font-lock-mode nil
 )

;; I don't use custom but package.el will write selected packages to it.
(setq custom-file "~/.emacs.d/lisp/custom.el")
(load custom-file t)

;;; Package repository init
;; Unpackaged is my folder for stuff I have not written but is not on melpa.
;; Mostly from the Emacs wiki
(let ((default-directory "~/.emacs.d/unpackaged/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Load email address and stuff
(load "~/lisp/private.el" t)

;; Enable melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; This is required because of a bug in Emacs 26.2
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; If this is a new install we need to make sure that all packages are available
(package-activate 'use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package delight)
(delight '((auto-revert-mode nil "autorevert") (eldoc-mode nil "eldoc")))

;; Necessary to prevent warnings about undeclared functions during byte compilation
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;; (use-package benchmark-init
;;   :demand t
;;   ;; To disable collection of benchmark data after init is done.
;;   :hook (after-init . benchmark-init/deactivate))

;; Set the window title to something better
(setq frame-title-format '("%b - Emacs"))

(setq mouse-autoselect-window t
      focus-follows-mouse t)

;; Don't accelerate scroll speed
(setq mouse-wheel-progressive-speed nil)

;; dash - list utilities
(use-package dash
  :config
  (dash-enable-font-lock))

;;; Defing functions
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (let ((mark-was-active mark-active))
    (exchange-point-and-mark)
    (unless mark-was-active
      (deactivate-mark))))

(defun find-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-todo-file ()
  (interactive)
  (find-file "~/.emacs.d/personal-org/emacs_todo.org"))

;; Found on http://emacs-fu.blogspot.com/
(defun ido-sudo-find-file ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable
by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (find-file
     (if (file-writable-p file)
         file
       (concat "/sudo::" file)))))

(defun focus-gdb-buffer-when-stopped (gdb-result)
  (require 'bindat)
  (require 'notifications)
  (unless (and (fboundp 'bindat-get-field)
               (string-equal (bindat-get-field gdb-result 'reason)
                             "exited-normally"))
    (notifications-notify :title "GDB"
                          :body "Execution stopped.")
    ;; This is overwritten immediately by the source buffer
    ;; so not the best solution
    (require 'gdb-mi)
    (when (fboundp 'gdb-get-buffer-create)
      (switch-to-buffer (gdb-get-buffer-create 'gdb-inferior-io)))
    (when (fboundp 'gdb-display-gdb-buffer)
      (gdb-display-gdb-buffer))))

(defun swap-windows ()
  "Swap the buffer in the current window with the one in the next."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-buffer)
    (set-window-buffer (next-window) this-buffer)
    (select-window (next-window))))

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; Used in case eshell locks up
;; (because of something with the prompt regexp I guess?
(defun force-erase-buffer ()
  "Force delete all text in the buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun force-kill-current-buffer ()
  "Force kill current buffer"
  (interactive)
  (let ((inhibit-read-only))))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun buffer-file-name= (name &optional process-fun)
  (let ((f (or process-fun #'file-name-nondirectory)))
    (and buffer-file-name
         (string= (funcall f buffer-file-name) name))))

;; Disable flycheck for .dir-local files
(defun dirlocals-flycheck-fix ()
  (when (buffer-file-name= ".dir-locals.el")
    (flycheck-mode -1)))

(defun call-process-string-output (program &rest args)
  (with-temp-buffer
    (when (= 0 (apply #'call-process program nil t nil args))
      (string-trim (buffer-string)))))

(defun visual-line-range ()
  "Return a cons cell of the range between the start and end of the visual line.
Indended to be used for highlighting of only the visual line in hl-line mode"
  (if truncate-lines
      (cons
       (line-beginning-position)
       (line-beginning-position 2))
      (save-excursion
        (cons
         (progn (beginning-of-visual-line) (point))
         (progn (beginning-of-visual-line 2) (point))))))

;;; C++ functions
;; Functions that are only used for C++ mode.

(require 'gud)
(require 'gdb-mi)

;; TODO: Gdb ignores default-directory if given a filename
(defun cmake-ide-gdb-command ()
  (require 'gud)
  (let ((build-dir (cide--build-dir)))
    (if (and (boundp 'cmake-ide-build-dir)
             (boundp 'cmake-ide-executable))
        (concat "gdb -i=mi "
                (file-name-as-directory (symbol-value 'cmake-ide-build-dir))
                (symbol-value 'cmake-ide-executable))
      ;; Fall back to last command
      (car gud-gdb-history))))

(defun cmake-ide-start-or-switch-to-gdb ()
  (interactive)
  (if (and gud-comint-buffer (buffer-live-p gud-comint-buffer))
        (gdb-display-gdb-buffer)
      (let ((default-directory (cide--locate-project-dir)))
        (gdb "gdb -i=mi"))))

(defun start-gdb-if-successfully-compiled (buffer msg)
  ;; Compilation mode is used for some other stuff (grep, etc) so we
  ;; need to check the buffer name
  (when (and
         (string-match "^finished" msg)
         (string= (buffer-name buffer) "*compilation*"))
    (cmake-ide-start-or-switch-to-gdb)))

;; TODO: make into an interactive function that runs compile and then starts gdb
;; (add-hook 'compilation-finish-functions
;;           #'start-gdb-if-successfully-compiled)

;;; General package configuration
;;; Tools
(use-package cmake-ide
  :defer t
  :config
  (cmake-ide-setup))
(use-package cmake-mode
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

;; Smartparens is not enabled in minibuffers currently
;; (use-package electric
;;   :config
;;   ;; MESSES WITH SMARTPARENS IF ENABLED SIMULTANEOUSLY
;;   (electric-pair-mode -1))

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
        flycheck-ghc-args '("-dynamic")
        flycheck-global-modes '(not rust-mode))
  (global-flycheck-mode)
  (add-hook 'flycheck-error-list-mode-hook (lambda () (setq truncate-lines nil))))

(use-package helm
  :delight
  :bind
  (("M-x" . helm-M-x)
   ;;("C-x C-m" . helm-M-x) ; Used for encoding by default
   ;;("C-c C-m" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list))
  :config
  (require 'helm-config)
  (helm-mode 1))

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

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-delete-by-moving-to-trash nil ; Delete files directly from magit
        ))

;; Org
(use-package org-mime ; Send html email using org-mode export
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
                             (,gtd-reminder-file :maxlevel . 2))
        org-latex-packages-alist '(("margin=2cm" "geometry" nil))
        org-clock-persist 'history)
  (org-clock-persistence-insinuate))

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

;; Better pdf viewing (docview is kind of blurry), can also edit pdfs
(use-package pdf-tools
  :bind (:map pdf-view-mode-map
         ("n" . image-forward-hscroll)
         ("e" . pdf-view-next-line-or-next-page)
         ("i" . pdf-view-previous-line-or-previous-page)
         ("o" . image-backward-hscroll))
  :config
  (pdf-loader-install))

;; To enter passwords in minibuffer instead of separate window
(use-package pinentry
  :demand t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package recentf
  :init
  (setq recentf-max-menu-items 150)
  ;; Magic advice to rename entries in recentf when moving files in
  ;; dired.
  (defun rjs/recentf-rename-notify (oldname newname &rest args)
    (if (file-directory-p newname)
        (rjs/recentf-rename-directory oldname newname)
      (rjs/recentf-rename-file oldname newname)))

  (defun rjs/recentf-rename-file (oldname newname)
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-equal name oldname)
                        newname
                      oldname))
                  recentf-list)))

  (defun rjs/recentf-rename-directory (oldname newname)
    ;; oldname, newname and all entries of recentf-list should already
    ;; be absolute and normalised so I think this can just test whether
    ;; oldname is a prefix of the element.
    (setq recentf-list
          (mapcar (lambda (name)
                    (if (string-prefix-p oldname name)
                        (concat newname (substring name (length oldname)))
                      name))
                  recentf-list)))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (advice-add 'dired-rename-file :after #'rjs/recentf-rename-notify))

;; RTags is used in C++
(use-package rtags
  :defer t
  :config
  (setq rtags-path
      (format "%srtags-%s/bin/"
              (rtags-package-install-path)
              rtags-package-version))

  (unless (file-exists-p rtags-path)
    (when (y-or-n-p "RTags has not been compiled. Do you want to do that now?")
      (rtags-install)))
  (use-package company-rtags
    :after (company rtags))
  (use-package flycheck-rtags
    :after (flycheck rtags))

  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)

  ;; TODO: Should rtags be used for all c-modes?
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq-local eldoc-documentation-function #'rtags-eldoc)))

  ;; (define-key c-mode-map [(tab)] 'company-complete)
  ;; (define-key c++-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map (kbd "M-.") #'rtags-find-symbol-at-point)

  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil) ; RTags runs checker manually?
    )

  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

  )

(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")))

(use-package smartparens
  :delight
  :hook (minibuffer-setup . turn-on-smartparens-strict-mode) ; Doesn't seem quite working
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  :custom
  (sp-override-key-bindings '(("M-<backspace>" . nil)))
  (sp-base-key-bindings 'paredit))

(use-package undo-tree
  :delight
  :config
  (setq undo-tree-enable-undo-in-region nil)
  (global-undo-tree-mode 1))

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

(use-package auctex
  :defer t
  :config
  ;; Latex templates?
  (use-package cdlatex))

(when (file-exists-p "~/programmering/Carp/emacs/carp-mode.el")
  (add-to-list 'load-path "~/programmering/Carp/emacs")
  (require 'carp-mode)
  (require 'inf-carp-mode)
  (add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode)))

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
  ;;(use-package clj-refactor)
  (setq cider-repl-use-pretty-printing t))

;; Load carp-mode (must happen after clojure-mode is loaded)
(add-to-list 'load-path "~/Programmering/carp/Carp/emacs")
(require 'carp-mode)
(require 'inf-carp-mode)
(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))

(use-package crystal-mode :defer t)
(use-package elm-mode :defer t
  :config
  (use-package flycheck-elm))
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
    :config
    (intero-global-mode 1)))

(use-package julia-mode
  :defer t
  :config
  (use-package flycheck-julia :config (flycheck-julia-setup))
  (use-package julia-repl
    :hook julia-mode))

(use-package idris-mode :defer t)

(use-package markdown-mode :defer t)

(use-package racket-mode :defer t)

(use-package rust-mode
  :defer t
  :init
  (defun my-project-try-cargo-toml (dir)
    "Try to locate a Rust project above DIR."
    (let ((found (locate-dominating-file dir "Cargo.toml")))
      (if (stringp found) `(transient . ,found) nil)))
  :config
  (add-to-list 'project-find-functions #'my-project-try-cargo-toml)
  (use-package eglot
    ;; eglot is a general lsp package
    :hook (rust-mode . eglot-ensure)
    :custom
    (eglot-confirm-server-initiated-edits nil)))

;; Slime currently has to be used for cepl/livesupport (Don't remember why)
(use-package slime
  :defer t
  :hook (lisp-mode . slime-mode)
  :config
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (when (file-exists-p "/home/emil/sbcl.core-for-slime")
    (setq slime-lisp-implementations '((sbcl
                                        ("sbcl" "--core" "/home/emil/sbcl.core-for-slime"))))))

;; Faster than flex completion. Seems to mess stuff up though
;;'(sly-complete-symbol-function (quote sly-simple-complete-symbol))
;; (use-package sly ; Sylvester the Cat's Common Lisp IDE
;;   :defer t
;;   ;;:hook (lisp-mode . sly-mode)
;;   :bind
;;   ((:map sly-prefix-map
;;          ("E" . nil)
;;          ("I" . nil)
;;          ("i" . nil)
;;          ("x" . nil)))
;;   :config
;;   (use-package sly-quicklisp)
;;   (setq inferior-lisp-program "sbcl"  ; Use sbcl for CL repls
;;         ))

(use-package toml-mode :defer t)

(use-package typescript-mode
  :config
  (use-package tide))

(use-package yaml-mode :defer t)

;;; Set global builtin modes
;; Enable saving of minibuffer history
(savehist-mode 1)

;; Delete selected text when entering new if region is active
(delete-selection-mode 1)

;; Set up highlighting of cursor/line
(blink-cursor-mode -1)
;; (global-hl-line-mode 1)
(setq hl-line-range-function #'visual-line-range) ; Only highlight visual line, not wrapped

;; Binds ‘C-c left’ and ‘C-c right’ to undo and redo window changes
(winner-mode 1)

;;; Set variables
;; Set variables that don't fit better under Package config (or that I
;; haven't had the time to move yet).

;; Keep closing paren for argument list indented to previous level
(c-add-style "my-c-style"
             '("linux"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil) ; Don't use tabs
               (c-offsets-alist
                ;; Keep the closing brace previous indentation
                (arglist-close . 0))))

(setq
 ;; Keep backup and auto save files in their own folders
 ;; Also place remote files in /tmp by default (there is something wrong with sudo)
 auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
                                  (".*" ,(concat user-emacs-directory "backups/") t))
 backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups/")))
 backward-delete-char-untabify-method nil ; Don't convert tabs to spaces when deleting
 case-replace nil ;; Replace with case from replacement string
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (csharp-mode . "my-c-style") ; csharp-mode will automatically override the style if we don't set it specifically
                   (other . "my-c-style"))
 calendar-week-start-day 1          ; Week starts on monday
 column-number-mode t               ; Enable column number in modeline
 confirm-kill-processes nil ; Don't ask for confirmation when closing a buffer that is attached to a process
 confirm-nonexistent-file-or-buffer nil ; Don't ask for confirmation when creating new buffers
 dabbrev-case-fold-search nil           ; Make dabbrev case sensitive
 electric-indent-inhibit t ; Stop electric indent from indenting the previous line
 gdb-display-io-nopopup t ; Stop io buffer from popping up when the program outputs anything
 history-delete-duplicates t
 html-quick-keys nil ; prevent C-c X bindings when using sgml-quick-keys
 lazy-highlight-initial-delay 0 ; Don't wait before highlighting searches
 ;; Push clipboard contents from other programs to kill ring also
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil     ; Sentences end with a single space
 sgml-quick-keys t  ; Make characters in html behave electrically
 ;; Make Emacs split window horizontally by default
 split-height-threshold nil
 split-width-threshold 120
 tab-always-indent 'complete            ; Use tab to complete
 ;; Faster than the default scp (according to Emacs wiki)
 tramp-default-method "ssh"
 visible-bell 1 ; Turn off annoying sound
 )

;; Set up gnus
(setq gnus-directory "~/.emacs.d/mail"
      message-directory "~/.emacs.d/mail"
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nnimap "Skolmail"
                                              (nnimap-address "outlook.office365.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl)))
                                        ;gnus-interactive-exit nil ; stop prompt but do I want it for updates or something?
      ;; Make sure emails end up in sent folder after they have been sent
      ;; TODO: not working?
      ;; gnus-message-archive-group "nnimap+Skolmail:Skickade objekt"
      ;; Settings for sending email
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

(setq-default
 word-wrap t ; Make line wraps happen at word boundaries
 indent-tabs-mode nil ; Don't use tabs unless the .dir-locals file says so
 )

(with-eval-after-load 'dired-x
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

;; Use y/n instead of longer yes/no
;; (fset 'yes-or-no-p 'y-or-n-p)

;;; Bindings
;; Similar to the variables set above. Some of these should be moved to
;; the configuration of their respective packages.

(global-set-key (kbd "C-`") #'push-mark-no-activate) ; Push current position to mark ring
(global-set-key (kbd "M-`") 'jump-to-mark) ; Pop last mark from mark ring and jump to it
(define-key global-map [remap exchange-point-and-mark]
  #'exchange-point-and-mark-no-activate) ; Don't change region activation state when swapping point and mark
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Set up bindings to quickly open special files
(bind-key* "C-c i" #'find-init-file)

(global-set-key (kbd "C-x F") 'ido-sudo-find-file) ; Open file as root

;; Make it easier to use macro bindings when fn keys are default
(global-set-key (kbd "M-<f4>") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<f5>") 'kmacro-start-macro-or-insert-counter)

(global-set-key (kbd "C-c o") 'swap-windows)
(define-key ctl-x-4-map "t" #'toggle-frame-split)

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; evil-numbers is used to increment/decrement numbers in region/at point
(use-package evil-numbers
  :config
  (global-set-key (kbd "C-c +") #'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") #'evil-numbers/dec-at-pt))

;; TODO: add some way of closing the window if no errors
;; And start gdb if not running
(global-set-key (kbd "C-c C") #'cmake-ide-compile)

;; Go to start and end of visual line instead of wrapped line
(global-set-key (kbd "C-a")
                (lambda ()
                  (interactive)
                  (if truncate-lines
                      (beginning-of-line)
                    (beginning-of-visual-line))))
(global-set-key (kbd "C-e")
                (lambda ()
                  (interactive)
                  (if truncate-lines
                      (end-of-line)
                    (end-of-visual-line))))

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;;; ** Hooks
;; Some of the stuff under here should also be moved to it's package configuration
;; TODO: move everything to package-config

;; Make sure dir-locals.el is reloaded if the major mode changes
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Don't line break
            (setq truncate-lines t)))

;; Pop up emacs frame, gdb buffer and io buffer on error
(add-hook 'gdb-stopped-functions #'focus-gdb-buffer-when-stopped)

;; dired-x is required for dired-omit-mode
(add-hook 'dired-mode-hook (lambda () (require 'dired-x)))

;; Do not use dired-omit-mode for 'recover-session'
(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-omit-mode nil))
    ad-do-it))

(add-hook 'emacs-lisp-mode-hook #'dirlocals-flycheck-fix)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Ligature font
;; Use the hasklig font but only in haskell mode if it's installed.

(require 'dash)

;; The code in this file comes from https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md
(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))

(defun my-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with
codepoints starting from codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))

;; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
(defvar my-hasklig-ligatures
  (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                 "==" "===" "==>" "=>" "=<<" "!!" ">>"
                 ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                 "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                 "<<" "<<<" "<+>" ".." "..." "++" "+++"
                 "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
    (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
(defun my-set-hasklig-ligatures ()
  "Add hasklig ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append my-hasklig-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))

(when (and (window-system)
           (find-font (font-spec :name "Hasklig")))
  (set-frame-font "Hasklig")
  (add-hook 'haskell-mode-hook 'my-set-hasklig-ligatures))

;;; Theme
;; Set up color theme and other visual stuff.
(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-one t)
  (electric-pair-mode -1) ; For some reason electric-pair-mode is enabled here

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Prevent lsp-face-highlight from being too distracting
  (with-eval-after-load "lsp-methods"
    (let ((brighter-bg (doom-lighten (face-background 'default) 0.05)))
      (doom-themes-set-faces 'doom-one
     (lsp-face-highlight-read :background brighter-bg)
     (lsp-face-highlight-textual :background brighter-bg)
     (lsp-face-highlight-write :background brighter-bg))))

  (with-eval-after-load "rtags"
    (dolist (props '((rtags-errline "red")
                  (rtags-fixitline "yellow")))
      (cl-destructuring-bind (face color) props
     (unset-face-attributes face '(:foreground :background))
     (set-face-attribute face nil :underline
                         `(:color ,color :style wave)))))

  (with-eval-after-load "em-prompt"
    ;; Make the eshell prompt slightly green so it stands out
    (set-face-foreground 'eshell-prompt "#9ccca4")))

(use-package solaire-mode
  :after doom-themes
  :demand t
  :hook ((after-change-major-mode . turn-on-solaire-mode)
         (ediff-prepare-buffer . solaire-mode)
         ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
         ;; itself off every time Emacs reverts the file
         (after-revert . turn-on-solaire-mode)
         ;; highlight the minibuffer when it is activated:
         (minibuffer-setup . solaire-mode-in-minibuffer))

  :config
  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

;; (use-package eink-theme
;;   :config
;;   (load-theme 'eink t)
;;   (set-face-attribute 'font-lock-comment-face nil :weight 'light)
;;   (set-face-attribute 'mode-line nil :height 1.0)
;;   (set-face-attribute 'mode-line-inactive nil :height 1.0))

(use-package paren-face
  :config
  (global-paren-face-mode))

;; (use-package leuven-theme
;;   :config
;;   (load-theme 'leuven t))

;; (load-theme 'tango-dark t)

(use-package yascroll
  :config
  (global-yascroll-bar-mode)
  (setq yascroll:delay-to-hide nil))

;; Without this the cursor would be black and very hard to see on
;; a dark background
(set-mouse-color "white")

;; Disable menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show matching parens
(show-paren-mode 1)

;;; ** Final init
;; Set up some auto modes and enable some useful disabled commands.

;; TODO: Move these to use use-package :mode instead
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; objective-c by default
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)) ; perl by default
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)) ; scheme by default
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".sbclrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".xmobarrc" . haskell-mode))
(add-to-list 'auto-mode-alist '("Makefile2" . makefile-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))
(add-to-list 'auto-mode-alist '("config.work" . conf-space-mode))
(add-to-list 'auto-mode-alist '("config.base" . conf-space-mode))

;;; Enable some commands that are disabled by default
;; The goal collumn is where you end up when you switch line
;; (useful for editing tables)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
