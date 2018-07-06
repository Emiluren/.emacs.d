;;; -*- lexical-binding: t -*-

;; Added by Package.el. Do not remove
(package-initialize)

;; TODO: move all gitignore files to .not_in_git
;; TODO: Maybe change M-/ to hippie-expand

(setq
 ;; Give emacs 100mb memory to use before trying to collect garbage
 gc-cons-threshold 100000000
 ;; Fix emacs 26 being slow
 x-wait-for-event-timeout nil
 custom-file "~/.emacs.d/lisp/custom.el"
 global-font-lock-mode nil
 )
;; I don't use custom but package.el will write selected packages to it
(load custom-file t)

(add-to-list 'load-path "~/.emacs.d/lisp") ; All of my other elisp files
(add-to-list 'load-path "~/Programmering/emacs/telega.el/") ; Telegram client

;; Load email address and stuff
(load "private" t)

;; Enable melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; If this is a new install we need to make sure that all packages are available
(package-activate 'use-package)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Necessary to prevent warnings about undeclared functions during byte compilation
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;; (use-package benchmark-init
;;   :demand t
;;   ;; To disable collection of benchmark data after init is done.
;;   :hook (after-init . benchmark-init/deactivate))

(use-package exwm
  :config

  (require 'exwm-randr)
  (exwm-randr-enable)

  (require 'exwm-config)
  (exwm-config-default)

  (async-shell-command "compton" "*compton*"))

;; dash - list utilities
(use-package dash
  :config
  (dash-enable-font-lock))

(load "package-config")

;; TODO: refactor these with use-package
(load "set-variables")
(load "mode-setup")
(load "bindings")
(load "hooks")

(add-hook 'c-mode-common-hook (lambda ()
				(require 'my-c++-settings)))

(load "hasklig")
(load "theme")

;; TODO: Move these to use use-package :mode instead
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)) ; objective-c by default
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".xmobarrc" . haskell-mode))
(add-to-list 'auto-mode-alist '("Makefile2" . makefile-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD" . sh-mode))

;;; Enable some commands that are disabled by default
;; The goal collumn is where you end up when you switch line
;; (useful for editing tables)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
