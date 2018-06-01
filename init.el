;;; -*- lexical-binding: t -*-

(setq
;; Give emacs 100mb memory to use before trying to collect garbage
 gc-cons-threshold 100000000
 ;; Fix emacs 26 being slow
 x-wait-for-event-timeout nil
 custom-file "~/.emacs.d/lisp/custom.el"
 )

;; Make sure custom settings are set up before other stuff
;; so that selected packages are available
(load custom-file)

;; Added by Package.el. Do not remove
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Load email address and stuff
(let ((private-file "~/.emacs.d/lisp/private.el"))
  (when (file-exists-p private-file)
   (load private-file)))

;; Enable melpa repository
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; If this is a new install we need to make sure that all packages are available
(unless (file-directory-p "~/.emacs.d/elpa")
  (package-refresh-contents)
  (package-install-selected-packages))

(load "~/.emacs.d/lisp/set-variables.el")
(load "~/.emacs.d/lisp/mode-setup.el")
(load "~/.emacs.d/lisp/bindings.el")
(load "~/.emacs.d/lisp/hooks.el")
(load "~/.emacs.d/lisp/hasklig.el")
(load "~/.emacs.d/lisp/c++-stuff.el")
(load "~/.emacs.d/lisp/theme.el")

;; Use .m as matlab instead of objective-c
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Open the clfswm config file in lisp mode and xmobar in haskell mode
(add-to-list 'auto-mode-alist '("clfswmrc" . lisp-mode))
(add-to-list 'auto-mode-alist '(".xmobarrc" . haskell-mode))
(add-to-list 'auto-mode-alist '("Makefile2" . makefile-mode))

;;; Enable some commands that are disabled by default
;; The goal collumn is where you end up when you switch line
;; (useful for editing tables)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
