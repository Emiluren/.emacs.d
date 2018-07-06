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

(use-package delight)

;; Necessary to prevent warnings about undeclared functions during byte compilation
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;; (use-package benchmark-init
;;   :demand t
;;   ;; To disable collection of benchmark data after init is done.
;;   :hook (after-init . benchmark-init/deactivate))

;; Set the window title to something better
;;(setq frame-title-format '("%b - Emacs"))

(setq mouse-autoselect-window t
      focus-follows-mouse t)

(defun setup-as-wm ()
  (use-package exwm
    :config

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist '(1 "HDMI1"))
    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
		(start-process-shell-command
		 "xrandr" nil "xrandr --output HDMI1 --auto")))
    (exwm-randr-enable)

    (add-hook 'exwm-manage-finish-hook
	      (lambda ()
		(when (and exwm-class-name
			   (string= exwm-class-name "Firefox"))
		  (exwm-input-set-local-simulation-keys
		   '(([?\C-b] . [left])
		     ([?\C-f] . [right])
		     ([?\C-p] . [up])
		     ([?\C-n] . [down])
		     ([?\C-a] . [home])
		     ([?\C-e] . [end])
		     ([?\M-v] . [prior])
		     ([?\C-v] . [next])
		     ([?\C-d] . [delete])
		     ([?\C-k] . [S-end delete])
		     ([?\C-s] . ?\C-f) ; find
		     ([?\M-w] . ?\C-c) ; copy
		     ([?\C-y] . ?\C-v) ; paste
		     )))))

    ;;(setq exwm-workspace-minibuffer-position 'bottom) ; Hide minibuffer when idle
    (require 'exwm-config)
    (exwm-config-default)

    ;; TODO move this to when the first frame is created
    ;;(async-shell-command "compton --config ~/.config/compton.conf" "*compton*")

    ;; Enable moving of windows to other workspaces
    (setq exwm-workspace-show-all-buffers t
	  exwm-layout-show-all-buffers t))
  (use-package symon
    :config
    (setq symon-monitors '(symon-linux-memory-monitor
			   symon-linux-cpu-monitor
			   symon-linux-battery-monitor
			   symon-current-time-monitor))
    (symon-mode)))

;; TODO: make this work in daemon mode
(setup-as-wm)

;; dash - list utilities
(use-package dash
  :config
  (dash-enable-font-lock))

(load "package-config")

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
