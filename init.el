;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Enable melpa repository
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))

;; Disable menu, tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(setq-default tab-width 4)

;; Some misc key bindings
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-x g") 'magit-status)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(package-selected-packages
   (quote
	(haskell-mode csharp-mode paredit use-package parinfer magit nyan-mode cider slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
