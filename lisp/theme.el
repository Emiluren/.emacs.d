;; Theme settings

;; Remove extreme highlighting of rtags errors
;; (Does not quite seem to be working?)
;; Try running it after rtags has been loaded
(defun unset-face-attributes (face attributes &optional frame)
  (dolist (attr attributes)
    (set-face-attribute 'rtags-errline frame attr 'unspecified)))

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  (load-theme 'doom-one t)

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

(use-package zenburn-theme
  :defer t)
(use-package tao-theme
  :defer t)
(use-package apropospriate-theme
  :defer t)

;; Without this the cursor would be black and very hard to see on
;; a dark background
(set-mouse-color "white")

;; Disable menu, tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use nyan-mode to replace scroll bar
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-wavy-trail t))

;; Set the window title to something better
(setq frame-title-format '("%b - Emacs"))

;; Show matching parens
(show-paren-mode 1)
