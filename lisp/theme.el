;; Theme settings
;; TODO: refactor to use use-package
(require 'doom-themes)

;;; Code:
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Prevent lsp-face-highlight from being too distracting
(let ((brighter-bg (doom-lighten (face-background 'default) 0.05)))
  (doom-themes-set-faces 'doom-one
	(lsp-face-highlight-read :background brighter-bg)
	(lsp-face-highlight-textual :background brighter-bg)
	(lsp-face-highlight-write :background brighter-bg)))

;; Remove extreme highlighting of rtags errors
;; (Does not quite seem to be working?)
;; Try running it after rtags has been loaded
(defun unset-face-attributes (face attributes &optional frame)
  (dolist (attr attributes)
    (set-face-attribute 'rtags-errline frame attr 'unspecified)))
(with-eval-after-load "rtags"
  (dolist (props '((rtags-errline "red")
		   (rtags-fixitline "yellow")))
    (cl-destructuring-bind (face color) props
      (unset-face-attributes face '(:foreground :background))
      (set-face-attribute face nil :underline
			  `(:color ,color :style wave)))))

(require 'em-prompt)
;; Make the eshell prompt slightly green so it stands out
(set-face-foreground 'eshell-prompt "#9ccca4")

;; Without this the cursor would be black and very hard to see on
;; a dark background
(set-mouse-color "white")

(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

;; Disable menu, tool and scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use nyan-mode to replace scroll bar
(require 'nyan-mode)
(nyan-mode 1)
(setq nyan-wavy-trail t)

;; Set the window title to something better
(setq frame-title-format '("%b - Emacs"))

;; Show matching parens
(show-paren-mode 1)
