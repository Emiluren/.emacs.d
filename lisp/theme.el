;; Theme settings

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

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
