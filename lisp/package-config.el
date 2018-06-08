;; Necessary to prevent warnings about undeclared functions during byte compilation
;; (eval-when-compile
;;   (setq use-package-expand-minimally byte-compile-current-file))

(use-package ediff
  :defer t
  :defines ediff-window-setup-function
  :functions ediff-window-setup-plain
  :config
  (setq ediff-window-setup-function #'ediff-window-setup-plain)) ; Prevent ediff from using a separate frame for instructions

(use-package eshell
  :defer t
  :config
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

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

;; Org
(use-package org-mime
  :defer t)

(use-package org
  :ensure org-plus-contrib
  :defer t
  :bind
  (("C-c l" . 'org-store-link)
   ("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   ("C-c b" . 'org-switchb)))

;; Julia
(use-package julia-mode
  :defer t)

(use-package julia-repl
  :after julia-mode
  :defer t)

(use-package flycheck-julia
  :after julia-mode
  :defer t
  :config
  (flycheck-julia-setup))

;; TODO: install these packages with use-package
;; sly-quicklisp
;; fish-completion
;; fish-mode
;; git-gutter
;; yasnippet-snippets
;; solaire-mode
;; yasnippet
;; sly
;; company-rtags
;; evil-numbers
;; flycheck-rtags
;; smex
;; which-key
;; cmake-ide
;; cmake-mode
;; company-lsp
;; toml-mode
;; lsp-mode
;; lsp-rust
;; lsp-ui
;; yaml-mode
;; flycheck
;; flycheck-clojure
;; flycheck-crystal
;; flycheck-elixir
;; flycheck-elm
;; rust-mode
;; markdown-mode
;; crystal-mode
;; doom-themes
;; tao-theme
;; alchemist
;; elixir-mode
;; apropospriate-theme
;; glsl-mode
;; clj-refactor
;; geiser
;; zenburn-theme
;; undo-tree
;; haskell-mode
;; csharp-mode
;; paredit
;; parinfer
;; nyan-mode
;; cider
