(require 'my-functions)

;; TODO: move everything to package-config

;; Make sure dir-locals.el is reloaded if the major mode changes
(add-hook 'after-change-major-mode-hook 'hack-local-variables)

(add-hook 'prog-mode-hook
	  (lambda ()
	    ;; Don't line break
	    (setq truncate-lines t)))

;; Flycheck stuff
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Set up rust lsp stuff
(with-eval-after-load 'lsp-mode
  (require 'lsp-ui)
  (require 'lsp-rust)
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'rust-mode-hook #'lsp-rust-enable))

(require 'company-lsp)
(push 'company-lsp company-backends)

;; Pop up emacs frame, gdb buffer and io buffer on error
(add-hook 'gdb-stopped-functions #'focus-gdb-buffer-when-stopped)

;; dired-x is required for dired-omit-mode
(add-hook 'dired-load-hook (lambda () (require 'dired-x)))

;; Do not use dired-omit-mode for 'recover-session'
(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-omit-mode nil))
    ad-do-it))

;; TODO: Make sure this runs after sly for the keybindings to be correct
(add-hook 'lisp-mode-hook #'turn-on-stumpwm-mode-for-init-file)

(add-hook 'emacs-lisp-mode-hook #'dirlocals-flycheck-fix)

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;(add-hook 'sly-mode-hook              #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook    #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)
;;(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Does not work with Emacs 26 yet
;; (require 'clj-refactor)

(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
