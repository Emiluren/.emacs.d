(require 'rtags)
(require 'company)
(require 'gud)
(require 'gdb-mi)

(use-package rtags
  :defer t)
(use-package company-rtags
  :defer t
  :after (company rtags))
(use-package flycheck-rtags
  :defer t
  :after (flycheck rtags))
(use-package cmake-ide
  :defer t)
(use-package cmake-mode
  :defer t)

(setq rtags-path
      (format "%srtags-%s/bin/"
              (rtags-package-install-path)
              rtags-package-version))

(unless (file-exists-p rtags-path)
  (when (y-or-n-p "RTags has not been compiled. Do you want to do that now?")
    (rtags-install)))

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

(cmake-ide-setup)

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

(provide 'my-c++-settings)
