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
  :bind ("C-x g" . 'magit-status))

(use-package org
  :bind
  (("C-c l" . 'org-store-link)
   ("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   ("C-c b" . 'org-switchb)))
