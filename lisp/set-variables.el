;; Keep closing paren for argument list indented to previous level
(c-add-style "my-c-style"
             '("linux"
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)	; Don't use tabs
	       (c-offsets-alist
		;; Keep the closing brace previous indentation
		(arglist-close . 0))))

(require 'em-dirs) ; Needed for eshell/pwd

(setq
 ;; Keep backup files in a separate folder
 backup-directory-alist '(("." . "~/.emacs.d/backups/"))
 ;; Use eww to browse web pages by default
 browse-url-browser-function 'eww-browse-url
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "my-c-style"))
 column-number-mode t ; Enable column number in modeline
 ;; Company seems to work poorly with sly and gud/gdb
 ;; TODO: check with sly again
 company-global-modes '(not gud-mode lisp-mode sly-mrepl-mode)
 confirm-nonexistent-file-or-buffer nil ; Don't ask for confirmation when creating new buffers
 dabbrev-case-fold-search t ; Make dabbrev case sensitive
 electric-indent-inhibit t ; Stop electric indent from indenting the previous line
 ;; Use a separate line for eshell working directory
 ;; Seems to cause some sort of problem with the history though
 eshell-hist-ignoredups t
 eshell-cmpl-ignore-case t
 eshell-prompt-regexp "[#$] "
 ;; To make sudo work better in eshell
 eshell-prefer-lisp-functions t
 eshell-prompt-function
 (lambda ()
   (concat (abbreviate-file-name (eshell/pwd))
           (if (= (user-uid) 0) "\n# " "\n$ ")))
 gdb-display-io-nopopup t ; Stop io buffer from popping up when the program outputs anything
 html-quick-keys nil ; prevent C-c X bindings when using sgml-quick-keys
 ido-enable-flex-matching t ; Fuzzy matching
 ido-auto-merge-work-directories-length -1 ; And disable annoying auto file search
 ido-create-new-buffer 'always ; Create new buffers without confirmation
 inferior-lisp-program "sbcl" ; Use sbcl for CL repls
 magit-delete-by-moving-to-trash nil ; Delete files directly from magit
 minibuffer-auto-raise t ; Focus Emacs if minibuffer activates
 ;; Set up path and stuff for org-mode
 org-directory "~/.emacs.d/personal-org/"
 org-default-notes-file (concat org-directory "/notes.org")
 recentf-max-menu-items 25
 ;; Push clipboard contents from other programs to kill ring also
 save-interprogram-paste-before-kill t
 sgml-quick-keys t ; Make characters in html behave electrically
 ;; Make Emacs split window horizontally by default
 split-height-threshold nil
 split-width-threshold 120
 tab-always-indent 'complete		; Use tab to complete
 ;; Faster than the default scp (according to Emacs wiki)
 tramp-default-method "ssh"
 )

;; Set up gnus
(setq gnus-directory "~/.emacs.d/mail"
      message-directory "~/.emacs.d/mail"
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nnimap "Skolmail"
					      (nnimap-address "outlook.office365.com")
					      (nnimap-server-port 993)
					      (nnimap-stream ssl)))
      ;; Make sure emails end up in sent folder after they have been sent
      ;; TODO: not working?
      ;; gnus-message-archive-group "nnimap+Skolmail:Skickade objekt"
      ;; Settings for sending email
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)

(setq-default
 ;; Make line wraps happen at word boundaries
 word-wrap t
 dired-omit-mode t ; Hide dotfiles, backup files and autosave files in dired
 )

(with-eval-after-load 'dired-x
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

;; Use y/n instead of longer yes/no
(fset 'yes-or-no-p 'y-or-n-p)
