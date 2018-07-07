;; Keep closing paren for argument list indented to previous level
(c-add-style "my-c-style"
             '("linux"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil) ; Don't use tabs
               (c-offsets-alist
                ;; Keep the closing brace previous indentation
                (arglist-close . 0))))

(setq
 ;; Keep backup and auto save files in their own folders
 ;; Also place remote files in /tmp like default
 auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
				  (".*" ,(concat user-emacs-directory "backups/") t))
 backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups/")))
 backward-delete-char-untabify-method nil ; Don't convert tabs to spaces when deleting
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (csharp-mode . "my-c-style") ; csharp-mode will automatically override the style if we don't set it specifically
                   (other . "my-c-style"))
 calendar-week-start-day 1          ; Week starts on monday
 column-number-mode t               ; Enable column number in modeline
 confirm-kill-processes nil ; Don't ask for confirmation when closing a buffer that is attached to a process
 confirm-nonexistent-file-or-buffer nil ; Don't ask for confirmation when creating new buffers
 dabbrev-case-fold-search nil           ; Make dabbrev case sensitive
 electric-indent-inhibit t ; Stop electric indent from indenting the previous line
 gdb-display-io-nopopup t ; Stop io buffer from popping up when the program outputs anything
 history-delete-duplicates t
 html-quick-keys nil ; prevent C-c X bindings when using sgml-quick-keys
 inferior-lisp-program "sbcl"  ; Use sbcl for CL repls
 lazy-highlight-initial-delay 0 ; Don't wait before highlighting searches
 recentf-max-menu-items 150
 ;; Push clipboard contents from other programs to kill ring also
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil     ; Sentences end with a single space
 sgml-quick-keys t  ; Make characters in html behave electrically
 ;; Make Emacs split window horizontally by default
 split-height-threshold nil
 split-width-threshold 120
 tab-always-indent 'complete            ; Use tab to complete
 ;; Faster than the default scp (according to Emacs wiki)
 tramp-default-method "ssh"
 )

;; Set up gnus
(setq gnus-directory "~/.emacs.d/mail"
      message-directory "~/.emacs.d/mail"
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods '((nntp "news.gmane.org")
                                      (nnimap "Skolmail"
                                              (nnimap-address "outlook.office365.com")
                                              (nnimap-server-port 993)
                                              (nnimap-stream ssl)))
					;gnus-interactive-exit nil ; stop prompt but do I want it for updates or something?
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
