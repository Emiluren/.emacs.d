(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (let ((mark-was-active mark-active))
    (exchange-point-and-mark)
    (unless mark-was-active
      (deactivate-mark))))

(defun find-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun find-todo-file ()
  (interactive)
  (find-file "~/.emacs.d/personal-org/emacs_todo.org"))

;; Found on http://emacs-fu.blogspot.com/
(defun ido-sudo-find-file ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable
by user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (find-file
     (if (file-writable-p file)
         file
       (concat "/sudo::" file)))))

(with-eval-after-load 'company
  (defun company-complete-common-or-selection ()
    "Insert the common part of all candidates, or selection if pressed again."
    (interactive)
    (when (company-manual-begin)
      (let ((tick (buffer-chars-modified-tick)))
        (call-interactively 'company-complete-common)
        (when (eq tick (buffer-chars-modified-tick))
          (call-interactively 'company-complete-selection))))))

(defun focus-gdb-buffer-when-stopped (gdb-result)
  (require 'bindat)
  (unless (and (fboundp 'bindat-get-field)
	       (string-equal (bindat-get-field gdb-result 'reason)
			     "exited-normally"))
    (raise-frame)
    ;; This is overwritten immediately by the source buffer
    ;; so not the best solution
    (require 'gdb-mi)
    (when (fboundp 'gdb-get-buffer-create)
      (switch-to-buffer (gdb-get-buffer-create 'gdb-inferior-io)))
    (when (fboundp 'gdb-display-gdb-buffer)
      (gdb-display-gdb-buffer))))

;; TODO: Maybe change this to only use yank
;; seems to mess up yank pop
(defun kill-region-yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (setq this-command 'yank)
  (let ((yank-text (current-kill 0)))
    (when (region-active-p)
      (call-interactively #'kill-region))
    (insert-for-yank yank-text))
  (when mark-active
    (call-interactively 'indent-region)))

(defun swap-windows ()
  "Swap the buffer in the current window with the one in the next."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-buffer (window-buffer (next-window))))
    (set-window-buffer (selected-window) next-buffer)
    (set-window-buffer (next-window) this-buffer)
    (select-window (next-window))))

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; Used in case eshell locks up
;; (because of something with the prompt regexp I guess?
(defun force-erase-buffer ()
  "Force delete all text in the buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun force-kill-current-buffer ()
  "Force kill current buffer"
  (interactive)
  (let ((inhibit-read-only))))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun buffer-file-name= (name &optional process-fun)
  (let ((f (or process-fun #'file-name-nondirectory)))
    (and buffer-file-name
         (string= (funcall f buffer-file-name) name))))

;; Add stumpwm-mode to eval expressions in stumpish
(add-to-list 'load-path "~/.stumpwm.d/modules/util/swm-emacs")
(defun turn-on-stumpwm-mode-for-init-file ()
  (when (buffer-file-name= "init.lisp" #'abbreviate-file-name)
    (require 'stumpwm-mode)
    (when (fboundp 'stumpwm-mode)
      (stumpwm-mode 1))))

;; Disable flycheck for .dir-local files
(defun dirlocals-flycheck-fix ()
  (when (buffer-file-name= ".dir-locals.el")
    (flycheck-mode -1)))

;; Necessary since paredit ignores delete-active-region
(defun paredit-backward-delete-or-delete-region (&optional arg)
  (interactive "P")
  (require 'paredit)
  (if (and (fboundp 'paredit-delete-region)
	   delete-active-region
	   (region-active-p))
      (paredit-delete-region (region-beginning) (region-end))
    (when (fboundp 'paredit-backward-delete)
      (paredit-backward-delete arg))))

(provide 'my-functions)
