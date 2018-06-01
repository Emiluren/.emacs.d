;; Enable saving of minibuffer history
(savehist-mode 1)

;; Delete selected text when entering new if region is active
(delete-selection-mode 1)

;; Show what keys can be pressed in the middle of a sequence
(which-key-mode 1)

;; Set up recentf
(require 'recentf)
(recentf-mode 1)

;; Set up highlighting of cursor/line
(blink-cursor-mode -1)
(global-hl-line-mode 1)

;; Show git diff in fringe
(global-git-gutter-mode 1)

;; Make undo easier to use
(global-undo-tree-mode)

;; Enable Interactively Do Things
(require 'ido)
(ido-mode t)

;; Enable fish completion in shell and eshell
;; TODO: I got this message in eshell
;; "Completion function pcomplete-completions-at-point uses a
;; deprecated calling convention"
;; Is that because of this?
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(flycheck-julia-setup)
