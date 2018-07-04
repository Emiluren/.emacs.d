
;; TODO: move all of this to package-config


;; Enable saving of minibuffer history
(savehist-mode 1)

;; Delete selected text when entering new if region is active
(delete-selection-mode 1)

;; Set up recentf
(require 'recentf)
(recentf-mode 1)

;; Set up highlighting of cursor/line
(blink-cursor-mode -1)
;; (global-hl-line-mode 1)

;; Enable fish completion in shell and eshell
;; TODO: I got this message in eshell
;; "Completion function pcomplete-completions-at-point uses a
;; deprecated calling convention"
;; Is that because of this? (seems not to be, still not working)

;; Seems to mess some stuff up with eshell
;; (when (and (executable-find "fish")
;;            (require 'fish-completion nil t))
;;   (global-fish-completion-mode))

