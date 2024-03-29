;;; spade-mode.el --- Support for the Spade hardware description language
;; Author: Emil Segerbäck
;; Copyright (C) 2021, Emil Segerbäck
;; Created: 2021-10-19
;; Keywords: languages spade
;; URL: https://github.com/Emiluren/.emacs.d/tree/master/lisp/spade-mode

;; This file is not part of GNU Emacs.

;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Code:

(require 'generic)
(require 'rust-mode)

(defvar electric-indent-chars)

(defvar spade-keywords
  '("entity" "fn" "reg" "let" "reset" "inst" "enum" "decl" "struct" "use" "as" "mod" "stage" "pipeline" "assert" "match" "if" "else"))

(defvar spade-constants "false\\|true\\|[0-9]+")

(defvar spade-tab-width 4)

(defvar spade-font-lock-defaults
  `((;; stuff between double quotes
     ("\"\\.\\*\\?" . font-lock-string-face)
     (,spade-constants . font-lock-constant-face)
     ("bool\\|int\\|clk" . font-lock-type-face)
     (,(regexp-opt spade-keywords 'symbols) . font-lock-keyword-face)
     )))

(define-derived-mode spade-mode prog-mode "Spade"
  "A mode for Spade files, a hardware description language"

  (setq-local font-lock-defaults spade-font-lock-defaults)

  (setq-local tab-width spade-tab-width)

  (setq-local indent-line-function 'rust-mode-indent-line)

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Auto indent on }
  (setq-local electric-indent-chars
              (cons ?} (and (boundp 'electric-indent-chars)
                            electric-indent-chars)))

  (modify-syntax-entry ?/ "< 1b" spade-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" spade-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.spade\\'" . spade-mode))

;;; spade-mode.el ends here
