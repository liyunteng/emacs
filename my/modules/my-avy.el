;;; my-avy.el --- avy                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope thhkat it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(use-package avy
  :ensure t
  :bind-keymap ("C-x j" . avy-command-prefix)
  :bind (:map avy-command-prefix
			  ("j" . avy-goto-char)
			  ("c" . avy-goto-char)
			  ("w" . avy-goto-word-1)
			  ("s" . avy-goto-symbol-1)
			  ("." . avy-goto-word-or-subword-1)
			  ("l" . avy-goto-line)
			  ("m" . avy-move-line)
			  ("p" . avy-copy-line)
			  ("b" . avy-pop-mark)
			  ("r" . avr-resume)
			  :map isearch-mode-map
			  ("C-j" . avy-isearch)
			  )
  :init
  (defvar avy-command-prefix)
  (define-prefix-command 'avy-command-prefix)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  )

(provide 'my-avy)
;;; my-avy.el ends here
