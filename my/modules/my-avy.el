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
  :bind-keymap ("C-x j" . my--avy-command-prefix)
  :bind (:map my--avy-command-prefix
			  ("j" . avy-goto-char)
			  ("c" . avy-goto-char)
			  ("w" . avy-goto-word-1)
			  ("s" . avy-goto-symbol-1)
			  ("." . avy-goto-word-or-subword-1)
			  ("l" . avy-goto-line)
			  ("m" . avy-move-line)
			  ("p" . avy-copy-line)
			  ("b" . avy-pop-mark)
			  ("r" . avy-resume)
			  ("u" . my/avy-goto-url)
			  ("o" . my/avy-open-url)
			  :map isearch-mode-map
			  ("C-j" . avy-isearch)
			  )
  :init
  (defvar my--avy-command-prefix)
  (define-prefix-command 'my--avy-command-prefix)
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  (defun my/avy-goto-url()
    "Use avy to go to an URL in the buffer."
    (interactive)
    (avy--generic-jump "https?://" nil 'pre))
  (defun my/avy-open-url ()
    "Use avy to select an URL in the buffer and open it."
    (interactive)
    (save-excursion
      (my/avy-goto-url)
      (browse-url-at-point)))
  )

(provide 'my-avy)
;;; my-avy.el ends here
