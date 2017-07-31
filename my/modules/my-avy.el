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
(my-require-package 'avy)
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)
(defvar avy-command-prefix)
(define-prefix-command 'avy-command-prefix)
(global-set-key (kbd "C-x j") 'avy-command-prefix)
(define-key avy-command-prefix (kbd "c") 'avy-goto-char)
(define-key avy-command-prefix (kbd "w") 'avy-goto-word-1)
(define-key avy-command-prefix (kbd "s") 'avy-goto-symbol-1)
(define-key avy-command-prefix (kbd ".") 'avy-goto-word-or-subword-1)
(define-key avy-command-prefix (kbd "l") 'avy-goto-line)
(define-key avy-command-prefix (kbd "m") 'avy-move-line)
(define-key avy-command-prefix (kbd "p") 'avy-copy-line)
(define-key avy-command-prefix (kbd "b") 'avy-pop-mark)
(define-key avy-command-prefix (kbd "r") 'avy-resume)
(define-key isearch-mode-map (kbd "C-j") 'avy-isearch)

(provide 'my-avy)
;;; my-avy.el ends here
