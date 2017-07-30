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
(global-set-key (kbd "C-x j c") 'avy-goto-char)
(global-set-key (kbd "C-x j w") 'avy-goto-word-1)
(global-set-key (kbd "C-x j s") 'avy-goto-symbol-1)
(global-set-key (kbd "C-x j .") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-x j l") 'avy-goto-line)
(global-set-key (kbd "C-x j m") 'avy-move-line)
(global-set-key (kbd "C-x j p") 'avy-copy-line)
(global-set-key (kbd "C-x j b") 'avy-pop-mark)
(global-set-key (kbd "C-x j r") 'avy-resume)
(define-key isearch-mode-map (kbd "C-j") 'avy-isearch)

(provide 'my-avy)
;;; my-avy.el ends here
