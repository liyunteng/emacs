;;; my-isearch.el --- my isearch                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

;; Author: liyunteng;; Show number of matches while searching <li_yunteng@163.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(my-require-package 'anzu)
(require 'anzu)
(global-anzu-mode +1)
(setq anzu-mode-lighter "")
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

(defun my-anzu-update-mode-line (here total)
  "Custom update function which does not propertize the status."
  (when anzu--state
    (let ((status (cl-case anzu--state
                    (search (format "(%s/%d%s)"
                                    (anzu--format-here-position here total)
                                    total (if anzu--overflow-p "+" "")))
                    (replace-query (format "(%d replace)" total))
                    (replace (format "(%d/%d)" here total)))))
      status)))
(setq anzu-mode-line-update-function 'my-anzu-update-mode-line)

;; Search back/forth for the symbol at point
;; See http://www.emacswiki.org/emacs/SearchAtPoint
(defun my/isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun my/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(my-require-package 'swiper)
(require 'swiper)

(global-set-key (kbd "M-s s") 'swiper)
;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "M-w") 'swiper-from-isearch)
(define-key isearch-mode-map [(control return)] 'my/isearch-exit-other-end)
(define-key isearch-mode-map "\C-\M-w" 'my/isearch-yank-symbol)
(define-key isearch-mode-map (kbd "C-q") 'isearch-del-char)
(define-key isearch-mode-map (kbd "C-w") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "M-i") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-word-or-char)
(define-key isearch-mode-map (kbd "C-M-y") 'isearch-yank-pop)
(define-key isearch-mode-map (kbd "M-l") 'isearch-yank-line)
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

(provide 'my-isearch)
;;; my-isearch.el ends here
