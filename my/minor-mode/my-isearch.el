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

(use-package anzu
  :ensure t
  :bind (([remap query-replace-regexp]. anzu-query-replace-regexp)
		 ([remap query-replace] . anzu-query-replace))
  :diminish anzu-mode
  ;; :defines anzu-mode-line-update-function anzu--state
  :init
  ;; (defun my-anzu-update-mode-line (here total)
  ;;   "Custom update function which does not propertize the status."
  ;;   (when anzu--state
  ;; 	(let ((status (cl-case state--anzu
  ;; 					(search (format "(%s/%d%s) "
  ;; 									(anzu--format-here-position here total)
  ;; 									total (if anzu--overflow-p "+" "")))
  ;; 					(replace-query (format "(%s/%d replace) "
  ;; 										   (anzu--format-here-position here total)
  ;; 										   total))
  ;; 					(replace (format "(%s/%d replace) "
  ;; 									 (anzu--format-here-position here total)
  ;; 									 total))
  ;; 					;; (replace-query (format "(%d replace) " total))
  ;; 					;; (replace (format "(%d/%d) " here total))
  ;; 					)))
  ;; 	  status)))
  ;; (setq anzu-mode-line-update-function 'my-anzu-update-mode-line)
  (global-anzu-mode +1))

(use-package isearch
  :bind  (
		  :map isearch-mode-map
			   ("C-o" . isearch-occur)
			   ("C-q" . isearch-del-char)
			   ;; ("C-w" . isearch-delete-char)
			   ("C-w" . back-kill-word)

			   ("C-y" . isearch-yank-word-or-char)
			   ("M-l" . isearch-yank-line)
			   ("M-w" . isearch-yank-kill)
			   ("C-M-w" . my/isearch-yank-symbol)
			   ("C-M-y" . isearch-yank-pop)

			   ("C-e" . isearch-edit-string)

			   ("M-i" . isearch-toggle-case-fold)
			   ("M-r" . isearch-toggle-regexp)

			   ([(control return)] . my/isearch-exit-other-end)
			   )
  :config
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
  )

(provide 'my-isearch)
;;; my-isearch.el ends here
