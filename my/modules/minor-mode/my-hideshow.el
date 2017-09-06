;;; my-hideshow.el --- hideshow                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
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

(use-package hideshow
  :ensure t
  :commands (hs-minor-mode)
  :bind-keymap ("C-c m" . hs-command-prefix)
  :bind (:map hs-command-prefix
			  ("h" . hs-hide-block)
			  ("s" . hs-show-block)
			  ("H" . hs-hide-all)
			  ("S" . hs-show-all)
			  ("l" . hs-hide-level)
			  ("m" . hs-toggle-hiding)
			  ("i" . hs-hide-initial-comment-block)
			  ([shfit mouse-1] . hs-mouse-toggle-hiding)
			  )
  :init
  (defvar hs-command-prefix)
  (define-prefix-command 'hs-command-prefix)

  (dolist (hook '(prog-mode-hook
				  nxml-mode
				  html-mode
				  web-mode))
	(add-hook hook 'hs-minor-mode))
  )

;;

;;代码折叠



;; (define-key hs-minor-mode-map [(shift mouse-2)] 'hs-mouse-toggle-hiding)

(provide 'my-hideshow)
;;; my-hideshow.el ends here
