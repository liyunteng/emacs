;;; my-go.el --- my go                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

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


(use-package go-mode
  :ensure t
  :init
  (use-package go-eldoc
	:ensure t)
  (use-package gotest
	:ensure t)
  :bind (:map go-mode-map
			  ("C-c C-a" . go-test-current-project)
			  ("C-c C-t" . go-test-current-file)
			  ("C-c C-." . go-test-current-test)

			  ("C-h f" . helm-apropos)
			  ("C-c C-d" . godoc-at-point)
			  ("C-c d" . godoc)
			  ("C-M-\\" . gofmt)
			  ("C-c C-c" . go-run)

			  ("C-c RET" . go-import-add)
			  ("C-c SPC" . go-remove-unused-imports)

			  ("C-c j" . godef-jump)
			  ("C-c C-j" . godef-jump-other-window)
			  ("M-." . godef-jump)
			  ("C-c C-b" . xref-pop-marker-stack)
			  ("C-c C-p" . godef-describe)
			  ("C-c C-l" . godef-describe)
			  )

  :config
  (defun my-go-mode-hook ()
	"My go mode hook."
	(progn
	  (go-eldoc-setup)
	  ))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(provide 'my-go)
;;; my-go.el ends here
