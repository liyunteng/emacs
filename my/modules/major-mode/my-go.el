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

(my-require-package 'go-mode)
(my-require-package 'go-eldoc)


(after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-a") 'go-test-current-project)
  (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c C-.") 'go-test-current-test)

  (define-key go-mode-map (kbd "C-h f") 'helm-apropos)
  (define-key go-mode-map (kbd "C-c C-d") 'godoc-at-point)
  (define-key go-mode-map (kbd "C-c d") 'godoc)
  (define-key go-mode-map (kbd "C-M-\\") 'gofmt)
  (define-key go-mode-map (kbd "C-c C-c") 'go-run)

  (define-key go-mode-map (kbd "C-c RET") 'go-import-add)
  (define-key go-mode-map (kbd "C-c SPC") 'go-remove-unused-imports)

  (define-key go-mode-map (kbd "C-c C-j") 'godef-jump)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c C-b") 'xref-pop-marker-stack)
  (define-key go-mode-map (kbd "C-c C-p") 'godef-describe)
  (define-key go-mode-map (kbd "C-c C-l") 'godef-describe)

  (defun my-go-mode-hook ()
    "My go mode hook."
    (progn
      (go-eldoc-setup)
      ))

  (add-hook 'go-mode-hook 'my-go-mode-hook))

(provide 'my-go)
;;; my-go.el ends here
