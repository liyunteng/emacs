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
  :defer t
  :commands (go-mode)
  :bind (:map go-mode-map
	          ("C-c C-d" . godoc-at-point)  ; need go-tools's go doc
	          ("C-c C-p" . godoc)	    ; need godoc
	          ("C-M-\\" . gofmt)


	          ("C-c C-a" . go-import-add)
	          ("C-c C-q" . go-remove-unused-imports)

	          ("C-c RET" . my/smart-compile)
	          ("C-c C-c" . my/smart-compile)


	          ("M-." . godef-jump)
	          ("C-c C-l" . godef-describe)
	          ;; ("C-c C-b" . xref-pop-marker-stack)
	          )
  :init
  (use-package go-eldoc
    :ensure t
    :defer t
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  (use-package go-guru
    :ensure t
    :defer t
    :init
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
    )

  ;; (use-package gotest
  ;;   :ensure t
  ;;   :bind (:map go-mode-map
  ;; 		("C-c C-a" . go-test-current-project)
  ;; 		("C-c C-t" . go-test-current-file)
  ;; 		("C-c C-." . go-test-current-test)
  ;; 		("C-c C-c" . go-run)
  ;; 		)
  ;;   )


  :config
  ;; need go get golang.org/tools/cmd/goimports
  (if (executable-find "goimports")
      (setq gofmt-command "goimports"))

  (defun my-go-mode-hook ()
    (set (make-local-variable 'tab-width) 4)
    )
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(provide 'my-go)
;;; my-go.el ends here
