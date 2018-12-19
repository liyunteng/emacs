;;; my-ac.el --- auto completion                     -*- lexical-binding: t; -*-

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

;; TODO: compat with lsp
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)

(use-package company
  :ensure t
  :bind
  (
   :map company-mode-map
   ("TAB" . company-indent-or-complete-common)
   ("C-M-/" . company-other-backend)
   ("C-M-?" . company-begin-backend)
   ("M-SPC" . company-other-backend)
   ("M-/" . hippie-expand)
   ;; ("TAB" . company-indent-or-complete-common)
   :map company-active-map
   ("TAB" . company-complete-common)
   ("C-w" . nil)
   ("C-l" . company-show-location)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-v" . company-next-page)
   ("C-d" . company-show-doc-buffer)
   ("M-v" . company-previous-page)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :commands (global-company-mode company-mode company-auto-begin)
  :init
  ;; (defalias 'completion-at-point 'company-complete-common)

  (global-company-mode -1)

  :config
  ;; fix company-candidates-length is 0 will start company
  (defun company-manual-begin ()
    (interactive)
    (company-assert-enabled)
    (setq company--manual-action t)
    (unwind-protect
	    (let ((company-minimum-prefix-length 1))
	      (or company-candidates
	          (company-auto-begin)))
      (unless company-candidates
	    (setq company--manual-action nil))))

  (setq company-auto-complete t
        company-minimum-prefix-length 2
        company-idle-delay 1
        ;; company-show-numbers t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix)

        company-tooltip-limit 20
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-abort-manual-when-too-short t
        company-selection-wrap-around nil)

  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (when (fboundp 'page-break-lines-mode)
    (defvar my--page-break-lines-on-p nil)
    (make-local-variable 'my--page-break-lines-on-p)

    (defun my--page-break-lines-disable (&rest ignore)
      (null ignore)
      (when (setq my--page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun my--page-break-lines-maybe-reenable (&rest ignore)
      (null ignore)
      (when my--page-break-lines-on-p (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'my--page-break-lines-disable)
    (add-hook 'company-completion-finished-hook 'my--page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook 'my--page-break-lines-maybe-reenable)))

(use-package company-quickhelp
  :ensure t
  :after company
  :if (display-graphic-p)
  :bind
  (:map company-active-map
	    ("C-h"  . company-quickhelp-mode))
  :init
  (company-quickhelp-mode 1)
  :config
  ;; (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 0.5)
  (setq company-quickhelp-max-lines 30))


;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-response-timeout 2)
;;   (setq lsp-message-project-root-warning nil))

;; (use-package lsp-ui
;;   :ensure t
;;   :init
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package company-lsp
;;   :ensure t
;;   :init
;;   (push 'company-lsp company-backends))

;; ;; need clangd
;; (when (executable-find "clangd")
;;   (lsp-define-stdio-client lsp-clangd-c
;;                            "c"
;;                            (lsp-make-traverser ".projectile")
;;                            (list "clangd")
;;                            :ignore-regexps
;;                            '("^Error -[0-9]+: .+$"))
;;   (lsp-define-stdio-client lsp-clangd-c++
;;                            "cpp"
;;                            (lsp-make-traverser ".projectile")
;;                            (list "clangd")
;;                            :ignore-regexps
;;                            '("^Error -[0-9]+: .+$"))
;;   (add-hook 'c-mode-hook 'lsp-clangd-c-enable)
;;   (add-hook 'c++-mode-hook 'lsp-clangd-c++-enable))

;; ;; need go-langserver
;; (use-package lsp-go
;;   :ensure t
;;   :if (executable-find "go-langserver")
;;   :init
;;   (add-hook 'go-mode-hook 'lsp-go-enable))

;; ;; need python-language-server
;; (when (executable-find "pyls")
;;   (lsp-define-stdio-client lsp-python
;;                            "python"
;;                            (lsp-make-traverser ".projectile")
;;                            (list "pyls")
;;                            :ignore-regexps
;;                            '("^Error -[0-9]+: .+$"))
;;   (add-hook 'python-mode-hook 'lsp-python-enable))


;; copy from spacemacs
(defvar my-company-default-backends
  '(company-capf
    company-yasnippet
    (company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-oddmuse company-dabbrev)
  "The list of default company backends used.")

(defmacro my|enable-company (mode &optional backends)
  "Enable company for the given MODE.
MODE must match the symbol passed in `my|defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
	    (func (intern (format "my--init-company-%S" mode)))
	    (backend-list (intern (format "company-backends-%S" mode))))
    `(progn
       (defvar ,backend-list
         (append ,backends my-company-default-backends)
         ,(format "Company backend list for %S" mode))

       (defun ,func ()
	     ,(format "Initialize company for %S" mode)
	     (set (make-local-variable 'company-backends)
	          ,backend-list))

       (add-hook ',mode-hook ',func t)
       (add-hook ',mode-hook 'company-mode t)
       )))

(defmacro my|disable-company (mode)
  "Disable company for the given MODE.
MODE parameter must match the parameter used in the call to
`my|enable-company'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
	    (func (intern (format "my--init-company-%S" mode))))
    `(progn
       (remove-hook ',mode-hook ',func)
       (remove-hook ',mode-hook 'company-mode)
       )))

(my|enable-company c-mode-common '(company-semantic company-clang))
(my|enable-company cmake-mode '(company-cmake))
(my|enable-company css-mode '(company-css))
(my|enable-company nxml-mode '(company-nxml))
(my|enable-company emacs-lisp-mode)
(my|enable-company lisp-interaction-mode)
(my|enable-company ielm-mode)
(my|enable-company inferior-emacs-lisp-mode)
(my|enable-company java-mode '(company-eclim))
(my|enable-company python-mode '(elpy-company-backend))
(my|enable-company inferior-python-mode '(elpy-company-backend))

(use-package company-go
  :ensure t
  :defer t
  :commands (company-go))
(my|enable-company go-mode '(company-go))

(use-package company-shell
  :ensure t
  :defer t
  :commands (company-shell
	         company-shell-env))
(my|enable-company sh-mode '(company-shell company-shell-env))

(use-package company-php
  :ensure t
  :defer t
  :commands (company-php))
(my|enable-company php-mode '(company-php))

(use-package company-web
  :ensure t
  :defer t
  :commands (company-web-html company-web-jade company-web-slim))
(my|enable-company web-mode '(company-web-html company-web-jade company-web-slim))


(my|disable-company term-mode)
(my|disable-company shell-mode)
(my|disable-company eshell-mode)
(my|disable-company org-mode)
(my|disable-company org-agenda-mode)
(my|disable-company calendar-mode)


(provide 'my-ac)
;;; my-ac.el ends here
