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

(my-require-package 'company)
(my-require-package 'company-quickhelp)
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)

(require 'company)
(global-company-mode -1)
(when (display-graphic-p)
  (after-load 'company
    (require 'company-quickhelp)
    ;; maybe crash
    ;; (setq-default company-quickhelp-delay 1)
    ;; (setq-default company-quickhelp-max-lines 30)
    ;; (company-quickhelp-mode 1)
    (define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin)
    ))


;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines-mode
    (defvar sanityinc/page-break-lines-on-p nil)
    (make-variable-buffer-local 'sanityinc/page-break-lines-on-p)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-completion-finished-hook 'sanityinc/page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook 'sanityinc/page-break-lines-maybe-reenable)))

(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
(setq company-search-filtering t)
(setq company-auto-complete t)
(setq company-idle-delay 1)
(setq company-tooltip-idle-delay 0.5)
(setq company-transformers '(company-sort-by-backend-importance company-sort-by-occurrence))
(setq company-tooltip-limit 10)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)
(setq company-abort-manual-when-too-short t)
(setq company-selection-wrap-around nil)

(define-key company-mode-map (kbd "C-M-/") 'company-other-backend)
(define-key company-mode-map (kbd "C-M-?") 'company-begin-backend)
(define-key company-mode-map (kbd "M-/") 'hippie-expand)
(define-key company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
(define-key company-active-map (kbd "TAB") 'company-complete-common)
(define-key company-active-map (kbd "C-w") nil)
(define-key company-active-map (kbd "C-l") 'company-show-location)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-v") 'company-next-page)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

(define-key company-active-map (kbd "M-v") 'company-previous-page)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; copy from spacemacs
(defvar my-default-company-backends
  '((company-yasnippet)
    (company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")
(defvar auto-completion-enable-snippets-in-popup t)

(defmacro my|defvar-company-backends (mode)
  "Define a MODE specific company backend variable with default backends.
The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     ',my-default-company-backends
     ,(format "Company backend list for %S" mode)))


(defun my--show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            ;; '(:with company-yasnippet)
            )))

(defmacro my|enable-company (mode)
  "Enable company for the given MODE.
MODE must match the symbol passed in `my|defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my--init-company-%S" mode)))
        (backend-list (intern (format "company-backends-%S" mode))))
    `(progn
       (defun ,func ()
         ,(format "Initialize company for %S" mode)
         (when auto-completion-enable-snippets-in-popup
           (setq ,backend-list (mapcar 'my--show-snippets-in-company
                                       ,backend-list)))
         (set (make-variable-buffer-local 'auto-completion-front-end)
              'company)
         (set (make-variable-buffer-local 'company-backends)
              ,backend-list))
       (add-hook ',mode-hook ',func t)
       (add-hook ',mode-hook 'company-mode t))))

(defmacro my|disable-company (mode)
  "Disable company for the given MODE.
MODE parameter must match the parameter used in the call to
`my|enable-company'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my--init-company-%S" mode))))
    `(progn
       (remove-hook ',mode-hook ',func)
       (remove-hook ',mode-hook 'company-mode))))


(my|defvar-company-backends c-mode)
(my|defvar-company-backends c++-mode)
(push '(company-semantic company-clang) company-backends-c-mode)
(push '(company-semantic company-clang) company-backends-c++-mode)
(my|enable-company c-mode)
(my|enable-company c++-mode)

(my|defvar-company-backends cmake-mode)
(push 'company-cmake company-backends-cmake-mode)
(my|enable-company cmake-mode)

(my|defvar-company-backends go-mode)
(my-require-package 'company-go)
(after-load 'go-mode
  (require 'company-go))
(push 'company-go company-backends-go-mode)
(my|enable-company go-mode)

(my|defvar-company-backends sh-mode)
(my-require-package 'company-shell)
(after-load 'sh-mode
  (require 'company-shell))
(push 'company-shell company-backends-sh-mode)
(push 'company-shell-env company-backends-sh-mode)
(my|enable-company sh-mode)

(my|defvar-company-backends php-mode)
(my-require-package 'company-php)
(after-load 'php-mode
  (require 'company-php))
(push 'company-php company-backends-php-mode)
(my|enable-company php-mode)

(my|defvar-company-backends css-mode)
(push 'company-css company-backends-css-mode)
(my|enable-company css-mode)

(my|defvar-company-backends nxml-mode)
(push 'company-nxml company-backends-nxml-mode)
(my|enable-company nxml-mode)

(my|defvar-company-backends web-mode)
(my-require-package 'company-web)
(after-load 'web-mode
  (require 'company-web))
(push 'company-web company-backends-web-mode)
(my|enable-company web-mode)

(my|defvar-company-backends emacs-lisp-mode)
(push 'company-capf company-backends-emacs-lisp-mode)
(my|enable-company emacs-lisp-mode)

(my|defvar-company-backends java-mode)
(push 'company-eclim company-backends-java-mode)
(my|enable-company java-mode)

(my|defvar-company-backends objc-mode)
(my|defvar-company-backends idl-mode)
(my|defvar-company-backends pike-mode)
(my|defvar-company-backends awk-mode)
(my|defvar-company-backends markdown-mode)
(my|defvar-company-backends LaTex-mode)
(my|defvar-company-backends js2-mode)

(my|disable-company term-mode)
(my|disable-company shell-mode)

;; (my|defvar-company-backends python-mode)
;; (my|defvar-company-backends inferior-python-mode)

(provide 'my-ac)
;;; my-ac.el ends here
