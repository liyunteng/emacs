;;; my-company.el --- company                        -*- lexical-binding: t; -*-

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

(my-require-package 'company)
(my-require-package 'company-web)
(my-require-package 'company-shell)
(my-require-package 'company-php)
(my-require-package 'company-go)
(require 'company)
(require 'company-web)
(require 'company-shell)
(require 'company-php)
(require 'company-go)

;; (my-require-package 'company-quickhelp)
;; (require 'company-quickhelp)
;; (setq-default company-quickhelp-use-propertized-text t)
;; (setq-default company-quickhelp-delay 0.5)
;; (setq-default company-quickhelp-max-lines 30)
;; (company-quickhelp-mode -1)

(setq tab-always-indent 'complete)  ;; use 't when company is disabled
;; (add-to-list 'completion-styles 'initials t)
;; (add-to-list 'completion-styles 'substring t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold nil)

(defconst my-defaut-company-backends
  '((company-yasnippet)
    (company-capf)

    ;; (company-eclim
    ;;  company-oddmuse)

    ;; (company-auctex-macros
    ;;  company-auctex-symbols
    ;;  company-auctex-environments)

    ;; (company-gtags
    ;;  company-etags
    ;;  company-keywords)
    (company-files)
    (company-xcode)

    (company-dabbrev-code
     company-dabbrev
     company-abbrev)
    ))

;; add yasnippet to company default
(setq-default company-backends
              my-defaut-company-backends)

(defun my-company-set-backends (mode-hook backends)
  "My Company Set Backends, set MODE-HOOK's backends to BACKENDS."
  (add-hook mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append backends my-defaut-company-backends)))))


(my-company-set-backends 'emacs-lisp-mode-hook '(company-capf company-elisp))

(my-company-set-backends 'c-mode-hook '((company-clang) (company-semantic)))
(my-company-set-backends 'c++-mode-hook '((company-clang) (company-semantic)))

(my-company-set-backends 'go-mode-hook '((company-go)))
(my-company-set-backends 'sh-mode-hook '((company-shell company-shell-env)))
(my-company-set-backends 'php-mode-hook '((company-ac-php-backend)))

(my-company-set-backends 'message-mode-hook '((company-bbdb)))
(my-company-set-backends 'nxml-mode-hook '((company-nxml)))
(my-company-set-backends 'css-mode-hook '((company-css)))
(my-company-set-backends 'cmake-mode-hook '((company-cmake)))
(my-company-set-backends 'TeX-mode-hook '((company-auctex-macros
                                           company-auctex-symbols
                                           company-auctex-bibs
                                           company-auctex-environment)))

(my-company-set-backends 'web-mode-hook '((company-web-html company-web-jade company-web-slim)))
(my-company-set-backends 'html-mode-hook '((company-web-html company-web-jade company-web-slim)))
;; (my-company-set-backends 'jade-mode-hook '((company-web-html company-web-jade company-web-slim)))
(my-company-set-backends 'slim-mode-hook '((company-web-html company-web-jade company-web-slim)))


;; (setq company-frontends '(
;;                           ;; helm-company-frontend
;;                           ;; company-echo-frontend
;;                           ;; company-echo-strip-common-frontend
;;                           ;; company-echo-metadata-frontend
;;                           ;; company-preview-frontend
;;                           ;; company-preview-common-frontend
;;                           ;; company-preview-if-just-one-frontend
;;                           ;; company-pseudo-tooltip-frontend
;;                           ;; company-pseudo-tooltip-unless-just-one-frontend
;;                           ;; company-pseudo-tooltip-unless-just-one-frontend-with-delay
;;                           ))

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
(define-key company-active-map (kbd "M-v") 'company-previous-page)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(defcustom my-disable-company-modes-hook '(term-mode-hook
                                           org-mode-hook
                                           org-agenda-mode-hook
                                           shell-mode-hook
                                           eshell-mode-hook
                                           calendar-mode-hook)

  "non-nil values disable company mode in that mode."
  :type 'list
  :group 'prelude)

(dolist (hook my-disable-company-modes-hook)
  (add-hook hook
            (lambda ()
              (company-mode -1))))

(global-company-mode t)

(provide 'my-company)
;;; my-company.el ends here
