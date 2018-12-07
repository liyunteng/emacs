;;; my-lsp.el --- lsp                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  liyunteng

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-response-timeout 2)
  (setq lsp-message-project-root-warning nil)
  )
;; (use-package lsp-ui
;;   :ensure t)

(use-package lsp-clangd
  :ensure t
  :config
  (add-hook 'c-mode-hook 'lsp-clangd-c-enable)
  (add-hook 'c++-mode-hook 'lsp-clangd-c++-enable))

(use-package lsp-go
  :ensure t
  :config
  (add-hook 'go-mode-hook 'lsp-go-enable))

;; (use-package company
;;   ;; :diminish company-mode		;; for see current backend
;;   :ensure t
;;   :bind
;;   (
;;    ("TAB" . company-indent-or-complete-common)
;;    :map company-mode-map
;;    ("C-M-/" . company-other-backend)
;;    ("C-M-?" . company-begin-backend)
;;    ("M-SPC" . company-other-backend)
;;    ("M-/" . hippie-expand)
;;    ;; ("TAB" . company-indent-or-complete-common)
;;    :map company-active-map
;;    ("TAB" . company-complete-common)
;;    ("C-w" . nil)
;;    ("C-l" . company-show-location)
;;    ("C-n" . company-select-next)
;;    ("C-p" . company-select-previous)
;;    ("C-v" . company-next-page)
;;    ("C-d" . company-show-doc-buffer)
;;    ("M-v" . company-previous-page)
;;    :map company-search-map
;;    ("C-n" . company-select-next)
;;    ("C-p" . company-select-previous)
;;    )
;;   :commands (global-company-mode company-mode)
;;   :init
;;   (my|add-toggle company-mode
;;     :mode company-mode
;;     :documentation "Company mode")

;;   (global-company-mode +1)

;;   :config
;;   (defun company-manual-begin ()
;;     (interactive)
;;     (company-assert-enabled)
;;     (setq company--manual-action t)
;;     (unwind-protect
;; 	    (let ((company-minimum-prefix-length 1))
;; 	      (or company-candidates
;; 	          (company-auto-begin)))
;;       (unless company-candidates
;; 	    (setq company--manual-action nil))))

;;   (setq company-show-numbers t
;;         ;; company-lighter-base "ac"
;;         ;; company-search-filtering t
;;         company-auto-complete t
;;         company-minimum-prefix-length 2
;;         company-idle-delay 0
;;         company-tooltip-idle-delay 0.5
;;         company-transformers '(company-sort-by-backend-importance
;;                                company-sort-by-occurrence
;;                                company-sort-prefer-same-case-prefix)
;;         company-tooltip-limit 10
;;         company-tooltip-align-annotations t
;;         company-tooltip-flip-when-above t
;;         company-abort-manual-when-too-short t
;;         company-selection-wrap-around nil
;;         )

;;   ;; Suspend page-break-lines-mode while company menu is active
;;   ;; (see https://github.com/company-mode/company-mode/issues/416)
;;   (when (boundp 'page-break-lines-mode)
;;     (defvar my---page-break-lines-on-p nil)
;;     (make-variable-buffer-local 'my--page-break-lines-on-p)

;;     (defun my--page-break-lines-disable (&rest ignore)
;;       (when (setq my--page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
;;         (page-break-lines-mode -1)))

;;     (defun my--page-break-lines-maybe-reenable (&rest ignore)
;;       (when my--page-break-lines-on-p (page-break-lines-mode 1)))

;;     (add-hook 'company-completion-started-hook 'my--page-break-lines-disable)
;;     (add-hook 'company-completion-finished-hook 'my--page-break-lines-maybe-reenable)
;;     (add-hook 'company-completion-cancelled-hook 'my--page-break-lines-maybe-reenable))
;;   )

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))
(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

(provide 'my-lsp)
;;; my-lsp.el ends here
