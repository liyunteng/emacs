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

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)
(use-package company
  :ensure t
  :bind
  (("TAB" . company-indent-or-complete-common)
   :map company-mode-map
   ("C-M-/" . company-other-backend)
   ("C-M-?" . company-begin-backend)
   ("M-SPC" . company-other-backend)
   ("M-/" . hippie-expand)
   :map company-active-map
   ("TAB" . company-complete-common)
   ("C-w" . company-abort)
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
  (global-company-mode +1)

  :config
  (setq company-auto-commit t
        company-minimum-prefix-length 2
        company-idle-delay 2
        ;; company-show-numbers t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance
                               company-sort-prefer-same-case-prefix)

        company-tooltip-limit 20
        company-tooltip-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-abort-manual-when-too-short t
        company-selection-wrap-around nil
        )

  ;; Suspend page-break-lines-mode while company menu is active
  ;; (see https://github.com/company-mode/company-mode/issues/416)
  (with-eval-after-load 'page-break-lines
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
    (add-hook 'company-completion-cancelled-hook 'my--page-break-lines-maybe-reenable))

  (with-eval-after-load 'lsp-ui
    (defun my--lsp-ui-doc-hide (&rest ignore)
      (null ignore)
      (lsp-ui-doc-mode -1))
    (defun my--lsp-ui-doc-show (&rest ignore)
      (null ignore)
      (lsp-ui-doc-mode +1))
    (add-hook 'company-completion-started-hook 'my--lsp-ui-doc-hide)
    (add-hook 'company-completion-finished-hook 'my--lsp-ui-doc-show)
    (add-hook 'company-completion-cancelled-hook 'my--lsp-ui-doc-show))
  )

(use-package company-quickhelp
  :ensure t
  :after company
  :if (display-graphic-p)
  :bind
  (:map company-active-map
        ("C-h"  . company-quickhelp-mode))
  :init
  (company-quickhelp-mode +1)
  :config
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 0.5)
  (setq company-quickhelp-max-lines 30))


;; LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c o")
  (setq lsp-before-save-edits t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-idle-delay 0.500)

  (setq lsp-session-file (expand-file-name "lsp-session-v1" my-cache-dir))
  (setq lsp-server-install-dir (expand-file-name "lsp-server" my-cache-dir))

  (when (executable-find "clangd")
    ;; (setq lsp-clients-clangd-args '("--all-scopes-completion" "--clang-tidy" "--completion-style=detailed" "--suggest-missing-includes" "--background-index" "--header-insertion-decorators" "--log=verbose"))
    (defun my/clangd-generate-compile-commands ()
      (interactive)
      (let ((cmake (executable-find "cmake"))
            (cmakefile (file-exists-p "CMakeLists.txt"))
            (make (executable-find "make"))
            (bear (executable-find "bear"))
            (makefile (file-exists-p "Makefile")))
        (cond ((and cmake cmakefile)
               (progn (shell-command (format "%s -S . -BDebug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=YES" cmake))
                      (shell-command "ln -s Debug/compile_commands.json .")))
              ((and make bear makefile)
               (shell-command (format "%s %s" bear make)))
              (t
               (message "Failed")))
        ))
    (add-hook 'c-mode-common-hook 'lsp))

  ;; (when (executable-find "ccls")
  ;;   (use-package ccls
  ;;     :ensure t
  ;;     :init
  ;;     (add-hook 'c-mode-common-hook 'lsp)))

  (when  (executable-find "go-langserver")
    (add-hook 'go-mode-hook 'lsp))

  (when  (executable-find "pyls")
    (add-hook 'python-mode-hook 'lsp))

  ;; (use-package lsp-completion
  ;;   :init
  ;;   (setq lsp-completion-provider t))

  :config
  (setq lsp-log-io nil)
  (setq lsp-print-performance nil)
  ;; (setq lsp-log-max 20000)
  ;; (setq lsp-restart 'auto-restart)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-response-timeout 2)
  (setq lsp-document-sync-method lsp--sync-incremental)
  ;; (setq lsp-headerline-breadcrumb-enable t)
  (setq-default lsp-completion-provider t)
  (setq lsp-lens-enable t)

  ;; (setq lsp-keymap-prefix "C-c")


  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-semantic-highlighting-warn-on-missing-face t)
  (setq lsp-semantic-tokens-apply-modifiers t)

  (add-hook 'kill-emacs-hook 'lsp--global-teardown)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ("M-'" . lsp-ui-sideline-apply-code-actions))

  :config
  (setq
   ;; lsp-ui-doc-header t
   ;; lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-alignment 'window
   lsp-ui-doc-border "#b3b3b3"
   lsp-ui-doc-delay 0.8
   lsp-ui-doc-max-width 300
   lsp-ui-doc-max-height 50
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-code-actions nil)

  (defadvice recenter-top-bottom (after my-update-lsp-ui-doc-position activate)
    (when (and (lsp-ui-doc--frame-visible-p) lsp-ui-mode)
      (let ((lsp-ui-doc-delay 0))
        (lsp-ui-doc-hide)
        (lsp-ui-doc-show)))))


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

;; (my|enable-company c-mode-common '(company-lsp company-semantic company-clang))
(my|enable-company cmake-mode '(company-cmake))
(my|enable-company css-mode '(company-css))
(my|enable-company nxml-mode '(company-nxml))
(my|enable-company ielm-mode)
(my|enable-company inferior-emacs-lisp-mode)
(my|enable-company java-mode '(company-eclim))
(my|enable-company python-mode '(elpy-company-backend))
(my|enable-company inferior-python-mode '(elpy-company-backend))
(my|enable-company emacs-lisp-mode '(company-capf))
(my|enable-company lisp-interaction-mode '(company-capf))

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
