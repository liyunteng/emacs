;;; my-javascript.el --- javascript                  -*- lexical-binding: t; -*-

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

;; js2-mode
(use-package js2-mode
  :ensure t
  :defer t
  :commands (js2-mode js2-minor-mode js2-jsx-mode2)
  :init
  (defcustom preferred-javascript-mode
    (first (remove-if-not #'fboundp '(js2-mode js-mode)))
    "Javascript mode to use for .js files."
    :type 'symbol
    :group 'programming
    :options '(js2-mode js-mode))

  (defconst preferred-javascript-indent-level 2)

  ;; Need to first remove from list if present, since elpa adds entries too, which
  ;; may be in an arbitrary order
  (eval-when-compile (require 'cl))
  (setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
			                  (loop for entry in auto-mode-alist
				                    unless (eq preferred-javascript-mode (cdr entry))
				                    collect entry)))

  (setq js-indent-level preferred-javascript-indent-level)

  ;; Change some defaults: customize them to override
  (setq js2-basic-offset 2
	    js2-bounce-indent-p nil
	    js2-mode-show-parse-errors nil
	    js2-mode-show-strict-warnings nil
	    )

  :config
  (js2-imenu-extras-setup)

  (defun my-disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))

  (add-hook 'js2-mode-hook 'my-disable-js2-checks-if-flycheck-active)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

  ;; Javascript nests {} and () a lot, so I find this helpful

  (when (and (executable-find "ag"))
    (after-load 'js2-mode
      (define-key js2-mode-map (kbd "M-.") nil)
      (add-hook 'js2-mode-hook
		        (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))



;;; Coffeescript

  (after-load 'coffee-mode
    (setq-default coffee-js-mode preferred-javascript-mode
		          coffee-tab-width preferred-javascript-indent-level))

  (when (fboundp 'coffee-mode)
    (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

  ;; ---------------------------------------------------------------------------
  ;; Run and interact with an inferior JS via js-comint.el
  ;; ---------------------------------------------------------------------------

  (setq-default inferior-js-program-command "js")
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode))
  )

(use-package xref-js2
  :ensure t
  :after js2-mode)

(use-package js-comint
  :ensure t
  :after js2-mode)

(use-package skewer-mode
  :ensure t
  :after js2-mode
  :config
  (add-hook 'skewer-mode-hook
	        (lambda () (inferior-js-keys-mode -1))))
(provide 'my-javascript)
;;; my-javascript.el ends here
