;;; my-smartparens.el --- my smartparnes             -*- lexical-binding: t; -*-

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

(use-package smartparens
  :ensure t
  :commands (smartparens-mode
	     smartparens-strict-mode
	     smartparens-global-mode
	     smartparens-global-strict-mode)
  :bind (:map smartparens-mode-map
	      ("M-S" . sp-split-sexp)
              ("M-D" . sp-splice-sexp))

  :init
  (setq sp-show-pair-delay 0.2
	sp-show-pair-from-inside t
	sp-cancel-autoskip-on-backward-movement t
	sp-highlight-pair-overlay t
	sp-highlight-wrap-overlay t
	sp-highlight-wrap-tag-overlay t)
  (setq sp-base-key-bindings 'smartparens
	sp-autoskip-closing-pair 'always-end
	sp-hybrid-kill-entire-symbol nil
	blink-matching-paren nil)

  (my|add-toggle smartparens
    :mode smartparens-mode
    :documentation "Enable smartparens.")

  (my|add-toggle smartparens-strict-mode
    :mode smartparens-strict-mode
    :documentation "Enable smartparens strict.")

  (add-hook 'prog-mode-hook 'my/toggle-smartparens-on)
  (show-smartparens-global-mode +1)

  :config
  (require 'smartparens-config)
  ;; (sp-use-paredit-bindings)
  (sp-use-smartparens-bindings)
  (diminish 'smartparens-mode
            '(" sp" (:eval (if smartparens-strict-mode "/s"))))

  (defun my--conditionally-enable-smartparens-mode ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
    (if (or (eq this-command 'eval-expression)
	    (eq this-command 'pp-eval-expression)
	    (eq this-command 'eldoc-eval-expression)
            (eq this-command 'helm-eval-expression)
            (eq this-command 'edebug-eval-expression)
            (eq this-command 'debugger-eval-expression))
	(smartparens-mode)))
  (add-hook 'minibuffer-setup-hook 'my--conditionally-enable-smartparens-mode)

  (defun my--smartparens-pair-newline-and-indent (id action context)
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))
  (defun my/smart-closing-parenthesis ()
    (interactive)
    (let* ((sp-navigate-close-if-unbalanced t)
	   (current-pos (point))
	   (current-line (line-number-at-pos current-pos))
	   (next-pos (save-excursion
		       (sp-up-sexp)
		       (point)))
	   (next-line (line-number-at-pos next-pos)))
      (cond
       ((and (= current-line next-line)
	     (not (= current-pos next-pos)))
	(sp-up-sexp))
       (t
	(insert-char ?\))))))

  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
		   :unless '(sp-in-string-p)
		   :post-handlers '(((lambda (&rest _ignored)
				       (just-one-space)
				       (save-excursion (insert " ")))
				     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))

  (sp-pair "{" "}"
	   :unless '(sp-in-comment-p sp-in-string-p)
	   :post-handlers
	   '(:add (my--smartparens-pair-newline-and-indent "RET")))
  (sp-pair "(" ")"
	   :unless '(sp-in-comment-p sp-in-string-p)
	   :post-handlers
	   '(:add (my--smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" "]"
	   :unless '(sp-in-comment-p sp-in-string-p)
	   :post-handlers
	   '(:add (my--smartparens-pair-newline-and-indent "RET")))

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; (define-key smartparens-mode-map (kbd ")") 'my/smart-closing-parenthesis)
  ;; (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") nil)
  (define-key smartparens-mode-map (kbd "C-M-e") nil)
  (define-key smartparens-mode-map (kbd "C-w") 'sp-backward-kill-word)
  ;; (define-key smartparens-mode-map [remap backward-delete-char] 'sp-backward-delete-char)
  ;; (define-key smartparens-mode-map [remap backward-kill-word] 'sp-backward-kill-word)
  ;; (define-key smartparens-mode-map [remap backward-kill-sexp] 'sp-backward-kill-sexp)
  )


(provide 'my-smartparens)
;;; my-smartparens.el ends here
