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

(after-load 'smartparens
  (setq sp-show-pair-delay 0.2
        sp-show-pair-form-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (sp-with-modes
   '(c++-mode objc-mode c-mode python-mode go-mode)
   (sp-local-pair "{" "}"
		  :unless '(sp-in-comment-p sp-in-string-p)
		  :post-handlers '(:add ("||" "RET"))))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
  (define-key smartparens-mode-map [remap backward-delete-char] 'sp-backward-delete-char)
  (define-key smartparens-mode-map [remap backward-kill-word] 'sp-backward-kill-word)
  (define-key smartparens-mode-map [remap backward-kill-sexp] 'sp-backward-kill-sexp)
  )

(require 'smartparens-config)
;; (sp-use-paredit-bindings)
(setq-default sp-base-key-bindings 'smartparens)
(setq-default sp-autoskip-closing-pair 'always)
(setq-default sp-hybrid-kill-entire-symbol nil)
(sp-use-smartparens-bindings)
(show-smartparens-global-mode +1)
(setq blink-matching-paren nil)
(provide 'my-smartparens)
;;; my-smartparens.el ends here
