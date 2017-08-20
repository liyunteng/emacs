;;; my-yas.el --- yas

;; Copyright (C) 2014  liyunteng

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

(use-package yasnippet
  :ensure t
  :config
  (use-package dropdown-list
	:ensure t)

  ;; (defvar my-yas-dir (expand-file-name "snippets" user-emacs-directory))
  ;; (if (and (file-exists-p my-yas-dir)
  ;;          (not (member my-yas-dir yas-snippet-dirs)))
  ;;     (add-to-list 'yas-snippet-dirs my-yas-dir t))
  (setq yas-snippet-dirs (list 'yas-installed-snippets-dir yas--default-user-snippets-dir))
  ;; (yas-reload-all)

  ;; (yas-global-mode t)
  (dolist (hook '(prog-mode-hook
				  latex-mode-hook
				  plain-text-mode
				  nxml-mode-hook
				  html-mode-hook
				  web-mode

				  autoconf-mode-hook
				  conf-unix-mode-hook
				  cmake-mode-hook
				  css-mode-hook
				  m4-mode-hook
				  ;; org-mode-hook
				  makefile-gmake-mode-hook
				  sql-mode-hook
				  snippet-mode-hook
				  udev-mode-hook
				  ))
	(add-hook hook 'yas-minor-mode-on))

  (defun my/yas-reload-all ()
	"My yas-realod-all and compile."
	(interactive)
	(yas-compile-directory (file-truename my-yas-dir))
	(yas-reload-all)
	(yas-minor-mode 1))

  ;; (defun my-yas-field-to-statement(str sep)
  ;;   "If STR=='a.b.c' and SEP=' && ',
  ;; 'a.b.c' => 'a && a.b && a.b.c'"
  ;;   (let ((a (split-string str "\\.")) rlt)
  ;;     (mapconcat 'identity
  ;;                (mapcar (lambda (elem)
  ;;                          (cond
  ;;                           (rlt
  ;;                            (setq rlt (concat rlt "." elem)))
  ;;                           (t
  ;;                            (setq rlt elem)))) a)
  ;;                sep)))

  ;; (defun my-yas-get-first-name-from-to-field ()
  ;;   (let ((rlt "AGENT_NAME") str)
  ;;     (save-excursion
  ;;       (goto-char (point-min))
  ;;       ;; first line in email could be some hidden line containing NO to field
  ;;       (setq str (my-buffer-str)))
  ;;     ;; (message "str=%s" str)
  ;;     (if (string-match "^To: \"?\\([a-zA-Z]+\\)" str)
  ;;         (setq rlt (capitalize (match-string 1 str))))
  ;;     ;; (message "rlt=%s" rlt)
  ;;     rlt))

  ;; (defun my-yas-camelcase-to-string-list (str)
  ;;   "Convert camelcase string into string list"
  ;;   (let ((old-case case-fold-search)
  ;;         rlt)
  ;;     (setq case-fold-search nil)
  ;;     (setq rlt (replace-regexp-in-string "\\([A-Z]+\\)" " \\1" str t))
  ;;     (setq rlt (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]+\\)" "\\1 \\2" rlt t))
  ;;     ;; restore case-fold-search
  ;;     (setq case-fold-search old-case)
  ;;     (split-string rlt " ")))

  ;; (defun my-yas-camelcase-to-downcase (str)
  ;;   (let ((l (my-yas-camelcase-to-string-list str))
  ;;         (old-case case-fold-search)
  ;;         rlt)
  ;;     (setq case-fold-search nil)
  ;;     (setq rlt (mapcar (lambda (elem)
  ;;                         (if (string-match "^[A-Z]+$" elem)
  ;;                             elem
  ;;                           (downcase elem))
  ;;                         ) l))
  ;;     (setq case-fold-search old-case)
  ;;     (mapconcat 'identity rlt " ")))

  ;; (defun my-yas-escape-string (s)
  ;;   (let* ((rlt (replace-regexp-in-string "'" "\\\\'" s)))
  ;;     (setq rlt (replace-regexp-in-string "\"" "\\\\\"" rlt))
  ;;     rlt))

  ;; (defun my-yas-get-var-list-from-kill-ring ()
  ;;   "Variable name is among the `kill-ring'.  Multiple major modes supported."
  ;;   (let* ((top-kill-ring (subseq kill-ring 0 (min (read-number "fetch N `kill-ring'?" 1) (length kill-ring))) )
  ;;          rlt)
  ;;     (cond
  ;;      ((memq major-mode '(js-mode javascript-mode js2-mode js3-mode))
  ;;       (setq rlt (mapconcat (lambda (i) (format "'%s=', %s" (my-yas-escape-string i) i)) top-kill-ring ", ")))
  ;;      ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
  ;;       (setq rlt (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
  ;;                         "\" "
  ;;                         (mapconcat (lambda (i) (format "%s" i)) top-kill-ring " ")
  ;;                         )))
  ;;      ((memq major-mode '(c-mode c++-mode))
  ;;       (setq rlt (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
  ;;                         "\\n\", "
  ;;                         (mapconcat (lambda (i) (format "%s" i)) top-kill-ring ", ")
  ;;                         )))
  ;;      (t (setq rlt "")))
  ;;     rlt))
  ;; (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

  ;; (after-load 'yasnippet
  ;;   (setq-default mode-require-final-newline nil)
  ;;   (require 'dropdown-list)
  ;;   (setq yas-prompt-functions '(yas-dropdown-prompt
  ;;                                yas-completing-prompt))

  ;;   (defadvice yas-insert-snippet (around use-completing-prompt activate)
  ;;     "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
  ;;     (let ((yas-prompt-functions '(yas-completing-prompt)))
  ;;       ad-do-it))
  ;;   )

  ;; (yas-reload-all)
  ;; (yas-minor-mode t)
  )

(provide 'my-yas)
;;; my-yas.el ends here
