;;; my-auto-insert.el --- auto-insert

;; Copyright (C) 2014  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: lisp, autoinsert

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

(defconst gpl-license-content
  "SPDX-License-Identifier: GPL-2.0")

(defconst bsd-license-content
  "SPDX-License-Identifier: BSD-3-Clause-Clear")

(defconst mit-license-content
  "SPDX-License-Identifier: MIT")

(cl-defstruct my-auto-insert-header
  "my auto insert header struct."
  (short-description t :type bool)
  (license nil :type string)
  (copyright nil :type string)
  (author (concat (user-full-name) " <" user-mail-address ">" ) :type string)
  (time t :type bool)
  (update-time t :type bool)
  (license-content nil :type string))

(defvar test-header (make-my-auto-insert-header
                     :copyright (concat (format-time-string "%Y ")
                                        " Yunteng Li <li_yunteng@163.com>")
                     :author "liyunteng"
                     :license-content gpl-license-content
                     ))

(defvar streamocean-header (make-my-auto-insert-header
                            :author (concat (user-full-name) " <liyunteng@streamocean.com>")
                            :copyright (concat (format-time-string "%Y")
                                               " StreamOcean, Inc. All rights reserved.")
                            :update-time nil)
  "streamocean auto-insert header.")

(defvar mega-header (make-my-auto-insert-header
                     :author (concat (user-full-name) " <liyunteng@megarobo.tech>")
                     :copyright (concat (format-time-string "%Y")
                                        " Megarobo, Inc. All rights reserved.")))

(defvar addx-header (make-my-auto-insert-header
                     :short-description nil
                     :author (concat "Yunteng Li <yli@addx.ai>")
                     :copyright (concat (format-time-string "%Y")
                                        " Addx, Inc. All rights reserved.")
                     :time t
                     :update-time nil))

(defvar gpl-header (make-my-auto-insert-header
                    ;; :license "GPL-2.0"
                    :short-description nil
                    :license nil
                    :author nil
                    :copyright (concat (format-time-string "%Y")
                                       " Yunteng Li <li_yunteng@163.com>")
                    :license-content gpl-license-content
                    :update-time nil)
  "gpl auto-insert header.")

(defvar bsd-header (make-my-auto-insert-header
                    :short-description nil
                    :license nil
                    :author nil
                    :copyright (concat (format-time-string "%Y")
                                       " Yunteng Li <li_yunteng@163.com>"
                                       )
                    :license-content bsd-license-content
                    :update-time nil))

(defvar null-header (make-my-auto-insert-header
                     :short-description nil
                     :license nil
                     :copyright nil
                     :license-content nil
                     :author nil
                     :update-time nil
                     :time nil)
  "null auto-insert header.")

(defvar my-header (make-my-auto-insert-header
                   :license nil
                   :author nil
                   ;; :copyright (concat (format-time-string "%Y") " "(user-full-name) " " user-mail-address)
                   :update-time nil)
  "my auto-insert header")

(defcustom my-auto-insert-header-alist '(null-header
                                         test-header
                                         my-header
                                         streamocean-header
                                         mega-header
                                         addx-header
                                         gpl-header
                                         bsd-header)
  "My auto insert headers."
  :type 'list
  :group 'my-config)

(defcustom auto-insert-header 'null-header
  "Auto insert used license."
  ;; :type 'my-auto-insert-header
  :type 'symbol
  :group 'my-config)
;; (setq auto-insert-header streamocean-header)
;; (setq auto-insert-header 'mega-header)
(setq auto-insert-header 'my-header)

(use-package autoinsert
  :commands (auto-insert auto-insert-mode my-define-auto-insert)
  :init
  ;; don't auto-insert to custom.el
  (defadvice auto-insert (around check-custom-file-auto-insert activate)
    (when custom-file
      (if (not (equal (buffer-file-name) (file-truename custom-file)))
	      ad-do-it)))

  (defun my/auto-insert-select-header (&optional h)
    (interactive)
    (let ((p (completing-read "which header: "
                              my-auto-insert-header-alist)))
      (setq auto-insert-header (intern p))
      (setq this-command 'auto-insert)
      (funcall-interactively #'auto-insert)))

  (auto-insert-mode t)

  :config
  (setq auto-insert-query nil)
  ;; (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  ;; (define-auto-insert "\.c" "c-temp.c")

  (use-package time-stamp
    :init
    (setq time-stamp-line-limit 15)
    (setq time-stamp-start "Last-Updated:[ \t]+\\\\?[\"<]+")
    ;; (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S %U")
    (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S")
    (defun my/update-time-stamp ()
      (when time-stamp-active (time-stamp)))
    (add-hook 'write-file-functions #'my/update-time-stamp))
  (require 'time-stamp)

  (defun my-header (&optional prefix postfix)
    "My header with PREFIX and POSTFIX."
    (append
     '('(goto-char (point-min)))
     (if prefix prefix)
     '('(setq-local auto-insert--begin (point))
       '(setq-local auto-insert-header-value (symbol-value auto-insert-header))
       (when (my-auto-insert-header-copyright auto-insert-header-value)
         (concat "Copyright (C) " (my-auto-insert-header-copyright auto-insert-header-value) "\n\n"))

       ;; "Description: " (read-string "Description: ") "\n\n"
       (when (my-auto-insert-header-short-description auto-insert-header-value)
         (concat (file-name-nondirectory (buffer-file-name)) " - " (file-name-base (buffer-file-name)) "\n\n"))

       (when  (my-auto-insert-header-author auto-insert-header-value)
         (concat "Author : " (my-auto-insert-header-author auto-insert-header-value) "\n"))

       (when (my-auto-insert-header-time auto-insert-header-value)
         (concat "Date   : " (format-time-string "%Y/%m/%d") "\n"))

       (when (my-auto-insert-header-license auto-insert-header-value)
         (concat "License: " (my-auto-insert-header-license auto-insert-header-value) "\n"))

       ;; (when (my-auto-insert-header-copyright auto-insert-header-value)
       ;;   (concat "\nCopyright (C) " (my-auto-insert-header-copyright auto-insert-header-value) "\n"))

       (when (my-auto-insert-header-update-time auto-insert-header-value)
         (concat "Last-Updated: <>\n"))

       (when (my-auto-insert-header-license-content auto-insert-header-value)
         (concat "\n" (my-auto-insert-header-license-content auto-insert-header-value) "\n"))

       (if (not (equal (point) (point-min)))
           (let ((comment-style-origin comment-style))
             (setq comment-style 'extra-line)
             (comment-region auto-insert--begin (point))
             (setq comment-style comment-style-origin)
             nil)))
     (if postfix postfix)))

  (setq auto-insert-alist
        '((("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
           (concat "__" (replace-regexp-in-string
                         "[^A-Z0-9]" "_"
                         (replace-regexp-in-string
                          "\\+" "P"
                          (upcase (file-name-nondirectory buffer-file-name))))
                   "__")
           "#ifndef " str \n
           "#define " str "\n\n"
           _ "\n\n#endif " comment-start str comment-end)

          (("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
           nil
           "#include \""
           (let ((stem (file-name-sans-extension buffer-file-name))
                 ret)
             (dolist (ext '("H" "h" "hh" "hpp" "hxx" "h++") ret)
               (when (file-exists-p (concat stem "." ext))
                 (setq ret (file-name-nondirectory (concat stem "." ext))))))
           & ?\" | -10)

          (("[Mm]akefile\\'" . "Makefile") . "makefile.inc")

          (html-mode . (lambda () (sgml-tag "html")))

          (plain-tex-mode . "tex-insert.tex")
          (bibtex-mode . "tex-insert.tex")
          (latex-mode
           ;; should try to offer completing read for these
           "options, RET: "
           "\\documentclass[" str & ?\] | -1
           ?{ (read-string "class: ") "}\n"
           ("package, %s: "
            "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
           _ "\n\\begin{document}\n" _
           "\n\\end{document}")

          (ada-mode . ada-header)

          (".dir-locals.el"
           nil
           ";;; Directory Local Variables\n"
           ";;; For more information see (info \"(emacs) Directory Variables\")\n\n"
           "(("
           '(setq v1 (let (modes)
                       (mapatoms (lambda (mode)
                                   (let ((name (symbol-name mode)))
                                     (when (string-match "-mode$" name)
                                       (push name modes)))))
                       (sort modes 'string<)))
           (completing-read "Local variables for mode: " v1 nil t)
           " . (("
           (let ((all-variables
                  (apropos-internal ".*"
                                    (lambda (symbol)
			                          (and (boundp symbol)
				                           (get symbol 'variable-documentation))))))
             (completing-read "Variable to set: " all-variables))
           " . "
           (completing-read "Value to set it to: " nil)
           "))))\n")

          (("\\.el\\'" . "Emacs Lisp header")
           "Short description: "
           ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
           (make-string (max 2 (- 80 (current-column) 27)) ?\s)
           "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
           "

;; Copyright (C) " (format-time-string "%Y") "  "
           (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
           '(if (search-backward "&" (line-beginning-position) t)
                (replace-match (capitalize (user-login-name)) t t))
           '(end-of-line 1) " <" (progn user-mail-address) ">"
           &  "

\;; " _ "

\(provide '"
           (file-name-base (buffer-file-name))
           ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")
          )
        )

  )


(provide 'my-auto-insert)
;;; my-auto-insert.el ends here
