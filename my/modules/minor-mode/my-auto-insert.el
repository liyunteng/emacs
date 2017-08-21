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
  "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.\n\n"
  )

(defconst streamocean-license-content "")

(defconst gpl-license (list "li_yunteng@163.com" "GPL" gpl-license-content))
(defconst streamocean-license (list "liyunteng@streamocean.com" "StreamOcean" streamocean-license-content))
(defvar auto-insert-license gpl-license)

(use-package autoinsert
  :config
  (use-package time-stamp
	:config
	(setq time-stamp-line-limit 15)
	(setq time-stamp-start "Last-Updated:")
	(setq time-stamp-end "\n")
	(setq time-stamp-format " %04Y/%02m/%02d %02H:%02M:%02S")
	(add-hook 'write-file-functions 'time-stamp)
	)

  ;; (setq auto-insert t)
  (setq auto-insert-query nil)
  (setq auto-insert-license streamocean-license)
  ;; (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  ;; (define-auto-insert "\.c" "c-temp.c")

  (defun my-header (&optional prefix postfix)
	"My header with PREFIX and POSTFIX."
	(append
	 '((beginning-of-buffer) (insert "\n") (beginning-of-buffer) "")
	 (if prefix prefix)
	 '(
	   (null (setq-local begin (point)))
	   "Filename: " (file-name-nondirectory (buffer-file-name)) "\n"
	   "Description: " (read-string "Description: ") "\n\n"
	   "Copyright (C) " (format-time-string "%Y") " " (getenv "ORGANIZATION") | (concat user-full-name) "\n\n"
	   "Author: " user-full-name (if (search-backward "&" (line-beginning-position) t) (replace-match (capitalize (user-login-name)) t t)) " <" (car auto-insert-license) ">\n"
	   "License: " (car (cdr auto-insert-license)) "\n"
	   "Last-Updated: \n\n"
	   (car (cdr (cdr auto-insert-license)))
	   (comment-region begin (point)))
	 (if postfix postfix)))

  (define-auto-insert 'sh-mode
	(my-header '("#!/usr/bin/bash\n\n")))
  (define-auto-insert 'python-mode
	(my-header '("#!/usr/bin/env python\n" "# -*- coding: utf-8 -*-\n\n")))
  (define-auto-insert '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
	(if (version<= emacs-version "25.1.0")
		(my-header nil
				   '((let ((header (upcase (concat (file-name-nondirectory
													(file-name-sans-extension buffer-file-name))
												   "_"
												   (file-name-extension buffer-file-name)
												   "_"))))
					   (concat "#ifndef " header "\n"
							   "#define " header "\n\n")
					   )
					 _"\n\n#endif"))
	  (my-header)))
  (define-auto-insert '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
	(if (version<= emacs-version "25.1.0")
		(my-header nil
				   '("#include \""
					 (let ((stem (file-name-sans-extension buffer-file-name)))
					   (cond ((file-exists-p (concat stem ".h"))
							  (file-name-nondirectory (concat stem ".h")))
							 ((file-exists-p (concat stem ".hh"))
							  (file-name-nondirectory (concat stem ".hh")))
							 ((file-exists-p (concat stem ".hpp"))
							  (file-name-nondirectory (concat stem ".hpp")))
							 ((file-exists-p (concat stem ".hxx"))
							  (file-name-nondirectory (concat stem ".hxx")))
							 ((file-exists-p (concat stem ".h++"))
							  (file-name-nondirectory (concat stem ".h++")))
							 ))
					 & ?\" | -10 "\n"))
	  (my-header)))

  (auto-insert-mode +1)
  )


(provide 'my-auto-insert)
;;; my-auto-insert.el ends here
