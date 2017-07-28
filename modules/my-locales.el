;;; my-locales.el --- locale                         -*- lexical-binding: t; -*-

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

(defun my-utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun my-locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (my-utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (my-utf8-locale-p (getenv "LC_ALL"))
      (my-utf8-locale-p (getenv "LC_CTYPE"))
      (my-utf8-locale-p (getenv "LANG"))))


(set-language-environment 'utf-8)
(setq-default locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
(prefer-coding-system 'utf-8)
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'chinese-gb18030)
(provide 'my-locales)
;;; my-locales.el ends here
