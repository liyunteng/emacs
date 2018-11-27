;;; my-base.el --- base                              -*- lexical-binding: t; -*-

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
;; locale

(defun my-utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (or (string-match "UTF-8" v)
	     (string-match "utf8" v))))

(defun my-locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (my-utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (my-utf8-locale-p (getenv "LC_ALL"))
      (my-utf8-locale-p (getenv "LC_CTYPE"))
      (my-utf8-locale-p (getenv "LANG"))))

(when (or window-system (my-locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'chinese-gb18030)
  (prefer-coding-system 'chinese-gbk)
  (prefer-coding-system 'utf-8)
  )



;; disable ad redefinition warning
(setq ad-redefinition-action 'accept)

;; Always load newest byte code
(setq load-prefer-newer t)

;; adjust grabage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)


;;========== backup =========
;; 不产生备份文件
(setq-default make-backup-files nil)
(setq-default backup-directory-alist `((".*" . ,(expand-file-name "Backup/" my-cache-dir))))
;;所有的备份文件转移到Backup目录下
(when make-backup-files
  (setq save-silently t
	version-control t
	kept-old-versions 2
	kept-new-versions 2
	delete-old-versions t
	backup-by-copying t))

;; replaced by super-save
(setq-default auto-save-default t)
(setq-default auto-save-list-file-prefix
	      (cond ((eq system-type 'ms-dos)
		     ;; MS-DOS cannot have initial dot, and allows only 8.3 names
		     (concat (expand-file-name "auto-save/" my-cache-dir) "_saves-") )
		    (t
		     (concat (expand-file-name "auto-save/" my-cache-dir) ".saves-"))))


;; 将保存的文件移动到.emacs.d/目录中
(setq-default diary-file (expand-file-name "diary" my-cache-dir))

(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects" my-cache-dir))
(setq-default smex-save-file (expand-file-name "smex-items" my-cache-dir))

;; desktop
(setq-default desktop-path (list my-cache-dir))
(setq-default desktop-dirname my-cache-dir)
(provide 'my-base)
;;; my-base.el ends here
