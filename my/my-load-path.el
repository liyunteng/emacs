;;; my-load-path.el --- load path                    -*- lexical-binding: t; -*-

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

(defvar my-dir (expand-file-name "my" user-emacs-directory))
(defvar my-modules-dir (expand-file-name  "modules" my-dir))
(defvar my-forks-dir (expand-file-name "forks" my-dir))
(defvar my-libs-dir (expand-file-name "libs" my-dir))

(defvar my-cache-dir (expand-file-name "cache" user-emacs-directory))

(defvar my-personal-dir (expand-file-name "personal" user-emacs-directory))
(defvar my-personal-info-file (expand-file-name "person-info.el" my-personal-dir))

(defvar my-custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p my-cache-dir)
  (make-directory my-cache-dir))

(defun my-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
		 (not (string-prefix-p "." f)))
	(add-to-list 'load-path name)
	(my-add-subfolders-to-load-path name)))))

(add-to-list 'load-path my-dir)
(my-add-subfolders-to-load-path my-dir)
;;(my-add-subfolders-to-load-path my-modules-dir)
;;(my-add-subfolders-to-load-path my-libs-dir)
(my-add-subfolders-to-load-path my-forks-dir)

;; (when (file-exists-p my-personal-dir)
;;  (my-add-subfolders-to-load-path my-personal-dir))

(provide 'my-load-path)
;;; my-load-path.el ends here
