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

(defgroup my-config nil "My configurations."
  :group 'emacs
  :prefix "my-")

(defcustom my-packages-dir (expand-file-name "elpa" user-emacs-directory)
  "Download packages install directory."
  :type 'directory
  :group 'my-config)

(defcustom my-personal-dir (expand-file-name "personal" user-emacs-directory)
  "My personal-info directory."
  :type 'directory
  :group 'my-config)

(defcustom my-personal-info-file (expand-file-name "person-info.el" my-personal-dir)
  "My personal-info file."
  :type 'file
  :group 'my-config)

(defcustom my-custom-file (expand-file-name "custom.el" user-emacs-directory)
  "Custom file."
  :type 'file
  :group 'my-config)
(setq custom-file my-custom-file)

(defcustom my-cache-dir (expand-file-name "cache" user-emacs-directory)
  "Cache files directory."
  :type 'directory
  :group 'my-config)
(unless (file-exists-p my-cache-dir)
  (make-directory my-cache-dir))

(defconst my-dir (expand-file-name "my" user-emacs-directory) "My base directory.")
(defconst my-forks-dir (expand-file-name "forks" my-dir) "My forks package directory.")
(defconst my-libs-dir (expand-file-name "libs" my-dir) "My library directory.")
(make-obsolete-variable 'my-libs-dir 'my-forks-dir "25")

(defconst my-init-file (expand-file-name "my-init.el" my-dir))

(defun my-add-to-load-path (dir &optional append)
  "Add DIR to load path, if APPEND add to end."
  (add-to-list 'load-path dir append))

(defun my-add-to-load-path-if-exists (dir &optional append)
  "If DIR exists in the file system, add it to `load-path'.
If APPEND add to end."
  (when (file-exists-p dir)
    (my-add-to-load-path dir append)))

(defun my-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
				 (not (string-prefix-p "." f)))
		(add-to-list 'load-path name)
		(my-add-subfolders-to-load-path name)))))

(add-to-list 'load-path my-dir)
(add-to-list 'load-path my-personal-dir)
(my-add-subfolders-to-load-path my-dir)
;; (my-add-subfolders-to-load-path my-forks-dir)



(provide 'my-load-path)
;;; my-load-path.el ends here
