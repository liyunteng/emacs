;;; my-package.el --- package                        -*- lexical-binding: t; -*-

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

(require 'cl)
(require 'package)
(defconst 163-mirrors
  '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
    ("melpa" . "http://mirrors.163.com/elpa/melpa/")
    ("melpa-stable" . "http://mirrors.163.com/elpa/melpa-stable/")
    ("org" . "http://mirrors.163.com/elpa/org/")
    ("marmalade" . "http://mirrors.163.com/elpa/marmalade/"))
  "163 mirror")
(defconst tsinghua-mirrors
  '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
    ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
    ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/"))
  "tsinghua mirror")
(defconst elpa-china-mirrors
  '(("gnu" . "http://elpa.emacs-china.org/gnu/")
    ("melpa" . "http://elpa.emacs-china.org/melpa/")
    ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
    ("org" . "http://elpa.emacs-china.org/org/")
    ("marmalade" . "http://elpa.emacs-china.org/marmalade/"))
  "elpa.emacs-china.org mirror")
(setq package-archives 163-mirrors)
;; (setq package-pinned-packages
;;       '((ivy . "gnu")))


(setq package-user-dir my-packages-dir)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar my--pre-install-packages '(bind-key
				                   use-package
				                   diminish
				                   wgrep
				                   scratch
				                   command-log-mode

                                   try
				                   ))
;; (defun require-package (package)
;;   "Install PACKAGE unlesee already installed."
;;   (unless (package-installed-p package)
;; 	(package-install package)))

;; (defun require-packages (packages)
;;   "Ensure PACKAGES are installed.
;; Missing packages are installed automatically."
;;   (mapc #'require-package packages))

;; (defun my--install-pre-install-packages ()
;;   (unless (every #'package-installed-p my--pre-install-packages)
;; 	(message "%s" "Emacs is now refreshhing its package database...")
;; 	(package-refresh-contents)
;; 	(message "%s" "done.")
;; 	(require-packages my--pre-install-packages)))

;; (my--install-pre-install-packages)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (versions (mapcar #'package-desc-version known)))
        (if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
            (package-install package)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t))))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(defun require-packages (packages)
  (mapc #'require-package packages))

(require-packages my--pre-install-packages)


(defun my/list-foreign-packages ()
  "Browse third-party packages not bundled with My.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `my-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   package-activated-list
   ;; (set-difference package-activated-list my--pre-install-packages)
   ))


(defun my-set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun my-maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (my-set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (my-set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'my-maybe-widen-package-menu-columns)



;; use package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure nil
      ;; use-package-verbose init-file-debug
      use-package-verbose nil
      use-package-inject-hooks t
      ;; use-package-always-defer t
      use-package-compute-statistics t)

(provide 'my-package)
;;; my-package.el ends here
