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

(setq package-archives '(("melpa" . "http://mirrors.163.com/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("org" . "http://mirrors.163.com/elpa/org/")
						 ("marmalade" . "http://mirrors.163.com/elpa/marmalade/")
						 ))
(setq package-pinned-packages
      '((switch-window . "melpa-stable")))


(require 'cl-lib)

(setq package-user-dir my-packages-dir)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar my--pre-install-packages '(bind-key
								   use-package
								   diminish
								   wgrep
								   scratch
								   command-log-mode
								   uptimes))
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
(require 'bind-key)
(require 'use-package)
(setq use-package-always-ensure nil
	  ;; use-package-verbose init-file-debug
	  use-package-verbose nil
	  use-package-inject-hooks t
	  ;; use-package-always-defer t
	  )

(defconst my--use-package-add-hook-keywords '(:pre-init
                                              :post-init
                                              :pre-config
                                              :post-config)
  "Use package `add-abbrev' keywords.")
(defmacro my|use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (my|use-package-add-hook package-name
     [:keyword [option]]...)

:pre-init      Code to run before the default `:init' configuration.
:post-init     Code to run after the default `:init' configuration.
:pre-config    Code to run before the default `:config' configuration.
:post-config   Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can
override lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword my--use-package-add-hook-keywords)
      (let ((body (my-mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body t)) expanded-forms)))))
    `(progn ,@expanded-forms)))


;; test
;; (my|use-package-add-hook multi-term
;;   :pre-init
;;   (message "pre-init multi-term")
;;   :post-init
;;   (message "post-init multi-term")
;;   :pre-config
;;   (message "pre-config multi-term")
;;   :post-config
;;   (message "post-config multi-term")
;;   )
;; (use-package multi-term
;;   :defer t
;;   :init
;;   (progn
;;     (message "use-package init multi-term")
;;     ;; (use-package multi-term)
;;     )
;;   :config
;;   (message "use-package config multi-term")
;;   :demand t)

(provide 'my-package)
;;; my-package.el ends here
