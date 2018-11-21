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

(defvar my-dir)
(defvar my-pinned-packages-file (expand-file-name "my-pinned-packages.el" my-dir))
(defvar my-packages-save-dir (expand-file-name "elpa" user-emacs-directory))

(require 'cl)
(require 'package)
(when (file-exists-p my-pinned-packages-file)
  (load my-pinned-packages-file))
(setq package-user-dir my-packages-save-dir)
(setq package-enable-at-startup t)
(defvar my-packages '(bind-key use-package))

(defun my-packages-installed-p ()
  "Check if all packages in `my-packages' are installed."
  (every #'package-installed-p my-packages))

(defun my-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun my-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'my-require-package packages))

(defun my-install-packages ()
  "Install all packages listed in `my-packages'."
  (unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs My is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (my-require-packages my-packages)))

;; ;; run package installation
(my-install-packages)

(defun my/list-foreign-packages ()
  "Browse third-party packages not bundled with My.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `my-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list my-packages)))


;; use package
;;(my-require-package 'diminish)
;;(require 'diminish)
(require 'bind-key)
(require 'use-package)
(setq
 use-package-always-ensure nil
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
