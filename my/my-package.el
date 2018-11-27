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

(defun my-mplist-get (plist prop)
  "Get the values associated to  PLIST of PROP, a modified plist."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun my-mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; keybinding
(defun my--create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.

Supported properties:
`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((global-key (my-mplist-get props :global-key))
        (def-key (my-mplist-get props :define-key)))
    (append
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defvar my-toggles '()
  "List of all declared toggles.
The structure of an element is a property list (name :func FUNCTION :doc STRING :key STRING).")

(defmacro my|add-toggle (name &rest props)
  "Add a toggle with NAME symbol.

This macro creates the following functions:
- my/toggle-NAME switches on or off depending on the current state
- my/toggle-NAME-on only switches on if currently disabled
- my/toggle-NAME-off only switches off if currently enabled

Avaiblabe PROPS:

`:status EXPRESSION'
    The EXPRESSION to evaluate to get the current status of the toggle.

`:if EXPRESSION'
    If this EXPRESSION evaluate to nil then no attempt to update the toggle
    status will be performed.

`:on BODY'
    Evaluate BODY when the toggle is switched on.

`:off BODY'
    Evaluate BODY when the toggle is switched off.

`:documentation STRING'
    STRING describes what the toggle does.

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of 'PREFIX-ARG' (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"on\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `my--create-key-binding-form' can be
used.

Example:
 (my|add-toggle company-mode
  :status (not (equal company-mode nil))
  :on (company-mode 1)
  :off (company-mode -1)
  :define-key  (lisp-interaction-mode-map . \"C-x i\")
  :global-key \"C-# i\")

Create my/toggle-company-mode my/toggle-company-mode-on and
my/toggle-company-mode-off."

  (declare (indent 1))
  (let* ((wrapper-func (intern (format "my/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (my-mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (my-mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bindkeys (my--create-key-binding-form props wrapper-func))
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicate ,wrapper-func-status) ',props)
             my-toggles)
       ;; toggle function
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "%s\nToggle %s on and off." doc (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (when (called-interactively-p 'any)
                          (message ,(format "%s disabled." name))))
               ,@on-body
               (when (called-interactively-p 'any)
                 (message ,(or on-message (format "%s enabled." name)))))
           (message "This toggle is not supported.")))
       ;; predicate function
       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         ,status-eval)
       ;; Only define on- or off-functions when status is available
       ,@(when status
           ;; on-function
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             ;; off-function
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bindkeys)))


(provide 'my-package)
;;; my-package.el ends here
