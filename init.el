;;; my-modules.el --- my modules                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

;; Author: liyunteng;;; Code: <li_yunteng@163.com>
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
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Emacs is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.4")
  (error "Requires at least GNU Emacs 24.4, but you're running %s" emacs-version))
;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


(defvar my-dir (file-name-directory load-file-name))
(defvar my-modules-dir (expand-file-name  "modules" my-dir))
(defvar my-savefile-dir (expand-file-name "savefile" my-dir))
(defvar my-forks-dir (expand-file-name "forks" my-dir))
(defvar my-libs-dir (expand-file-name "libs" my-dir))

(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

(defun my-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
		 (not (string-prefix-p "." f)))
	(add-to-list 'load-path name)
	(my-add-subfolders-to-load-path name)))))

(setq-default custom-file (expand-file-name "my-custom.el" my-modules-dir))
(my-add-subfolders-to-load-path my-modules-dir)
(my-add-subfolders-to-load-path my-forks-dir)
(add-to-list 'load-path my-libs-dir)

(add-to-list 'load-path (expand-file-name "preload" my-dir))
(require 'my-benchmarking)
(require 'cl)
(require 'package)

(setq package-archives '(
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("org" . "http://elpa.emacs-china.org/org/")))
(add-to-list 'package-pinned-packages
             '(switch-window . "melpa-stable"))

;; set package-user-dir to be relative to My install path
(setq package-user-dir (expand-file-name "elpa" my-dir))
(package-initialize)

(defvar my-packages '(anzu
		      elisp-slime-nav
		      expand-region
		      helm
		      helm-directory
		      whitespace-cleanup-mode
		      zenburn-theme))

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

;; run package installation
(my-install-packages)
(defun my-list-foreign-packages ()
  "Browse third-party packages not bundled with My.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `my-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list my-packages)))

(defmacro my-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))


(defvar my-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (my-auto-install extension package mode))))
 my-auto-install-alist)

(defvar my-modules
  '(my-base
    my-utils
    my-locales
    my-gui
    my-window
    my-themes

    my-isearch
    my-avy
    my-register
    ;; my-ido
    ;; my-ivy
    my-edit

    ;; minor
    my-helm
    my-semantic
    my-tramp
    my-magit
    my-gud
    my-flyspell
    my-auto-insert
    my-smartparens
    my-hideshow
    my-yas
    my-ac
    ;; my-company
    ;; my-auto-complete
    my-recentf

    ;; major
    my-ibuffer
    my-dired
    my-c
    my-go
    my-python
    my-lisp
    my-org
    my-sh
    my-syslog
    my-javascript
    my-term
    my-mew
    ;; my-qt
    ;; my-header

    my-global-keybind
    my-session
    my-server
    ))

(defun my-load (m)
  "Load feature M."
  (load (locate-library (format "%s" m))))

(when (file-exists-p my-modules-dir)
  (add-to-list 'load-path my-modules-dir)
  (message "Loading my configuration files")
  ;; (dolist (module my-modules)
  ;;   (message "Loading %s" module)
  ;;   (require module)
  ;;   )
  (mapc 'my-load my-modules)
  )

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'my-modules)
;;; my-modules.el ends here
