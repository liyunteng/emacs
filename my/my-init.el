;;; my-init.el --- my init                           -*- lexical-binding: t; -*-

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

(require 'my-debug)
(require 'my-package)
(setq debug-on-error nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


(setq-default custom-file my-custom-file)
(when (file-exists-p my-personal-info-file)
  (load my-personal-info-file))

(defvar my-modules
  '(my-base
    my-utils
    my-exec-path
    my-gui
    my-themes
    my-window

    my-isearch
    my-avy
    my-register
    ;; my-ido
    ;; my-ivy
    my-edit

    ;; minor
    my-mode
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
    my-web
    my-sh
    my-syslog
    my-javascript
    my-term
    my-mew
    ;; my-qt
    ;; my-header

    my-global-keybind
    my-session
    my-locales
    my-server))

(defun my-load (m)
  "Load feature M."
  (unless (load (locate-library (format "%s" m)))
    (error "Loading %s failed" m)))

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

(provide 'my-init)
;;; my-init.el ends here
