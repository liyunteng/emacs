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

(load-file (concat user-emacs-directory "my/my-load-path.el"))

(defvar my-modules
  '(
    my-debug
    my-utils
    my-package
    my-base
    my-gui
    my-themes
    my-window
    my-auto
    my-avy
    my-edit
    my-isearch

    ;; my-ido
    ;; my-ivy
    my-mode
    my-helm
    my-tramp
    my-magit
    my-smartparens
    my-gud
    my-flyspell
    my-flycheck
    my-auto-insert
    my-hideshow
    my-ac
    ;; my-auto-complete

    my-lisp
    my-ibuffer
    my-dired
    my-c
    my-go
    my-python
    my-org
    my-web
    my-sh
    my-syslog
    my-javascript
    my-json
    my-term
    my-mu4e
    ;; my-qt
    ;; my-header

    my-jump
    my-yas
    my-server
    my-session
    ))

(defun my-load (m)
  "Load feature M."
  (unless (load (locate-library (format "%s" m)))
    (error "Loading %s failed" m)))


(defun my-show-init-time ()
  "Show init time."
  (if desktop-save-mode
      (message "Emacs startup time: %.2fms Desktop restore time: %.2fms"
	       (my-time-subtract-millis after-init-time before-init-time)
	       (my-time-subtract-millis after-desktop-read-time before-desktop-read-time))
    (message "Emacs startup time: %.2fms"
	     (my-time-subtract-millis after-init-time before-init-time)))
  )



(defun my-init ()
  "Load my modules."
  (when (file-exists-p my-personal-info-file)
    (my-load my-personal-info-file))

  (when (file-exists-p my-modules-dir)
    (add-to-list 'load-path my-modules-dir)
    ;; (dolist (module my-modules)
    ;;   (message "Loading %s" module)
    ;;   (require module)
    ;;   )
    (mapc 'my-load my-modules))

  (when (and custom-file
	     (file-exists-p custom-file))
    (my-load custom-file))

  (add-hook 'after-init-hook
  	    (lambda () (run-at-time 0 nil 'my-show-init-time)) t)
  )

(provide 'my-init)
;;; my-init.el ends here
