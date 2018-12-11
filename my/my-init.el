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
(require 'my-load-path)

(defconst my-modules '(
                       my-utils
                       my-base
                       my-package
                       my-exec-path
                       my-frame
                       my-themes

                       my-edit
                       my-search
                       my-extension
                       my-buffer
                       my-dired
                       my-ibuffer
                       my-window
                       my-session
                       my-helm
                       ;; my-ido
                       ;; my-ivy

                       ;; my-jump
                       my-smartparens
                       my-flyspell
                       my-flycheck
                       my-yas
                       my-ac
                       ;; my-auto-complete

                       my-term
                       my-magit
                       my-tramp
                       my-gud
                       my-mu4e

                       my-avy
                       my-auto-insert
                       ;; my-header
                       my-hideshow
                       my-auto-mode

                       my-lisp
                       my-c
                       ;; my-qt
                       my-go
                       my-python
                       my-org
                       my-web
                       my-syslog
                       my-javascript

                       my-server
                       )
  "My auto load modules.")

(defun my-load (m)
  "Load feature M."
  (if (load (locate-library (format "%s" m)))
      (message "Loading %s" m)
    (error "Loading %s failed" m)))

(defun my-require (m)
  "Require feature M."
  (if (require m)
      (message "Loading %s" m)
    (error "Requiring %s failed" m)))

(defun my/show-init-time ()
  "Show init time."
  (interactive)
  (if desktop-save-mode
      (message "Emacs startup time: %.2fms Desktop restore time: %.2fms"
	           (my-time-subtract-millis after-init-time before-init-time)
	           (my-time-subtract-millis after-desktop-read-time before-desktop-read-time))
    (message "Emacs startup time: %.2fms"
	         (my-time-subtract-millis after-init-time before-init-time))))


(defun my-init ()
  "Load my modules."
  (when (and my-personal-info-file
             (file-exists-p my-personal-info-file))
    (require 'person-info))

  ;;(mapc 'my-load my-modules)
  (mapc 'my-require my-modules)

  (when (and custom-file (file-exists-p custom-file))
    (load custom-file))

  (add-hook 'after-init-hook
  	        (lambda () (run-at-time 0 nil 'my/show-init-time)) t))
(my-init)

(provide 'my-init)
;;; my-init.el ends here
