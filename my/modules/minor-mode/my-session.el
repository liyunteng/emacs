;;; my-session.el --- session                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

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

(my-require-package 'session)
(require 'desktop)
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq-default desktop-globals-to-save
              (append '((comint-input-ring        . 50)
                        (compile-history          . 30)
                        desktop-missing-file-warning
                        (dired-regexp-history     . 20)
                        (extended-command-history . 30)
                        (face-name-history        . 20)
                        (file-name-history        . 100)
                        (grep-find-history        . 30)
                        (grep-history             . 30)
                        (ido-buffer-history       . 100)
                        (ido-last-directory-list  . 100)
                        (ido-work-directory-list  . 100)
                        (ido-work-file-list       . 100)
                        (ivy-history              . 100)
                        (magit-read-rev-history   . 50)
                        (minibuffer-history       . 50)
                        (org-clock-history        . 50)
                        (org-refile-history       . 50)
                        (org-tags-history         . 50)
                        (query-replace-history    . 60)
                        (read-expression-history  . 60)
                        (regexp-history           . 60)
                        (regexp-search-ring       . 20)
                        (search-ring              . 20)
                        (shell-command-history    . 50)
                        tags-file-name
                        tags-table-list)))
;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list my-cache-dir))
(setq desktop-auto-save-timeout 600)
(setq desktop-missing-file-warning t)
(setq desktop-restore-in-current-display t)
(setq desktop-save 'if-exists)
(setq-default desktop-buffers-not-to-save "\\` \\|^*my-debug*$")
(desktop-save-mode t)

(defadvice desktop-read (around time-restore activate)
  (let ((start-time (current-time)))
    (prog1
        ad-do-it
      (message "Desktop restored in %.2fms"
               (my-time-subtract-millis (current-time)
                                        start-time)))))

(defadvice desktop-create-buffer (around time-create activate)
  (let ((start-time (current-time))
        (filename (ad-get-arg 1)))
    (prog1
        ad-do-it
      (message "Desktop: %.2fms to restore %s"
               (my-time-subtract-millis (current-time)
                                        start-time)
               (when filename
                 (abbreviate-file-name filename))))))

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(require 'savehist)
;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60)
(savehist-mode +1)

(setq-default history-length 1000)

(require 'session)
(setq-default session-save-file (expand-file-name "session" my-cache-dir))
(setq-default session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\\\|^/ssh:\\|^ftp:\\|^rsync:)")
(setq-default session-set-file-name-exclude-regexp "[/\\]\\.overview\\|[/\\]\\.session\\|News[/\\]\\|^/ssh:\\|^ftp:\\|^rsync:")
(add-hook 'after-init-hook 'session-initialize)

(provide 'my-session)
;;; my-session.el ends here
