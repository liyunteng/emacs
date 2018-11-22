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

(use-package recentf
  :init
  (recentf-mode +1)
  :config
  (setq
   recentf-max-saved-items 300
   recentf-max-menu-items 30
   recentf-exclude '("/tmp/" "/ssh:" "/root@" "/sudo:"
		     "TAGS" "GTAGS" "GRAGS" "GPATH"))

  (add-to-list 'recentf-exclude
	       (expand-file-name my-cache-dir))
  (add-to-list 'recentf-exclude
	       (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude
	       "COMMIT_EDITMSG\\'")

  ;; (add-hook 'after-init-hook 'recentf-mode)
  ;; (add-hook 'after-init-hook 'recentf-load-list)
  )

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :commands (desktop-full-file-name
	     desktop-save)
  :defines (desktop-save)
  :init
  (setq desktop-path (list my-cache-dir)
	desktop-dirname my-cache-dir
	desktop-auto-save-timeout 600
	desktop-missing-file-warning t
	desktop-restore-in-current-display t
	desktop-save t
	;; desktop-save 'ask-if-new
	)
  ;; fix if no deskop-file desktop-read will close all window
  (unless (or (not (desktop-full-file-name)) my-debug)
    (desktop-save-mode +1))
  :config
  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
  	(append '(desktop-missing-file-warning
		  (comint-input-ring        . 50)
  		  (compile-history          . 30)
  		  (dired-regexp-history     . 20)
  		  (extended-command-history . 30)
  		  (face-name-history        . 20)
  		  (file-name-history        . 100)
  		  (grep-find-history        . 30)
  		  (grep-history             . 30)
  		  (helm-ff-history          . 100)
  		  (helm-file-name-history   . 100)
  		  (helm-grep-history        . 30)
  		  (helm-occur-history       . 30)
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


  ;; (defadvice desktop-read (around time-restore activate)
  ;;   (let ((start-time (current-time)))
  ;;     (prog1
  ;; 	  ad-do-it
  ;; 	(message "Emacs startup time: %.2fs Desktop restored in %.2fs"
  ;; 		 (/ (my-time-subtract-millis after-init-time before-init-time) 1000)
  ;; 		 (/ (my-time-subtract-millis (current-time) start-time) 1000)))))


  (defun my/desktop-remove ()
    "Desktop clear and Desktop remove."
    (interactive)
    (desktop-clear)
    (desktop-remove))

  (defvar before-desktop-read-time nil)
  (defvar after-desktop-read-time nil)
  (defadvice desktop-read (around time-restore activate)
    (let ((start-time (current-time)))
      (prog1
	  (setq before-desktop-read-time start-time)
  	ad-do-it
	(setq after-desktop-read-time (current-time) )
  	)))

  (defadvice desktop-create-buffer (around time-create activate)
    (let ((start-time (current-time))
	  (filename (ad-get-arg 1))
	  (buffername (ad-get-arg 2))
	  (mj (ad-get-arg 3)))
      (prog1
	  ad-do-it
	(message "Desktop: %.2fms to restore %s [%s]"
		 (my-time-subtract-millis (current-time) start-time)
		 (if filename
		     (abbreviate-file-name filename)
		   buffername)
		 mj))))

  (defadvice desktop-remove (around set-desktop-dirname activate)
    ad-do-it
    (setq desktop-dirname my-cache-dir))
  )

;; savehist keeps track of some history
(use-package savehist
  :init
  (savehist-mode +1)
  :config
  (setq savehist-additional-variables
	'(mark-ring
	  global-mark-ring
	  search-ring
	  regexp-search-ring
	  extended-command-history
	  )
	;; save every minute
	savehist-autosave-interval 60
	history-length 1000
	))

(use-package saveplace
  :init
  (if (fboundp 'save-place-mode)
      (save-place-mode +1)))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :init
  (super-save-mode +1)
  :config
  (add-to-list 'super-save-triggers 'dired-jump)
  (add-to-list 'super-save-triggers 'dired-jump-other-window))

(provide 'my-session)
;;; my-session.el ends here
