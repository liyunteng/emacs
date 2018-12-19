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
  (setq recentf-save-file (expand-file-name "recentf" my-cache-dir))
  (add-hook 'after-init-hook 'recentf-mode)
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
	           "COMMIT_EDITMSG\\'"))

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :commands (desktop-full-file-name
	         desktop-save)
  :defines (desktop-save)
  :init
  (setq desktop-path (list my-cache-dir)
	    desktop-dirname my-cache-dir
	    desktop-auto-save-timeout 600
	    desktop-missing-file-warning nil
        desktop-load-locked-desktop t
	    desktop-restore-in-current-display t
        desktop-restore-frames t
        desktop-restore-reuses-frames t
	    desktop-save t
	    ;; desktop-save 'ask-if-new
	    )

  ;; don't save /tmp/*
  (setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/*\\)")

  (if (daemonp)
      ;; In daemon mode only first make frame, load desktop
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (unless desktop-save-mode
                      ;; FIXME: turn on restore window-configuration
                      ;; in daemon mode
                      (setq desktop-restore-frames nil)
                      (desktop-save-mode +1)
                      (desktop-read)))))
    (desktop-save-mode +1))

  :config
  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
	    (append '((comint-input-ring        . 50)
		          (compile-history          . 30)
                  desktop-missing-file-warning
		          (dired-regexp-history     . 20)
		          (extended-command-history . 30)
		          (face-name-history        . 20)
		          (file-name-history        . 100)
		          (grep-find-history        . 30)
		          (grep-history             . 30)
		          (helm-ff-history          . 100)
		          (helm-file-name-history   . 100)
                  (helm-M-x-input-history   . 100)
                  (helm-external-command-history . 20)
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


  (defun my/desktop-clear ()
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
    (setq desktop-dirname my-cache-dir)))

;; savehist keeps track of some history
(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist" my-cache-dir))
  (add-hook 'after-init-hook 'savehist-mode))

(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" my-cache-dir))
  (add-hook 'after-init-hook 'save-place-mode))

;; (use-package session
;;   :ensure t
;;   :init
;;   (setq session-save-file (expand-file-name ".session" my-cache-dir))
;;   (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;;   (setq session-save-file-coding-system 'utf-8)
;;   (add-hook 'after-init-hook 'session-initialize))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :init
  (add-hook 'after-init-hook 'super-save-mode)
  :config
  (add-to-list 'super-save-triggers 'dired-jump)
  (add-to-list 'super-save-triggers 'dired-jump-other-window))

(provide 'my-session)
;;; my-session.el ends here
