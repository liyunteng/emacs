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

;;
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

  (add-to-list 'recentf-exclude (expand-file-name my-cache-dir))
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  ;; (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/.*" (getenv "HOME")))

  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude (format "%s/.orhc-bibtex-cache" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/configuration/emacs\\.d/\\(?!\\(main.*\\)\\)" (getenv "HOME")))


  ;; Some caches
  (add-to-list 'recentf-exclude (format "%s/\\.ido\\.last" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.recentf" (getenv "HOME")))


  ;; elfeed
  (add-to-list 'recentf-exclude (format "%s/\\.elfeed/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/shared/pCloudDrive/emacs/elfeed/.*" (getenv "HOME")))

  ;; Org-mode organisation
  (add-to-list 'recentf-exclude (format "%s/shared/pCloudDrive/org/organisation/.*" (getenv "HOME")))

  ;; Org/todo/calendars
  (add-to-list 'recentf-exclude ".*todo.org")
  (add-to-list 'recentf-exclude (format "%s/Calendars/.*" (getenv "HOME")))

  ;; Maildir
  (add-to-list 'recentf-exclude (format "%s/maildir.*" (getenv "HOME")))
  )

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :commands (desktop-full-file-name
	         desktop-save)
  :bind (("C-x M-k" . my/desktop-clear))
  :defines (desktop-save)
  :init
  (defun my/desktop-clear ()
    "Desktop clear and Desktop remove."
    (interactive)
    (desktop-clear)
    (desktop-remove))

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
  (setq desktop-path (list my-cache-dir)
	    ;; desktop-dirname my-cache-dir
	    desktop-auto-save-timeout 600
	    desktop-missing-file-warning nil
        desktop-load-locked-desktop t
	    desktop-restore-in-current-display nil
        desktop-restore-frames t
        desktop-restore-reuses-frames t
	    desktop-save t
	    ;; desktop-save 'ask-if-new
	    )
  ;; don't save /tmp/*
  (setq desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\|^/tmp/*\\)")
  ;; (add-to-list 'desktop-minor-mode-table '(yas-minor-mode nil))
  (add-to-list 'desktop-minor-mode-table '(dired-git-info-mode nil))
  ;; fixme global-auto-revert-mode can't work
  (defun global-auto-revert-desktop-restore (arg))
  (add-to-list 'desktop-minor-mode-handlers '(global-auto-revert-mode . global-auto-revert-desktop-restore))

  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
	    (append '((desktop-saved-frameset   . nil)  ;don't save frameset
                  (buffer-name-history      . 30)
                  (command-history          . 30)
                  (compile-history          . 10)
                  (extended-command-history . 30)
                  (comint-input-ring        . 30)
                  (dired-regexp-history     . 10)
                  (face-name-history        . 20)
                  (file-name-history        . 50)
                  desktop-missing-file-warning
                  (grep-files-history       . 10)
		          (grep-find-history        . 10)
		          (grep-history             . 10)
                  (grep-regexp-history      . 10)
		          (helm-ff-history          . 30)
		          (helm-file-name-history   . 30)
                  (helm-M-x-input-history   . 30)
                  (helm-external-command-history . 30)
		          (helm-grep-history        . 30)
		          (helm-occur-history       . 30)
                  (ido-file-history         . 30)
		          (ido-buffer-history       . 30)
		          (ido-last-directory-list  . 30)
		          (ido-work-directory-list  . 30)
		          (ido-work-file-list       . 30)
		          (ivy-history              . 30)
                  (counsel-M-x-history      . 30)
                  (counsel-compile-history       . 10)
                  (counsel-describe-symbol-history . 10)
                  (counsel-grep-history           . 10)
                  (counsel-locate-history    . 10 )
                  (counsel-set-variable-history . 10)
                  (swiper-history           . 30)
                  (minibuffer-history       . 50)
		          (magit-read-rev-history   . 50)
		          (org-clock-history        . 50)
		          (org-refile-history       . 50)
		          (org-tags-history         . 50)
		          (query-replace-history    . 10)
		          (read-expression-history  . 10)
		          (regexp-history           . 10)
		          (regexp-search-ring       . 10)
		          (search-ring              . 20)
		          (shell-command-history    . 50)
		          tags-file-name
		          tags-table-list)))

  (defvar before-desktop-read-time nil)
  (defvar after-desktop-read-time nil)
  (defadvice desktop-read (around time-restore (&optional dirname ask) activate)
    (progn
      (setq before-desktop-read-time (current-time))
      ad-do-it
      (setq after-desktop-read-time (current-time))))

  (defadvice desktop-create-buffer (around time-create activate)
    (let ((start-time (current-time))
          (filename (ad-get-arg 1))
          (buffername (ad-get-arg 2))
          (mj (ad-get-arg 3)))
      (prog1
          ad-do-it
        (message "Desktop: %.2fms to restore %s [%S]"
    	         (my-time-subtract-millis (current-time) start-time)
    	         (if filename
    	             (abbreviate-file-name filename)
    	           buffername)
    	         mj))))

  (defadvice desktop-remove (around set-desktop-dirname activate)
    ad-do-it
    (setq desktop-dirname my-cache-dir)))

;; savehist keeps track of some history
;; (use-package savehist
;;   :init
;;   (setq savehist-file (expand-file-name "savehist" my-cache-dir))
;;   (add-hook 'after-init-hook 'savehist-mode))

;; (use-package session
;;   :ensure t
;;   :init
;;   (setq session-save-file (expand-file-name ".session" my-cache-dir))
;;   (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;;   (setq session-save-file-coding-system 'utf-8)
;;   (add-hook 'after-init-hook 'session-initialize))

(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" my-cache-dir))
  (add-hook 'after-init-hook 'save-place-mode))

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
