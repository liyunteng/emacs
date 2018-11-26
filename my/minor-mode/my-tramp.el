;;; my-tramp.el --- tramp                            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  liyunteng

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

(use-package tramp
  :defines (tramp-default-user-alist)
  :bind ("C-x M-f" . find-file-root)
  :init
  (setq tramp-auto-save-directory (expand-file-name "tramp" my-cache-dir))
  (setq tramp-persistency-file-name (expand-file-name "tramp/tramp" my-cache-dir))

  (setq tramp-verbose 0)
  ;; (setq-default tramp-default-method "ssh")
  (setq tramp-default-method "rcp")
  (setq tramp-default-user "root")
  (setq tramp-default-host "127.0.0.1")

  (setq tramp-chunksize 8196)
  (setq password-cache t)
  (setq password-cache-expiry 36000)
  (setq tramp-connection-timeout 10)

;;; 解决tramp登陆失败，导致emacs假死的问题
  ;; (setq tramp-ssh-controlmaster-options nil)
  (setq tramp-use-ssh-controlmaster-options nil)

  ;;(add-to-list 'tramp-remote-process-environment
  ;;(format "DISPALY=%s" (getenv "DISPLAY")))


  ;;使用sudo 编辑文件
  (defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
    "*The filename prefix used to open a file with `find-file-root'.")

  (defvar find-file-root-history nil
    "History list for files found using `find-file-root'.")

  (defvar find-file-root-hook nil
    "Normal hook for functions to run after finding a \"root\" file.")

  (defun find-file-root ()
    "*Open a file as the root user.
Prepends `find-file-root-prefix' to the selected file name so that it
maybe accessed via the corresponding tramp method."

    (interactive)
    (let* ( ;; We bind the variable `file-name-history' locally so we can
	   ;; use a separate history list for "root" files.
	   (file-name-history find-file-root-history)
	   (name (or buffer-file-name default-directory))
	   (tramp (and (tramp-tramp-file-p name)
		       (tramp-dissect-file-name name)))
	   path dir file)

      ;; If called from a "root" file, we need to fix up the path.
      (when tramp
	(setq path (tramp-file-name-localname tramp)
	      dir (file-name-directory path)))

      (when (setq file (read-file-name "Find file (UID = 0): " dir path))
	(find-file (concat find-file-root-prefix file))
	;; If this all succeeded save our new history list.
	(setq find-file-root-history file-name-history)
	;; allow some user customization
	(run-hooks 'find-file-root-hook))))

  (defface find-file-root-header-face
    '((t (:foreground "white" :background "red3")))
    "*Face use to display header-lines for files opened as root."
    :group 'my)

  (defun find-file-root-header-warning ()
    "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-root-hook'."
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
	   (space (+ 6 (- (window-width) (length warning))))
	   (bracket (make-string (/ space 2) ?-))
	   (warning (concat bracket warning bracket)))
      (setq header-line-format
	    (propertize  warning 'face 'find-file-root-header-face))))
  (add-hook 'find-file-root-hook 'find-file-root-header-warning)

  ;; (let ((my-tramp-methods nil)
  ;;       (my-tramp-ssh-method
  ;;        '("ssh"
  ;;          (tramp-login-program "ssh")
  ;;          (tramp-login-args (("%h")
  ;;                             ("-l" "%u")
  ;;                             ("-p" "%p")
  ;;                             ("-e" "none")
  ;;                             ("-A")))
  ;;          (tramp-remote-sh "/bin/bash")
  ;;          (tramp-copy-program nil)
  ;;          (tramp-copy-args nil)
  ;;          (tramp-copy-keep-date nil)
  ;;          (tramp-password-end-of-line nil)
  ;;          (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null")
  ;;                          ("-o" "UserKnownHostsFile=/dev/null")
  ;;                          ("-o" "StrictHostKeyChecking=no")))
  ;;          (tramp-default-port 22))))
  ;;   (setq tramp-methods (dolist (elt tramp-methods my-tramp-methods)
  ;;                         (if (string= (car elt) "ssh")
  ;;                             (setq my-tramp-methods (cons my-tramp-ssh-method my-tramp-methods))
  ;;                           (setq my-tramp-methods (cons elt my-tramp-methods))))))

  ;; (add-to-list 'tramp-default-user-alist
  ;; 			   '("ssh" "\\`host1\\.com\\'" "root"))
  ;; (add-to-list 'tramp-default-user-alist
  ;; 			   '("ssh" "\\`host2\\.com\\'" "lyt"))
  ;; (add-to-list 'tramp-default-proxies-alist
  ;;              '("\\`host2\\.com\\'" nil "/ssh:labrador@host3.com:"))
  ;; (add-to-list 'tramp-default-method-alist
  ;;              '("\\`host2\\.com\\'" nil "
  )


(provide 'my-tramp)
;;; my-tramp.el ends here
