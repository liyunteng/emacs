;;; my-mu4e.el --- gnus                              -*- lexical-binding: t; -*-

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

;; require:
;; 1. offlineimap
;; 2. mu
;;
;; 使用方法：
;; 1. 安装offlineimap, 拷贝personal/.offlineimaprc到~/
;; 2. 安装mu
;; 3. 拷贝personal/.authinfo.gpg 到~/
;; 已经配置好了163邮箱的收发


;;; Code:

;;
(use-package sendmail
  :defer t
  :config
  (setq send-mail-function 'smtpmail-send-it)
  ;; (setq send-mail-function 'mailclient-send-it)
  )

(use-package smtpmail
  :defer t)

(use-package message
  :defer t
  :config
  (setq message-confirm-send t						;防止误发邮件, 发邮件前需要确认
	    message-kill-buffer-on-exit t				;设置发送邮件后删除buffer
	    message-from-style 'angles					;`From' 头的显示风格
	    message-syntax-checks '((sender . disabled));语法检查
	    message-send-mail-function 'smtpmail-send-it
	    message-cite-function 'message-cite-original-without-signature ;;引用设置：不要原来的签名，引用全文
	    message-kill-buffer-on-exit t)
  (add-hook 'mail-citation-hook 'sc-cite-original))

(use-package mu4e
  :if (and (executable-find "offlineimap")
           (executable-find "mu"))
  :commands (mu4e mu4e-compose-new)
  :bind (("C-x M-m" . mu4e-compose-new))
  :init
  (defvar mu4e-account-alist nil
    "Account alist for custom multi-account compose.")

  :config
  (use-package mu4e-vars
    :defines (mu4e-maildir
	          mu4e-trash-folder
	          mu4e-refile-folder
	          mu4e-sent-folder
	          mu4e-drafts-folder
	          mu4e-get-mail-command
	          mu4e-update-interval
	          mu4e-view-show-images
	          mu4e-maildir-shortcuts
	          mu4e-bookmarks
	          mu4e-compose-parent-message
	          mu4e-completing-read-function
              mu4e-display-update-status-in-modeline
              mu4e-use-fancy-chars
              mu4e-debug)
    :config
    (setq mu4e-debug t)
    (setq mu4e-maildir "~/Maildir"
          mu4e-trash-folder "/已删除"
          mu4e-refile-folder "/归档"
          mu4e-sent-folder "/已发送"
          mu4e-drafts-folder "/草稿箱"
          mu4e-get-mail-command "offlineimap")

    (setq mu4e-bookmarks
	      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
	        ("date:today..now" "Today's messages" ?t)
	        ("date:7d..now" "Last 7 days" ?w)
	        ("mime:image/*" "Messages with images" ?p)
	        (,(mapconcat 'identity
			             (mapcar
			              (lambda (maildir)
			                (concat "maildir:" (car maildir)))
			              mu4e-maildir-shortcuts) " OR ")
	         "All inboxes" ?i)))

    (setq mu4e-update-interval 300
          mu4e-view-show-images t
          mu4e-completing-read-function 'completing-read
          mu4e-compose-parent-message t))

  (use-package mu4e-compose
    :defines (mu4e-sent-messages-behavior)
    :config
    (setq mu4e-sent-messages-behavior 'delete))

  (use-package mu4e-message
    :defines (mu4e-view-prefer-html
	          mu4e-html2text-command)
    :config
    (setq mu4e-view-prefer-html t)
    (defun my-render-html-message ()
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
	    (erase-buffer)
	    (shr-insert-document dom)
	    (goto-char (point-min))))
    (setq mu4e-html2text-command 'my-render-html-message))

  (use-package mu4e-draft
    :defines (mu4e-compose-signature
	          mu4e-compose-signature-auto-include)
    :init
    (setq
     mu4e-compose-signature-auto-include nil))

  ;; (use-package org-mu4e
  ;;   :defines (org-mu4e-convert-to-html)
  ;;   :commands (org-mu4e-compose-org-mode)
  ;;   :init
  ;;   (setq org-mu4e-convert-to-html t)
  ;;   (add-hook 'mu4e-compose-mode-hook
  ;;             'org-mu4e-compose-org-mode))

  (use-package mu4e-headers
    :defines (mu4e-headers-fields)
    :config
    (setq mu4e-headers-fields
          '((:human-date . 12)
            (:flags . 6)
            (:mailing-list . 15)
            (:from . 22)
            (:subject))))

  (use-package mu4e-view
    :defines (mu4e-view-actions
              mu4e-view-show-addresses
              mu4e-save-multiple-attachments-without-asking)
    :init
    (add-to-list 'mu4e-view-actions
		         '("View in browser" . mu4e-action-view-in-browser) t)
    :config
    (setq mu4e-save-multiple-attachments-without-asking t
          mu4e-view-show-addresses t))



  ;; hanld multi account
  (defun mu4e//search-account-by-mail-address (mailto)
  	"Return the account given an email address in MAILTO."
  	(car (rassoc-if (lambda (x)
  					  (equal (cadr (assoc 'user-mail-address x)) (car mailto)))
  					mu4e-account-alist)))
  (defun mu4e/set-account ()
  	"Set the account for composing a message.
  This function tries to guess the correct account from the email address first
  then fallback to the maildir."
  	(let* ((account
  			(if (> (length mu4e-account-alist) 1)
                (if mu4e-compose-parent-message
  				    (let* ((mailtos
  						    (mu4e-message-field mu4e-compose-parent-message :to))
  					       (mailto-account
  						    (car (cl-remove-if-not
  							      'identity
  							      (mapcar 'mu4e//search-account-by-mail-address
  									      mailtos))))
  					       (maildir
  						    (mu4e-message-field mu4e-compose-parent-message :maildir))
  					       (maildir-account
  						    (progn
  						      (string-match "/\\(.*?\\)/" maildir)
  						      (match-string 1 maildir))))
  				      (or mailto-account maildir-account))
  			      (funcall mu4e-completing-read-function
  					       "Compose with account: "
  					       (mapcar (lambda (var) (car var)) mu4e-account-alist)))
              (caar mu4e-account-alist)))
  		   (account-vars (cdr (assoc account mu4e-account-alist))))
  	  (if account-vars
  		  (mu4e//map-set account-vars)
  		(error "No email account found"))))

  (defun mu4e//map-set (vars)
  	"Setq an alist VARS of variables and values."
  	(mapc (lambda (var) (set (car var) (cadr var)))
  		  vars))

  (defun mu4e/mail-account-reset ()
  	"Reset mail account info to first."
  	(mu4e//map-set (cdar mu4e-account-alist)))

  (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account)
  (add-hook 'message-sent-hook 'mu4e/mail-account-reset)
  )

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  ;; (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  ;; (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-mode-line-display))

(use-package mu4e-maildirs-extension
  :ensure t
  :after mu4e
  :init
  (mu4e-maildirs-extension-load))

(use-package mu4e-conversation
  :disabled
  :ensure t
  :after mu4e
  :config
  (global-mu4e-conversation-mode))
(provide 'my-mu4e)

;;; my-mu4e.el ends here
