;;; my-gnus.el --- gnus                              -*- lexical-binding: t; -*-

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

(use-package message
  :defer t
  :config
  (use-package sendmail
	:defer t
	:config
	(setq send-mail-function 'smtpmail-send-it
		  )
	)
  (use-package smtpmail
	:defer t
	:config
	(setq
	 ;; smtpmail-smtp-server "smtp.163.com"
	 ;; smtpmail-stream-type 'ssl
	 ;; smtpmail-smtp-service 25		;ssl 994/465
	 ;; smtpmail-smtp-user "li_yunteng"

	 smtpmail-smtp-server "smtp.qiye.163.com"
	 smtpmail-stream-type 'starttls
	 smtpmail-smtp-service 25		;ssl 994/465
	 smtpmail-smtp-user "liyunteng@streamocean.com"

	 ;; smtpmail-local-domain "localhost"
	 ;; smtpmail-sendto-domain "smtp.qiye.163.com"
	 ;; smtpmail-debug-info t
	 )
	)

  (setq message-confirm-send t						;防止误发邮件, 发邮件前需要确认
		message-kill-buffer-on-exit t				;设置发送邮件后删除buffer
		message-from-style 'angles					;`From' 头的显示风格
		message-syntax-checks '((sender . disabled));语法检查
		message-send-mail-function 'smtpmail-send-it

		message-cite-function 'message-cite-original-without-signature ;;引用设置：不要原来的签名，引用全文

		message-kill-buffer-on-exit t
		message-elide-ellipsis "[...]\n"
		)
  (setq message-signature
		(concat
		 "李云腾 (Li Yunteng)\n"
		 "Email: liyunteng@streamocean.com\n"))

  (add-hook 'mail-citation-hook 'sc-cite-original)
  ;;写消息时如何打开自动折行 (word-wrap) ？
  ;; (add-hook 'message-mode-hook
  ;; 			(lambda ()
  ;; 			  (setq fill-column 80)
  ;; 			  (turn-on-auto-fill)))
  )

(use-package mu4e
  :commands (mu4e mu4e-compose-new)
  :init
  (global-set-key (kbd "C-x m") 'mu4e-compose-new)
  (defvar mu4e-account-alist nil
	"Account alist for custom multi-account compose.")
  :config
  (use-package mu4e-vars
	:config
	(setq mu4e-maildir "~/Maildir"
		  mu4e-trash-folder "/Trash"
		  mu4e-refile-folder "/Archive"
		  mu4e-sent-folder "/Sent"
		  mu4e-drafts-folder "/Drafts"
		  mu4e-get-mail-command "offlineimap"
		  mu4e-update-interval nil
		  mu4e-compose-signature-auto-include nil
		  mu4e-view-show-images t
		  mu4e-view-show-addresses t
		  mu4e-view-prefer-html t
		  )

	(setq mu4e-maildir-shortcuts
		  '(("/streamocean/INBOX" . ?t)
			("/163/INBOX" . ?c)))

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
	)

  (use-package mu4e-alert
    :defer t
	:ensure t
    :init (with-eval-after-load 'mu4e
			(mu4e-alert-enable-notifications)
			(mu4e-alert-enable-mode-line-display)))
  (use-package mu4e-maildirs-extension
    :defer t
	:ensure t
    :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load)))

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
					   "Compose with account:"
					   (mapcar (lambda (var) (car var)) mu4e-account-alist))))
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


  (setq mu4e-completing-read-function 'completing-read)
  ;; (add-to-list 'mu4e-view-actions
  ;; 			   '("View in browser" . mu4e-action-view-in-browser) t)

  (add-hook 'mu4e-compose-pre-hook 'mu4e/set-account)
  (add-hook 'message-sent-hook 'mu4e/mail-account-reset)

  (setq mu4e-account-alist
        '(("streamocean"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior sent)
           (mu4e-sent-folder "/streamocean/已发送")
           (mu4e-drafts-folder "/streamocean/草稿箱")
           (user-mail-address "liyunteng@streamocean.com")
           (user-full-name "liyunteng"))
          ("163"
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/163/已发送")
           (mu4e-drafts-folder "/163/草稿箱")
           (user-mail-address "li_yunteng@163.com")
           (user-full-name "liyunteng"))))
  (mu4e/mail-account-reset)
  )
(provide 'my-mu4e)
;;; my-gnus.el ends here
