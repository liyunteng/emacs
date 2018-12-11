;;; info.el --- my info                              -*- lexical-binding: t; -*-

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


(setq user-full-name "liyunteng"
      user-mail-address "li_yunteng@163.com")

(eval-after-load 'mu4e
  (progn
    (setq mu4e-maildir-shortcuts '(("/163/INBOX" . ?i)
                                   ("/163/已发送" . ?s)
                                   ("/163/已删除" . ?d)))
    ;; for multi account
    (setq mu4e-account-alist
          '(("163"
             (user-mail-address "li_yunteng@163.com")
             (smtpmail-smtp-user "li_yunteng")
             (smtpmail-smtp-server "smtp.163.com")
             (smtpmail-stream-type ssl)
             (smtpmail-smtp-service 465)) ;tls
            ;; ("Gmail"
            ;;  (user-mail-address "liyunteng@gmail.com")
            ;;  (smtpmail-smtp-user "liyunteng@gmail.com")
            ;;  (smtpmail-smtp-server "smtp.gmail.com")
            ;;  (smtpmail-stream-type starttls)
            ;;  (smtpmail-smtp-service 587) ;starttls
            ;;  ;; (smtpmail-stream-type ssl)
            ;;  ;; (smtpmail-smtp-service 465) ;ssl
            ;;  )
            ))
    t))


;; gnus
;; imap
;; (with-eval-after-load 'gnus
;;   (add-to-list 'gnus-secondary-select-methods
;; 			   '(nnimap "163"
;; 						(nnimap-address "imap.163.com")
;; 						(nnimap-server-port "imaps")
;; 						(nnimap-stream ssl)
;; 						(nnimap-user "li_yunteng")
;; 						))
;;   (add-to-list 'gnus-secondary-select-methods
;; 			   '(nnimap "streamocean"
;; 						(nnimap-address "imap.qiye.163.com")
;; 						(nnimap-server-port "imaps")
;; 						(nnimap-stream ssl)
;; 						(nnimap-user "liyunteng@streamocean.com")))
;;   )
;; pop
;; (setq-default mail-sources
;; 			  '(
;; 				;; (pop :server "pop.163.com"
;; 				;; 	 :user "li_yunteng"
;; 				;; 	 :password "yun1988"
;; 				;; 	 )

;; 				;; (pop :server "pop.163.qiye.com"
;; 				;; 	 :user "liyunteng@streamocean.com"
;; 				;; 	 :password "Lyt532482644")
;; 				))
;; smtp
;; (setq-default message-send-mail-function 'smtpmail-send-it
;; 			  send-mail-function 'smtpmail-send-it
;; 			  ;; smtpmail-smtp-server "smtp.163.com"
;; 			  ;; smtpmail-stream-type 'ssl
;; 			  ;; smtpmail-smtp-service 994		;ssl 994/465
;; 			  ;; smtpmail-smtp-user "li_yunteng"

;; 			  smtpmail-smtp-server "smtp.qiye.163.com"
;; 			  smtpmail-stream-type 'starttls
;; 			  smtpmail-smtp-service 25		;ssl 994/465
;; 			  smtpmail-smtp-user "liyunteng@streamocean.com"

;; 			  ;; smtpmail-local-domain "localhost"
;; 			  ;; smtpmail-sendto-domain "smtp.qiye.163.com"
;; 			  ;; smtpmail-debug-info t
;; 			  )

;; (setq-default nnmail-split-fancy
;; 			  '(|
;; 				(any ".*@streamocean.com" "streamocean")
;; 				(any ".*@163.com" "163")
;; 				(any ".*@gmail.com" "gmail")
;; 				(from "liyunteng@streamocean.com\\|liyunteng@163.com" "From-me")
;; 				(to "liyunteng@streamocean.com\\|li_yunteng@163.com" "To-me")
;; 				"misc"))

;; mew
;; (setq-default mew-refile-guess-alist
;;               '(
;;                 ;; ("To:" ("liyunteng@streamocean.com" "+to/liyunteng"))
;;                 ("From:"
;;                  ("jzang@streamocean.com"   . "+from/joanna")
;;                  ("ali@streamocean.com"     . "+from/ali")
;;                  ("songwei@streamocean.com" . "+from/hr")
;;                  ("wenfeng@streamocean.com" . "+from/hr")
;;                  ;; ("@streamocean.com"        . "+from/streamocean")
;;                  (".*"                      . "+inbox")
;;                  )))

;; (setq-default mew-config-alist
;;               '(("default"
;;                  ("name" . "liyunteng")
;;                  ("user" . "liyunteng")
;;                  ("mail-domain" . "streamocean.com")
;;                  ("mailbox-type" . pop)
;;                  ("smtp-server" . "smtp.qiye.163.com")
;;                  ("smtp-port" . "25")
;;                  ("smtp-auth" . pass)
;;                  ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))

;;                  ("pop-user" . "liyunteng@streamocean.com")
;;                  ("pop-server" . "pop.qiye.163.com")
;;                  ("pop-port" . "110")
;;                  ("pop-auth" . pass)
;;                  ("pop-auth-list" . ("PLAIN" "LOGIN"))

;;                  ;; ("imap-user" . "liyunteng")
;;                  ;; ("imap-auth" . "PASSWD")
;;                  ;; ("imap-server" . "imap.streamocean.com")
;;                  ;; ("imap-port" . "143")
;;                  )))

(provide 'person-info)
;;; info.el ends here
