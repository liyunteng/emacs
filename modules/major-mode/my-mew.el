;;; my-mew.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: abbrev

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
(my-require-package 'mew)

(setq-default mew-use-cached-passwd t)
(setq-default mew-passwd-timer-unit 999)
(setq-default mew-passwd-lifetime 999)

;; (setq-default mew-pop-size 0)
(setq-default mew-pop-delete nil)
(setq-default mew-auto-get t)
(setq-default mew-auto-flush-queue t)
(setq-default mew-use-unread-mark nil)

(setq-default mew-use-biff t)
(setq-default mew-use-biff-bell t)
(setq-default mew-biff-interval 5)

(setq-default mew-refile-auto-refile-confirm nil)
(setq-default mew-refile-auto-refile-skip-any-mark nil)
(setq-default mew-use-fast-refile t)

(setq-default mew-refile-guess-alist
              '(
                ;; ("To:" ("liyunteng@streamocean.com" "+to/liyunteng"))
                ("From:"
                 ("jzang@streamocean.com"   . "+from/joanna")
                 ("ali@streamocean.com"     . "+from/ali")
                 ("songwei@streamocean.com" . "+from/hr")
                 ("wenfeng@streamocean.com" . "+from/hr")
                 ;; ("@streamocean.com"        . "+from/streamocean")
                 (".*"                      . "+inbox")
                 )))

(setq-default mew-config-alist
              '(("default"
                 ("name" . "liyunteng")
                 ("user" . "liyunteng")
                 ("mail-domain" . "streamocean.com")
                 ("mailbox-type" . pop)
                 ("smtp-server" . "smtp.qiye.163.com")
                 ("smtp-port" . "25")
                 ("smtp-auth" . pass)
                 ("smtp-auth-list" . ("PLAIN" "LOGIN" "CRAM-MD5"))

                 ("pop-user" . "liyunteng@streamocean.com")
                 ("pop-server" . "pop.qiye.163.com")
                 ("pop-port" . "110")
                 ("pop-auth" . pass)
                 ("pop-auth-list" . ("PLAIN" "LOGIN"))

                 ;; ("imap-user" . "liyunteng")
                 ;; ("imap-auth" . "PASSWD")
                 ;; ("imap-server" . "imap.streamocean.com")
                 ;; ("imap-port" . "143")
                 )))

(after-load 'mew
  (add-to-list 'mew-cite-fields "Message-ID:")

  (defun my-mew-mode-hook ()
    (my/no-trailing-whitespace)
    (auto-image-file-mode t))

  (add-hook 'mew-summary-mode-hook 'my-mew-mode-hook)
  (add-hook 'mew-message-mode-hook 'my-mew-mode-hook))
(provide 'my-mew)
;;; my-mew.el ends here
