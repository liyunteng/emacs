;;; my-syslog.el --- syslog                          -*- lexical-binding: t; -*-

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

;; syslog-mode
(use-package hide-lines
  :ensure t)
;; (unless (package-installed-p 'syslog-mode)
;;   (package-install 'syslog-mode))
(autoload 'syslog-mode "syslog-mode")
(use-package syslog-mode
  :defines syslog-setup-on-load
  :init (setq syslog-setup-on-load t)
  :mode ("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|dmesg\\|\\.log.*\\)\\'" . syslog-mode)
  :config
  (defun my-enable-goto-address-mode ()
    "Enable goto address mode."
    (goto-address-mode t))

  (add-hook 'syslog-mode-hook 'my-enable-goto-address-mode)
  )
;; (add-to-list
;;  'auto-mode-alist
;;  '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|dmesg\\|\\.log.*\\)\\'" . syslog-mode))


(provide 'my-syslog)
;;; my-syslog.el ends here
