;;; my-server.el --- server                          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: lisp, abbrev

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


(require 'server)
;;(setq-default noninteractive t)


;; (setq-default server-host "127.0.0.1")
;; (setq-default server-port 55555)
;; (setq-default server-use-tcp t)

(setq-default server-buffer "*server*")
(setq-default server-log t)
(setq-default server-name "server")
(setq-default server-msg-size 4096)
(setq-default server-buffer-clients nil)

(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

(unless (server-running-p)
  (server-mode t))

(provide 'my-server)
;;; my-server.el ends here
