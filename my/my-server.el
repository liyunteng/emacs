;;; my-server.el --- server                          -*- lexical-binding: t; -*-

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

(use-package server
  :commands (server-running-p
	            server-mode)
  :init

  ;; (setq server-use-tcp t
  ;;       server-host "127.0.0.1"
  ;;       server-port 55555)

  (setq server-log t
    server-name "server")

  (defun server-remove-kill-buffer-hook ()
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
  (add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

  (defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
    "Open file with emacsclient with cursors positioned on requested line.
  Most of console-based utilities prints filename in format
  'filename:linenumber'.  So you may wish to open filename in that format.
  Just call:

    emacsclient filename:linenumber

  and file 'filename' will be opened and cursor set on line 'linenumber'"
    (ad-set-arg 0
      (mapcar (lambda (fn)
  			        (let ((name (car fn)))
  			          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
  				          (cons
  				            (match-string 1 name)
  				            (cons (string-to-number (match-string 2 name))
  				              (string-to-number (or (match-string 3 name) ""))))
  			            fn))) files)))
  (unless (server-running-p) (server-start))
  )

(provide 'my-server)
;;; my-server.el ends here
