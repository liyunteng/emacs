;;; my-register.el --- register                      -*- lexical-binding: t; -*-

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

;; 设置register 显示默认不延迟
(setq register-preview-delay 0)

;; (add-to-list 'desktop-globals-to-save '(window-configuration))
(setq-default register-alist nil)
(set-register ?z '(file . "~/git/"))
;; (set-register ?f '(window-configuration 10))

(defun print-frameset (data)
  "The DATA is useless, print \"a frameset.\"."
  (null data)
  (princ "a frameset"))

(defun frameset-to-register (register)
  "Store the current frameset in register REGISTER.
Use \\[jump-to-register] to restore the frameset.
Argument is a character, naming the register.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Frameset to register: ")))
  (set-register register
                (registerv-make
                 (vector (frameset-save nil
                                        :app 'register
                                        :filters frameset-session-filter-alist)
                         ;; frameset-save does not include the value of point
                         ;; in the current buffer, so record that separately.
                         (frameset-frame-id nil)
                         (point-marker))
                 :print-func 'print-frameset
                 :jump-func #'frameset--jump-to-register)))

(provide 'my-register)
;;; my-register.el ends here
