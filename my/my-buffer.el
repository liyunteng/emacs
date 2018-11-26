;;; my-buffer.el --- buffer                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  liyunteng

;; Author: liyunteng(defun my/switch-to-help-buffer () <li_yunteng@163.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar my--special-buffer-name-alist
  '("*Help*"
    "*scratch*"
    "*Messages*")
  "Auto generate switch-to-buffer and view-buffer.")

(defmacro my|switch-to-buffer (buffer-name)
  "My switch to BUFFER-NAME buffer."
  (let* ((name (if (string-match "\\*\\(.*\\)\\*" buffer-name)
                   (match-string 1 buffer-name)
                 buffer-name))
         (fname (intern (format "my/switch-to-buffer--%s" name)))
         (foname (intern (format "my/switch-to-buffer-other-window--%s" name))))
    `(progn
       (defun ,fname ()
         ,(format "Switch to %s buffer." name)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
               (switch-to-buffer buffer)
             (message "No %s buffer" ,name))))

       (defun ,foname ()
         ,(format "Switch to %s buffer in other window." name)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
               (switch-to-buffer-other-window buffer)
             (message "No %s buffer" ,name)))))))

(defmacro my|view-buffer (buffer-name)
  "My view BUFFER-NAME buffer."
  (let* ((name (if (string-match "\\*\\(.*\\)\\*" buffer-name)
                   (match-string 1 buffer-name)
                 buffer-name))
         (fname (intern (format "my/view-buffer--%s" name)))
         (foname (intern (format "my/view-buffer-other-window--%s" name))))
    `(progn
       (defun ,fname ()
         ,(format "View %s buffer." name)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
               (view-buffer buffer)
             (message "No %s buffer" ,name))))

       (defun ,foname ()
         ,(format "View %s buffer in other window." name)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
               (view-buffer-other-window buffer)
             (message "No %s buffer" ,name)))))))


;; (mapc
;;  (lambda (entry)
;;    (my|switch-to-buffer entry)
;;    (my|view-buffer entry))
;;  my--special-buffer-name-alist)

(provide 'my-buffer)
;;; my-buffer.el ends here
