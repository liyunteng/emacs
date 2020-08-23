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

(defmacro my|view-buffer (buffer-name)
  "My view BUFFER-NAME buffer."
  (let* ((bn (if (symbolp buffer-name)
               (symbol-value buffer-name)
               buffer-name))
          (name (if (string-match "\\*+\\([a-z][A-Z]+\\)\\*+" bn)
                  (match-string 1 bn)
                  bn))
          (vname (intern (format "my/view-buffer--%s" name)))
          (voname (intern (format "my/view-buffer-other-window--%s" name)))
          (sname (intern (format "my/switch-buffer--%s" name)))
          (soname (intern (format "my/switch-buffer-other-window--%s" name))))
    `(progn
       (defun ,vname ()
         ,(format "View %s buffer." bn)
         (interactive)
         (display-buffer  ,buffer-name 'display-buffer-in-same-window))

       (defun ,voname ()
         ,(format "View %s buffer in other window." bn)
         (interactive)
         (display-buffer ,buffer-name 'display-buffer-in-side-window))

       (defun ,sname ()
         ,(format "Switch %s buffer." bn)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
             (switch-to-buffer buffer)
             (message "No %s buffer" ,name))))

       (defun ,soname ()
         ,(format "Switch %s buffer in other window." bn)
         (interactive)
         (let ((buffer (get-buffer ,buffer-name)))
           (if buffer
             (switch-to-buffer-other-window buffer)
             (message "No %s buffer" ,name)))))))


(defun my/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
         (buffer-predicate
           (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
      (or (cl-find-if (lambda (buffer)
                        (and (not (eq buffer current-buffer))
                          (or (null buffer-predicate)
                            (funcall buffer-predicate buffer))))
            (mapcar #'car (window-prev-buffers window)))
        ;; `other-buffer' honors `buffer-predicate' so no need to filter
        (other-buffer current-buffer t)))))

(defun my/alternate-buffer-other-window (&optional window)
  "Switch back and forth between current and last buffer in other window."
  (interactive)
  (let ((current-buffer (window-buffer window))
         (buffer-predicate
           (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer-other-window
      (or (cl-find-if (lambda (buffer)
                        (and (not (eq buffer current-buffer))
                          (or (null buffer-predicate)
                            (funcall buffer-predicate buffer))))
            (mapcar #'car (window-prev-buffers window)))
        ;; `other-buffer' honors `buffer-predicate' so no need to filter
        (other-buffer current-buffer t)))))

(unless (fboundp 'kill-current-buffer)
  (defun kill-current-buffer ()
    "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'.

This is like `kill-this-buffer', but it doesn't have to be invoked
via the menu bar, and pays no attention to the menu-bar's frame."
    (interactive)
    (let ((frame (selected-frame)))
      (if (and (frame-live-p frame)
            (not (window-minibuffer-p (frame-selected-window frame))))
        (kill-buffer (current-buffer))
        (abort-recursive-edit)))))

(my|view-buffer "*Help*")
(my|view-buffer "*Messages*")
(my|view-buffer "*scratch*")
(my|view-buffer "*info*")
(my|view-buffer "*Backtrace*")

(global-set-key (kbd "M-g i") 'my/switch-buffer--info)
(global-set-key (kbd "M-g s") 'my/switch-buffer-other-window--scratch)
(global-set-key (kbd "M-g m") 'my/view-buffer-other-window--Messages)
(global-set-key (kbd "M-g h") 'my/view-buffer-other-window--Help)
(global-set-key (kbd "M-g b") 'my/view-buffer-other-window--Backtrace)

(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x m") 'my/alternate-buffer)
(global-set-key (kbd "C-x C-m") 'my/alternate-buffer-other-window)

(provide 'my-buffer)
;;; my-buffer.el ends here
