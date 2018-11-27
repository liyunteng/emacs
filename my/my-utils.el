;;; my-utils.el --- utils                            -*- lexical-binding: t; -*-

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

;; after-load

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun system-is-mac () "Is mac?"(eq system-type 'darwin))
(defun system-is-linux () "Is linux?" (eq system-type 'gnu/linux))
(defun system-is-mswindows () "Is ms window?" (eq system-type 'windows-nt))
(defun window-system-is-mac () "Window system is mac?" (memq (window-system) '(mac ns)))


(defmacro for (var from init to final do &rest body)
  "Execute a simple \"for\" loop.
     For example, (for i from 1 to 10 do (print i))."
  `(let ((,var ,init))
     (while (<= ,var ,final)
       ,@body
       (setq ,var (1+ ,var)))))

(defun add-functions-to-hook (hook funs &optional append local)
  "Add list of FUNS to HOOK."
  (dolist (fun funs)
    (add-hook hook fun append local)))

(defun add-function-to-hooks (fun hooks &optional append local)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun append local)))

(defun get-string-from-file (filepath)
  "Retrun FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun read-lines (filepath)
  "Retrun a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string (buffer-string) "\n" t)))

(defun my/insert-current-time-string ()
  "Insert the current time."
  (interactive "*")
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
;; (insert (format-time-string "%H:%M:%S" (current-time))))

(defun my/dos2unix-remove-M()
  "Remove ^M in files."
  (interactive)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match "")))

(defun my/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun my-derived-mode-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES.

Example:
(my-derived-mode-p 'lisp-interaction-mode 'text-mode 'prog-mode)"
  (let ((major-mode mode))
    (apply #'derived-mode-p modes)))

(defun my/echo (msg &rest args)
  "Display message in echo-area without logging it in *Messages* buffer.
MSG format-string or string.
ARGS if MSG is format-string ARGS contain message."
  (interactive)
  (let ((message-log-max nil))
    (apply #'message msg args)))



(defmacro my|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS,

The ClASS is defadvice's CLASS.
The body of the advice is in BODY.
Exaple:
   (my|advise-commands \"abc\" (proced) before (message \"from advise\"))"
  `(progn
     ,@(mapcar (lambda (command)
		 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
		    ,@body))
	       commands)))

(provide 'my-utils)
;;; my-utils.el ends here
