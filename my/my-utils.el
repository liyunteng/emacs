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

(defun my/dos2unix-remove-M(&optinal file)
  "Remove ^M in files."
  (interactive)
  (if file (find-file file))
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match "")))

(defun my/dos2unix (&optional file)
  "Convert the current buffer to UNIX file format."
  (interactive)
  (if file (find-file file))
  (set-buffer-file-coding-system 'utf-8-unix nil)
  (save-buffer))

(defun my/unix2dos (&optional file)
  "Convert the current buffer to DOS file format."
  (interactive)
  (if file (find-file file))
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

(defun my/indent-file (filename)
  "Indent FILENAME"
  (interactive "f")
  (if (file-exists-p filename)
      (condition-case nil
          (progn
            (find-file filename)
            (indent-region (point-min) (point-max))
            (save-buffer)
            (kill-buffer nil))
        (user-error "failed inent %s" filename))
    (user-error "Invalid filename %s" filename)))

(defun my--indent-directory (dir regex)
  "Indent Directory."
  (interactive "D")
  (if (and (file-exists-p dir))
      (dolist (file (directory-files-recursively dir regex))
        (message "indenting %s" file)
        (my/indent-file file)
        (message "indented %s\n" file))
    (user-error "Invalid directory %s" dir)))
(defun my/dos2unix-directory (dir regex)
  "Convert dos file to utf-8-unix.
Example:
(my/dos2unix-directory \"/home/lyt/core/client/libiHi\" \".*\\.\\(c\\|h\\|cpp\\|hpp\\)\")
"
  (interactive "D")
  (if (and (file-exists-p dir))
      (dolist (file (directory-files-recursively dir regex))
        (message "dos2unix %s" file)
        (my/dos2unix file)
        (kill-current-buffer))
    (user-error "Invalid directory %s" dir)))

(defun my/indent-my-emacs-lisp ()
  (interactive)
  (my--indent-directory my-dir ".*.el"))


(defun my-mplist-get (plist prop)
  "Get the values associated to  PLIST of PROP, a modified plist."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun my-mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; keybinding
(defun my--create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.

Supported properties:
`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((global-key (my-mplist-get props :global-key))
        (def-key (my-mplist-get props :define-key)))
    (append
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defvar my-toggles '()
  "List of all declared toggles.
The structure of an element is a property list (name :func FUNCTION :doc STRING :key STRING).")

(defmacro my|add-toggle (name &rest props)
  "Add a toggle with NAME symbol.

This macro creates the following functions:
- my/toggle-NAME switches on or off depending on the current state
- my/toggle-NAME-on only switches on if currently disabled
- my/toggle-NAME-off only switches off if currently enabled

Avaiblabe PROPS:

`:status EXPRESSION'
    The EXPRESSION to evaluate to get the current status of the toggle.

`:if EXPRESSION'
    If this EXPRESSION evaluate to nil then no attempt to update the toggle
    status will be performed.

`:on BODY'
    Evaluate BODY when the toggle is switched on.

`:off BODY'
    Evaluate BODY when the toggle is switched off.

`:documentation STRING'
    STRING describes what the toggle does.

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of 'PREFIX-ARG' (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"on\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `my--create-key-binding-form' can be
used.

Example:
 (my|add-toggle company-mode
  :status (not (equal company-mode nil))
  :on (company-mode 1)
  :off (company-mode -1)
  :define-key  (lisp-interaction-mode-map . \"C-x i\")
  :global-key \"C-# i\")

Create my/toggle-company-mode my/toggle-company-mode-on and
my/toggle-company-mode-off."

  (declare (indent 1))
  (let* ((wrapper-func (intern (format "my/toggle-%s"
                                       (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (my-mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (my-mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bindkeys (my--create-key-binding-form props wrapper-func))
         ;; we evaluate condition and status only if they are a list or
         ;; a bound symbol
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicate ,wrapper-func-status) ',props)
             my-toggles)
       ;; toggle function
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "%s\nToggle %s on and off." doc (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (when (called-interactively-p 'any)
                          (message ,(format "%s disabled." name))))
               ,@on-body
               (when (called-interactively-p 'any)
                 (message ,(or on-message (format "%s enabled." name)))))
           (message "This toggle is not supported.")))
       ;; predicate function
       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         ,status-eval)
       ;; Only define on- or off-functions when status is available
       ,@(when status
           ;; on-function
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             ;; off-function
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bindkeys)))

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

;; Local Variables:
;; compile-command: "make -k "
;; End:
