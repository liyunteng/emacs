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

;; elisp version of try...catch...finally
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn, fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun my-system-is-mac () "Is mac?"(eq system-type 'darwin))
(defun my-system-is-linux () "Is linux?" (eq system-type 'gnu/linux))
(defun my-system-is-mswindows () "Is ms window?" (eq system-type 'windows-nt))
(defun my-window-system-is-mac () "Window system is mac?" (memq (window-system) '(mac ns)))


(defun my-add-to-hook (hook funs)
  "Add list of FUNS to HOOK."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun my-add-to-hooks (fun hooks)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun my-add-all-to-hook (hook &rest funs)
  "Add FUNS to HOOK."
  (my-add-to-hook hook funs))

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

(defun string-all-matches (regex str &optional group)
  "Find all matches for REGEX within STR, returning the full match string or group GROUP."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

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

(defun my/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))

(defun my/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for a new name.

Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
		   (short-name (file-name-nondirectory filename))
		   (new-name (if new-filename new-filename
					   (read-file-name
						(format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
			 (error "A buffer named '%s' already exists!" new-name))
			(t
			 (let ((dir (file-name-directory new-name)))
			   (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
				 (make-directory dir t)))
			 (rename-file filename new-name 1)
			 (when buffer
			   (kill-buffer buffer)
			   (find-file new-name))
			 (when (fboundp 'recentf-add-file)
			   (recentf-add-file new-name)
			   (recentf-remove-if-non-kept filename))
			 (when (projectile-project-p)
			   (call-interactively #'projectile-invalidate-cache))
			 (message "File '%s' successfully renamed to '%s'" short-name
					  (file-name-nondirectory new-name)))))))

;; from magnars
(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        ;; (error "Buffer '%s' is not visiting a file!" name)
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun my/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

FILENAME is file or directory.
When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p (format "Are you sure you want to delete %s? " filename)))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

(defun my/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `my/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'my/delete-file filename t))

;; from magnars
(defun my/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (when (yes-or-no-p (format "Are you sure you want to delete %s? " filename))
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "Deleted file %s" filename)))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))
;;(define-key my-mode-map (kbd "C-c C-s") 'my/rotate-windows-backward)

(defun my--revert-buffer-function (ignore-auto noconfirm)
  "Revert buffer if buffer without file, or call revert-buffer--default."
  (if (buffer-file-name)
      (funcall #'revert-buffer--default ignore-auto noconfirm)
    (call-interactively major-mode)))
;; (setq revert-buffer-function 'my--revert-buffer-function)


(defun my-derived-mode-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES."
  (let ((major-mode mode))
    (apply #'derived-mode-p modes)))

(defun my/echo (msg &rest args)
  "Display message in echo-area without logging it in *Messages* buffer.
MSG format-string or string.
ARGS if MSG is format-string ARGS contain message."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun my-dump (varlist buffer)
  "Insert the setq statement to recreate the variables in VARLIST to BUFFER.

Example:
	(setq a \"a\" b \"b\" c \"c\")
	(setq x '(a b c))
	(my-dump x (get-buffer \"*scratch*\"

Output:
	(setq a (quote \"a\"))
	(setq b (quote \"b\"))
    (setq c (quote \"c\"))."
  (cl-loop for var in varlist do
           (print (list 'setq var (list 'quote (symbol-value var)))
                  buffer)))

(defun my-dump-vars-to-file (varlist filename)
  "Dump VARLIST to FILENAME."
  (with-temp-file filename
    (my-dump varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))





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
