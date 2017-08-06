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
;; elisp version of try...catch...finally

;; after-load
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


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


;; COPY FROM SPACEMACS
(defun my-add-to-load-path (dir &optional append)
  "Add DIR to load path, if APPEND add to end."
  (add-to-list 'load-path dir append))

(defun my-add-to-load-path-if-exists (dir &optional append)
  "If DIR exists in the file system, add it to `load-path'.
If APPEND add to end."
  (when (file-exists-p dir)
    (my-add-to-load-path dir append)))

(defun my-add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun my-add-to-hooks (fun hooks)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun my-add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (my-add-to-hook hook funs))

(defun my/echo (msg &rest args)
  "Display MSG in echo-area withou logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun my-derived-mode-p (mode &rest modes)
  "Non-nil if MODE is derived from one of MODES."
  (let ((major-mode mode))
    (apply #'derived-mode-p modes)))

(defun my-system-is-mac () "Is mac?"(eq system-type 'darwin))
(defun my-system-is-linux () "Is linux?" (eq system-type 'gnu/linux))
(defun my-system-is-mswindows () "Is ms window?" (eq system-type 'windows-nt))

(defun my-window-system-is-mac () "Window system is mac?" (memq (window-system) '(mac ns)))

(defun my-run-prog-mode-hooks () "Run `prog-mode-hook'." (run-hooks 'prog-mode-hook))
(defun my-run-text-mode-hooks () "Run `text-mode-hook'." (run-hooks 'text-mode-hook))

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

(defun my-dump (varlist buffer)
  "Dump VARLIST to BUFFER."
  (cl-loop for var in varlist do
           (print (list 'setq var (list 'quote (symbol-value var)))
                  buffer)))

(defun my-dump-vars-to-file (varlist filename)
  "Dump VARLIST to FILENAME."
  (with-temp-file filename
    (my-dump varlist (current-buffer))
    (make-directory (file-name-directory filename) t)))

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


(defun my/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
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

(defun my/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


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
  "List of all declared toggles. The structure of an element is a
property list (name :func FUNCTION :doc STRING :key STRING).")

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
    SYMBOL is bound to the raw value of prefix-arg (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"on\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `my--create-key-binding-form' can be
used.

For example:
\(my|add-toggle company-mode
  :status (not (equal company-mode nil))
  :on (company-mode 1)
  :off (company-mode -1)
  :define-key  (lisp-interaction-mode-map . \"C-x i\")
  :global-key \"C-# i\"
  \)
"
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
         ,(format "Toggle %s on and off." (symbol-name name))
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


;; jump
(defvar my-default-jump-handlers '()
  "List of jump handlers available in every mode.")
(defvar-local my-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro my|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `my-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `my-jump-handlers' in buffers of that mode.

Example:

\(my-define-jump-handlers c-mode\)
"
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my--init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "my-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`my-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq my-jump-handlers
			   (append ,handlers-list
					   my-default-jump-handlers))
         (message "handlers-list: %s" ,handlers-list))
       (add-hook ',mode-hook ',func)
       ;; (with-eval-after-load 'bind-map
       ;;   (spacemacs/set-leader-keys-for-major-mode ',mode
       ;;                                             "gg" 'spacemacs/jump-to-definition
       ;;                                             "gG" 'spacemacs/jump-to-definition-other-window))
       )))

(defun my/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler my-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun my/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `my/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (my/jump-to-definition)))

(my|define-jump-handlers lisp-interaction-mode elisp-slime-nav-find-elisp-thing-at-point)
(my|define-jump-handlers emacs-lisp-mode elisp-slime-nav-find-elisp-thing-at-point)
(my|define-jump-handlers c-mode semantic-ia-fast-jump find-tag)
(my|define-jump-handlers c++-mode semantic-ia-fast-jump find-tag)


;; use package
(my-require-package 'bind-key)
(my-require-package 'use-package)
(require 'use-package)
(setq use-package-verbose init-file-debug
      use-package-inject-hooks t)

(defconst my--use-package-add-hook-keywords '(:pre-init
                                              :post-init
                                              :pre-config
                                              :post-config)
  "Use package add-abbrev keywords.")
(defmacro my|use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (my|use-package-add-hook package-name
     [:keyword [option]]...)

:pre-init      Code to run before the default `:init' configuration.
:post-init     Code to run after the default `:init' configuration.
:pre-config    Code to run before the default `:config' configuration.
:post-config   Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can
override lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword my--use-package-add-hook-keywords)
      (let ((body (my-mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body t)) expanded-forms)))))
    `(progn ,@expanded-forms)))



;; test
;; (my|use-package-add-hook multi-term
;;   :pre-init
;;   (message "pre-init multi-term")
;;   :post-init
;;   (message "post-init multi-term")
;;   :pre-config
;;   (message "pre-config multi-term")
;;   :post-config
;;   (message "post-config multi-term")
;;   )
;; (use-package multi-term
;;   :defer t
;;   :init
;;   (progn
;;     (message "use-package init multi-term")
;;     ;; (my-require-package 'multi-term)
;;     )
;;   :config
;;   (message "use-package config multi-term")
;;   :demand t)

(defmacro my|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))
(provide 'my-utils)
;;; my-utils.el ends here
