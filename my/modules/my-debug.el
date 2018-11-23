;;; my-debug.el --- custom                    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

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
(defvar my-debug nil)
(defvar my-debug-timer-threshold 0
  "Generate message if file takes longger than this number of seconds to load.")
(defvar my-debug-with-profile nil)
(defvar my-debug-with-timed-requires t)
(defvar my-debug-with-adv-timers nil)

(defcustom my-debug-buffer-name "*my-debug*"
  "The name of print debug info buffer, when my-debug turn on."
  :type 'string
  :group 'my-config)

(defun my-time-subtract-millis (b a)
  "Subtract B A to Millis."
  (* 1000.0 (float-time (time-subtract b a))))

(defun my-load-timer (func &rest args)
  "Used to time invocation of `require' or `load' FUNC ARGS."
  (let ((start (current-time))
        (required (car args))
        delta)
    (prog1
        (apply func args)
      (setq delta (my-time-subtract-millis (current-time) start))
      (when (> delta my-debug-timer-threshold)
        (with-current-buffer (get-buffer-create my-debug-buffer-name)
          (goto-char (point-max))
          (insert (format "    Time: [%-10.2fms] (%-10.2fms)\n    Feature: %s\n    File: %s\n\n"
                          (my-time-subtract-millis (current-time) before-init-time)
                          delta required load-file-name)))))))

(defmacro my|make-function-timer (func)
  "Used to time call to FUNC."
  `(lambda (origfunc &rest args)
     (let ((start (current-time))
           delta)
       (prog1
           (apply origfunc args)
         (setq delta (my-time-subtract-millis (current-time) start))
         (when (> delta my-debug-timer-threshold)
           (with-current-buffer (get-buffer-create my-debug-buffer-name)
             (goto-char (point-max))
             (insert (format "Load or rquire [%-10.2fms] (%-10.2fms)\n    Feature: %s\n    Args: %s\n\n"
                             (my-time-subtract-millis (current-time) before-init-time)
                             delta ',func args))))))))

(defmacro my|make-function-profiler (func)
  "Used to profile FUNC."
  `(lambda (origfunc &rest args)
     (if (profiler-running-p)
         (profiler-report)
       (profiler-start 'cpu))
     (prog1
         (apply origfunc args)
       (with-current-buffer (get-buffer-create my-debug-buffer-name)
         (goto-char (point-max))
         (insert (format "[%.2fms] Done profiling function: %s\n\n"
                         (my-time-subtract-millis (current-time) before-init-time) ',func)))
       (profiler-report))))

(defun my-debug-init ()
  "Set the debug hooks."
  (when my-debug-with-profile
    (profiler-start 'cpu+mem)
    (add-hook 'after-init-hook
              (lambda ()
                (run-with-idle-timer 2 nil (lambda ()
                                             (profiler-report)
                                             (profiler-stop))))))
  (when my-debug-with-timed-requires
    (with-current-buffer (get-buffer-create my-debug-buffer-name)
      (goto-char (point-max))
      (insert (format "Threshold set at %.2fms\n\n"
                      my-debug-timer-threshold)))

    (defadvice package-initialize (around my-timed-initialize activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (my-time-subtract-millis (current-time) start))
        (when (> delta my-debug-timer-threshold)
          (with-current-buffer (get-buffer-create my-debug-buffer-name)
            (goto-char (point-max))
            (insert (format "package-initialize took %.2fms\n" delta))))
        res))

    (defadvice require (around my-timed-require activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
              delta (my-time-subtract-millis (current-time) start))
        (when (> delta my-debug-timer-threshold)
          (with-current-buffer (get-buffer-create my-debug-buffer-name)
            (goto-char (point-max))
            (insert (format "File: %s Required: %s Time: %5.2fms\n"
                            load-file-name (ad-get-arg 0) delta))))
        res))

    (defadvice load (around my-timed-load activate)
      (let ((start (current-time)) res delta)
        (setq res ad-do-it
	      delta (my-time-subtract-millis (current-time) start))
        (when (> delta my-debug-timer-threshold)
          (with-current-buffer (get-buffer-create my-debug-buffer-name)
            (goto-char (point-max))
            (insert (format "File: %s Loaded: %s Time: %5.2fms\n"
                            load-file-name (ad-get-arg 0) delta))))
        res)))

  (when my-debug-with-adv-timers
    (with-current-buffer (get-buffer-create my-debug-buffer-name)
      (insert (format "Measured times greater than %.2fms:\n\n"
                      my-debug-timer-threshold)))

    (advice-add 'load :around #'my-load-timer)
    (advice-add 'require :around #'my-load-timer)
    (advice-add 'package-initialize
                :around
                (my|make-function-timer package-intialize))
    )
  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer (get-buffer-create my-debug-buffer-name)
                (goto-char (point-max))
                (insert (format "== Emacs finished initializing in [%.2fms] ==\n\n"
                                (my-time-subtract-millis after-init-time before-init-time) )))))
  ;; Keep debug-on-error on for stuff that is lazily loaded
  (add-hook 'after-init-hook (lambda () (setq debug-on-error t))))

(defun my--parse-command-line (args)
  "Handle specific command line ARGS.
The reason why we don't use the Emacs hooks for processing user defined
arguments is that we want to process these arguments as soon as possible."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
	  ("--my-debug"
	   (setq my-debug t))
          ("--profile"
	   (setq my-debug t)
           (setq my-debug-with-profile t))
          ("--time"
           (setq my-debug-with-timed-requires t)
           (when next-arg-digit
             (setq my-debug-timer-threshold next-arg-digit
                   i (1+ i)))
           (setq my-debug t))
          ("--adv-time"
           (setq my-debug-with-adv-timers t)
           (when next-arg-digit
             (setq my-debug-timer-threshold next-arg-digit
                   i (1+ 1)))
           (setq my-debug t))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))
(setq command-line-args (my--parse-command-line command-line-args))
(when my-debug (my-debug-init))

(provide 'my-debug)
;;; my-debug.el ends here
