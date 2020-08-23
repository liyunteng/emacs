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
(setq debug-on-error t)
;; (setq max-lisp-eval-depth 400)
;; (setq max-specpdl-size 400)
(defvar my-debug nil)
(defvar my-init-times nil
  "A list of (FEATURE TYPE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

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
		      ("--time"
		        (setq my-debug t))
		      ("--my-debug"
		        (setq my-debug t))
		      (_ (push arg new-args))))
	    (setq i (1+ i)))
	  (nreverse new-args)))
(setq command-line-args (my--parse-command-line command-line-args))

(defun my-time-subtract-millis (b a)
  "Subtract B A to Millis."
  (* 1000.0 (float-time (time-subtract b a))))

(defadvice require (around my-require-times-ad () activate)
  (let* ((feature (ad-get-arg 0))
		      (already-loaded (memq feature features))
		      (require-start-time (and (not already-loaded) (current-time))))
	  ad-do-it
	  (when (and (not already-loaded) (memq feature features))
	    (let ((time (my-time-subtract-millis (current-time) require-start-time)))
		    (add-to-list 'my-init-times
					(list (format "%s" feature) "require" require-start-time time)
					t)))))
(defadvice load (around my-require-times-ad () activate)
  (let* ((filename (ad-get-arg 0))
		      (load-start-time (current-time)))
	  ad-do-it
	  (let ((time (my-time-subtract-millis (current-time) load-start-time)))
	    (add-to-list 'my-init-times
				(list filename "load" load-start-time time) t))))
(when my-debug
  (defadvice autoload (around my-autoload-times-ad () activate)
    (let* ((functionname (ad-get-arg 0))
            (filename (ad-get-arg 1))
	          (load-start-time (current-time)))
	    ad-do-it
	    (let ((time (my-time-subtract-millis (current-time) load-start-time)))
	      (add-to-list 'my-init-times
				  (list (format "%s#'%s" functionname filename) "autoload" load-start-time time) t)))))



(define-derived-mode my-init-times-mode tabulated-list-mode "Init-Times"
  "Show times taken to `require' or `load' packages."
  (setq tabulated-list-format
    `[("Start time (ms)" 20 my-init-times-sort-by-start-time-pred)
		   ("Time (ms)" 12 my-init-times-sort-by-load-time-pred)
		   ("Type" 12 t)
		   ("Feature" 100 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  (setq tabulated-list-entries #'my-init-times-tabulated-list-entries)
  ;; for auto-revert
  ;; (setq-local revert-buffer-function 'tabulated-list-revert)
  ;; (setq-local buffer-stale-function '(lambda (a) t))
  (tabulated-list-init-header))

(defun my-init-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
    (string-to-number (elt (nth 1 entry2) 0))))

(defun my-init-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 1))
    (string-to-number (elt (nth 1 entry2) 1))))

(defun my-init-times-tabulated-list-entries ()
  (cl-loop for (feature type start-time millis) in my-init-times
		with order = 0
		do (1+ order)
		collect (list order
						  (vector
						    (format "%.3f" (my-time-subtract-millis start-time before-init-time))
						    (format "%.3f" millis)
						    (format "[%8s]" type)
						    feature))))

(defun my/show-init-time ()
  "Show init time."
  (interactive)
  (if desktop-save-mode
    (message "Emacs startup time: %.2fms Desktop restore time: %.2fms"
	    (my-time-subtract-millis after-init-time before-init-time)
	    (my-time-subtract-millis after-desktop-read-time before-desktop-read-time))
    (message "Emacs startup time: %.2fms"
	    (my-time-subtract-millis after-init-time before-init-time))))

(defun my/show-init-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer
	  (get-buffer-create "*Init Times*")
    (my-init-times-mode)
    (tabulated-list-revert)
	  (auto-revert-mode +1)
    (display-buffer (current-buffer))))

(and my-debug
  (add-hook 'after-init-hook 'my/show-init-times))


(provide 'my-debug)
;;; my-debug.el ends here
