;;; my-jump.el --- my jump                           -*- lexical-binding: t; -*-

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
		 ;; (message "handlers-list: %s" ,handlers-list)
		 )
	   (add-hook ',mode-hook ',func)
	   ;; (with-eval-after-load 'bind-map
	   ;;   (spacemacs/set-leader-keys-for-major-mode ',mode
	   ;;                                             "gg" 'spacemacs/jump-to-definition
	   ;;                                             "gG" 'spacemacs/jump-to-definition-other-window))
	   )))

(defvar my-jump-original-buffer-alist '()
  "Buffer from which we jump to this symbol.")
(defun my/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
	(let ((old-buffer (current-buffer))
		  (old-point (point)))
	  (dolist (-handler my-jump-handlers)
		(let ((handler (if (listp -handler) (car -handler) -handler)))
		  (ignore-errors
			(call-interactively handler))
		  (when (or
				 (not (eq old-point (point)))
				 (not (equal old-buffer (current-buffer))))
			(add-to-list 'my-jump-original-buffer-alist old-buffer)
			(throw 'done t)))))
	(message "No jump handler was able to find this symbol.")))

(defun my/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (my/jump-to-definition))

(defun my/jump-back-to-origin ()
  "Jump back to origin buffer."
  (interactive)
  (if (length my-jump-original-buffer-alist)
	  (progn
		(funcall 'switch-to-buffer (car my-jump-original-buffer-alist))
		(setq-local my-jump-original-buffer-alist (cdr my-jump-original-buffer-alist))
		(delete-window))
	(error "No origial buffer")))

;; (my|define-jump-handlers lisp-interaction-mode elisp-slime-nav-find-elisp-thing-at-point)
;; (my|define-jump-handlers emacs-lisp-mode elisp-slime-nav-find-elisp-thing-at-point)
(my|define-jump-handlers elisp-slime-nav-mode elisp-slime-nav-find-elisp-thing-at-point)
(my|define-jump-handlers c-mode my/semantic-find-definition find-tag)
(my|define-jump-handlers c++-mode my/semantic-find-definition find-tag)
(my|define-jump-handlers go-mode godef-jump find-tag)
(my|define-jump-handlers python-mode elpy-goto-definition find-tag)

(global-set-key (kbd "M-.") 'my/jump-to-definition)
(global-set-key (kbd "C-M-.") 'my/jump-to-definition-other-window)
(global-set-key (kbd "C-M-,") 'my/jump-back-to-origin)

(provide 'my-jump)
;;; my-jump.el ends here
