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
(defvar my-jump-default-backends'(xref-find-definitions)
  "List of jump default backends.")
(defvar-local my-jump-backends'(xref-find-definitions)
  "List of jump backends local to this buffer.")

(defmacro my|define-jump-backends (mode &rest backends)
  "Defines jump backends for the given MODE.
This defines a variable `my-jump-backends-MODE' to which
backends can be added, and a function added to MODE-hook which
sets `my-jump-bakcends' in buffers of that mode.

Example:

\(my-define-jump-backends c-mode\)
"
  (let ((mode-hook (intern (format "%S-hook" mode)))
	    (func (intern (format "my--jump-init-backends-%S" mode)))
	    (backends-list (intern (format "my--jump-backends-%S" mode))))
    `(progn
       (defvar ,backends-list ',backends
	     ,(format (concat "List of mode-specific jump backends for %S. "
			              "These take priority over those in "
			              "`my-jump-default-backends'.")
		          mode))
       (defun ,func ()
	     (setq my-jump-backends
	           (append ,backends-list
		               my-jump-default-backends))
	     )
       (add-hook ',mode-hook ',func))))

(defvar my-jump-ring-length 128)
(defvar my-jump-mark-ring (make-ring my-jump-ring-length)
  "My jump buffer history alist.")
(defun my/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
	      (old-point (point))
	      (marker (point-marker))
	      )
      (dolist (-backend my-jump-backends)
	    (let ((backend (if (listp -backend) (car -backend) -backend)))
	      (ignore-errors (call-interactively backend))
	      (when (or
		         (not (eq old-point (point)))
		         (not (equal old-buffer (current-buffer))))
	        (ring-insert my-jump-mark-ring marker)
	        (throw 'done t))
          )))
    (message "No jump backend was able to find this symbol.")
    ))

(defun my/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (my/jump-to-definition)
  )

(defun my/jump-back-to-origin ()
  "Jump back to origin buffer."
  (interactive)
  (when (ring-empty-p my-jump-mark-ring)
    (user-error "Jump mark ring is empty"))
  (let ((marker (ring-remove my-jump-mark-ring 0)))
    (switch-to-buffer (or (marker-buffer marker)
			              (user-error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)
    (if (> (length (window-list)) 1)
	    (delete-window))))

(my|define-jump-backends emacs-lisp-mode elisp-slime-nav-find-elisp-thing-at-point)
(my|define-jump-backends c-mode my/semantic-find-definition)
(my|define-jump-backends c++-mode my/semantic-find-definition)
(my|define-jump-backends go-mode godef-jump)
(my|define-jump-backends python-mode elpy-goto-definition)

(global-set-key (kbd "M-.") 'my/jump-to-definition)
(global-set-key (kbd "C-M-.") 'my/jump-to-definition-other-window)
(global-set-key (kbd "C-M-,") 'my/jump-back-to-origin)

;; (use-package smart-jump
;;   :ensure t
;;   :bind (("M-." . smart-jump-go)
;;          ("M-," . smart-jump-back)
;;          ("M-?" . smart-jump-references))
;;   :config
;;   (smart-jump-setup-default-registers))
(provide 'my-jump)
;;; my-jump.el ends here
