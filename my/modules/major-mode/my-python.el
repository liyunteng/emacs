;;; my-python.el --- python                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  liyunteng

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


;; require see elpy-config

(use-package python
  :config
  (use-package elpy
	:ensure t
	:init
	(defvar my-python-virtualenv-dir (expand-file-name ".virtualenvs" "~/"))
	(defvar my-python-virtualenv-workon-name "default")
	(defvar my-python-virtualenv-workon-dir
	  (expand-file-name my-python-virtualenv-workon-name my-python-virtualenv-dir))

	(defvar my-python-elpy-dependency '("jedi" "importmagic" "autopep8" "yapf"))

	(setenv "WORKON_HOME" my-python-virtualenv-dir)

	:config
	(setq elpy-modules '(elpy-module-sane-defaults
						 elpy-module-company
						 elpy-module-eldoc
						 elpy-module-flymake
						 elpy-module-pyvenv
						 elpy-module-yasnippet
						 elpy-module-django))

	(setq elpy-rpc-backend "jedi"
		  elpy-dedicated-shells nil
		  )

	(if (not (executable-find "ipython"))
		(elpy-use-cpython)
	  (elpy-use-ipython))

	;; fix company-idel-delay from 0.01 to 1
	;; (defun elpy-module-company (command &rest args)
	;;   "Module to support company-mode completions ARGS."
	;;   (null args)
	;;   (pcase command
	;; 	(`global-init
	;; 	 (elpy-modules-remove-modeline-lighter 'company-mode)
	;; 	 )
	;; 	(`buffer-init
	;; 	 ;; We want immediate completions from company.
	;; 	 (set (make-local-variable 'company-idle-delay)
	;; 		  1)
	;; 	 ;; And annotations should be right-aligned.
	;; 	 (set (make-local-variable 'company-tooltip-align-annotations)
	;; 		  t)
	;; 	 ;; Also, dabbrev in comments and strings is nice.
	;; 	 (set (make-local-variable 'company-dabbrev-code-everywhere)
	;; 		  t)
	;; 	 ;; Add our own backend and remove a bunch of backends that
	;; 	 ;; interfere in Python mode.
	;; 	 (set (make-local-variable 'company-backends)
	;; 		  (cons 'elpy-company-backend
	;; 				(delq 'company-semantic
	;; 					  (delq 'company-ropemacs
	;; 							(delq 'company-capf
	;; 								  (mapcar #'identity company-backends))))))
	;; 	 (company-mode 1)
	;; 	 (when (> (buffer-size) elpy-rpc-ignored-buffer-size)
	;; 	   (message
	;; 		(format
	;; 		 (concat "Buffer larger than elpy-rpc-ignored-buffer-size (%d)."
	;; 				 " Elpy will turn off completion.")
	;; 		 elpy-rpc-ignored-buffer-size))))
	;; 	(`buffer-stop
	;; 	 (company-mode -1)
	;; 	 (kill-local-variable 'company-idle-delay)
	;; 	 (kill-local-variable 'company-tooltip-align-annotations)
	;; 	 (kill-local-variable 'company-backends))
	;; 	))

	(defun my-install-python-virtualenv ()
	  "My install python virtualenv."
	  (if (or (not (file-exists-p my-python-virtualenv-dir))
			  (not (file-exists-p my-python-virtualenv-workon-dir)))
		  (progn
			(let ((virtualenvbin (executable-find "virtualenv")))
			  (if (null virtualenvbin)
				  (message "virtualenv not found, please install virtualenv")
				(message "%s %s ..." virtualenvbin my-python-virtualenv-workon-dir)
				(shell-command (format "%s %s" virtualenvbin my-python-virtualenv-workon-dir) nil)

				(setenv "WORKON_HOME" my-python-virtualenv-dir)
				(pyvenv-workon my-python-virtualenv-workon-name)
				(let ((install-cmd (or (executable-find "pip")
									   (executable-find "easy_install"))))
				  (if install-cmd
					  (mapc (lambda (n)
							  (message "%s installing %s ..." install-cmd n)
							  (shell-command (format "%s install %s" install-cmd n) nil))
							my-python-elpy-dependency)
					(message "pip/easy_install not found, please install pip/easy_install")))
				(elpy-rpc-restart)
				(message "Done"))))))

	(defun my/elpy-shell-kill ()
	  "My elpy shell kill."
	  (interactive)
	  (elpy-shell-kill t))

	(defun my/elpy-shell-kill-all ()
	  "My elpa shell kill all."
	  (interactive)
	  (elpy-shell-kill-all t nil))

	(my-install-python-virtualenv)
	(pyvenv-workon my-python-virtualenv-workon-name)
	)

  (setq python-indent-guess-indent-offset nil)

  (defun my-python-mode-keys ()
	"My python key."
	;; (define-key jedi-mode-map (kbd "TAB") 'jedi:complete)
	(local-set-key (kbd "C-c C-f") 'python-shell-send-file)
	(local-set-key (kbd "C-c C-e") 'python-shell-send-defun)

	(define-key elpy-mode-map (kbd "C-c C-j") 'elpy-goto-definition)
	(define-key elpy-mode-map (kbd "C-c C-J") 'elpy-goto-definition-other-window)
	(define-key elpy-mode-map (kbd "C-c C-b") 'pop-tag-mark)

	(define-key elpy-mode-map (kbd "C-c C-q") 'my/elpy-shell-kill)
	(define-key elpy-mode-map (kbd "C-c C-Q") 'my/elpy-shell-kill-all)
	(define-key elpy-mode-map (kbd "C-c C-k") 'kill-region)
	)

  (defun my-python-mode-hook ()
	"My python mode hook."
	(when semantic-mode
	  (semantic-mode -1))

	(set (make-local-variable 'highlight-indentation-mode) nil)

	(elpy-enable)
	(subword-mode +1)
	(set (make-local-variable 'tab-width) 4)
	(setq-local electric-layout-rules
				'((?: . (lambda ()
						  (and (zerop (first (syntax-ppss)))
							   (python-info-statement-starts-block-p)
							   'after)))))
	(when (fboundp #'python-imenu-create-flat-index)
	  (setq-local imenu-create-index-function
				  #'python-imenu-create-flat-index))
	(add-hook 'post-self-insert-hook
			  #'electric-layout-post-self-insert-function nil 'local)
	)

  (add-hook 'python-mode-hook 'my-python-mode-keys)
  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defun my-python-shell-mode-hook ()
	"My python shell mode hook."
	(when semantic-mode
	  (semantic-mode -1))
	(set (make-local-variable 'tab-width) 4)
	(if (boundp 'python-shell-completion-native-enable)
		(setq python-shell-completion-native-enable t)))

  (add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook))

(provide 'my-python)

;;; my-python.el ends here
