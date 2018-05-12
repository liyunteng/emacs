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

(use-package python
  :commands (python-mode run-python)
  :init
  (setq python-indent-guess-indent-offset nil)
  (defun my-python-mode-hook ()
	"My python mode hook."
	(when semantic-mode
	  (semantic-mode -1))
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
	(local-set-key (kbd "C-c C-f") 'python-shell-send-file)
	(local-set-key (kbd "C-c C-e") 'python-shell-send-defun)
	)

  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defun my-python-shell-mode-hook ()
	"My python shell mode hook."
	(when semantic-mode
	  (semantic-mode -1))
	(set (make-local-variable 'tab-width) 4)
	(if (boundp 'python-shell-completion-native-enable)
		(setq python-shell-completion-native-enable t)))

  (add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

  :config
  (if (not (executable-find "ipython"))
	  (setq python-shell-interpreter "python"
			python-shell-interpreter-args "-i")
	(setq python-shell-interpreter "ipython"
		  python-shell-interpreter-args "-i"))

  (use-package elpy
  	:ensure t
  	:commands (elpy-mode)
  	:bind
  	(:map elpy-mode-map
  		  ("C-c C-d" . elpy-doc)
  		  ("C-c C-j" . elpy-goto-definition)
  		  ("C-c C-J" . elpy-goto-definition-other-window)

  		  ("C-c C-q" . my/elpy-shell-kill)
  		  ("C-c C-Q" . my/elpy-shell-kill-all)
  		  ("C-c C-k" . kill-region))
  	:init
  	;; (defvar my-python-virtualenv-dir (expand-file-name ".virtualenvs" "~/"))
  	;; (defvar my-python-virtualenv-workon-name "default")
  	;; (defvar my-python-virtualenv-workon-dir
  	;;   (expand-file-name my-python-virtualenv-workon-name my-python-virtualenv-dir))

  	;; (defvar my-python-elpy-dependency '("jedi" "importmagic" "yapf")) ;autopep8

  	;; (setenv "WORKON_HOME" my-python-virtualenv-dir)

  	(setq elpy-modules '(elpy-module-sane-defaults
  						 elpy-module-eldoc
  						 elpy-module-flymake
  						 elpy-module-pyvenv
  						 elpy-module-yasnippet
  						 elpy-module-django))
  	(elpy-enable)

  	(defun my/elpy-shell-kill ()
  	  "My elpy shell kill."
  	  (interactive)
  	  (elpy-shell-kill t))

  	(defun my/elpy-shell-kill-all ()
  	  "My elpa shell kill all."
  	  (interactive)
  	  (elpy-shell-kill-all t nil))

  	:config
	(setq elpy-shell-echo-input nil)

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

  	;; (my-install-python-virtualenv)
  	;; (pyvenv-workon my-python-virtualenv-workon-name)
  	)

  )

(provide 'my-python)

;;; my-python.el ends here
