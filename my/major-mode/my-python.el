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

(use-package elpy
  :ensure t
  :after python
  :commands (elpy-mode elpy-enable)
  :bind
  (:map elpy-mode-map
        ("C-c C-d" . elpy-doc)
        ;; ("C-c C-j" . elpy-goto-definition)
        ;; ("C-c C-J" . elpy-goto-definition-other-window)
        ("C-c C-q" . my/elpy-shell-kill)
        ("C-c C-Q" . my/elpy-shell-kill-all)
        ("C-c C-k" . kill-region)
        )
  :init
  (defvar my-python-virtualenv-dir (expand-file-name ".virtualenvs" "~/"))
  (defvar my-python-virtualenv-workon-name "default")
  (defvar my-python-virtualenv-workon-dir
    (expand-file-name my-python-virtualenv-workon-name my-python-virtualenv-dir))

  (unless (file-exists-p my-python-virtualenv-dir)
    (make-directory my-python-virtualenv-dir))
  (when (file-exists-p my-python-virtualenv-dir)
    (setenv "WORKON_HOME" my-python-virtualenv-dir))

  (setq elpy-rpc-virtualenv-path my-python-virtualenv-workon-dir)
  ;; (elpy-rpc--get-package-list)
  (defvar my-python-elpy-dependency '("jedi" "flake8" "autopep8" "yapf" "rope"))

  (when (executable-find "ipython")
    (progn (setq python-shell-interpreter "ipython"
    	         python-shell-interpreter-args "--simple-prompt --no-confirm-exit -i")
           (add-to-list 'my-python-elpy-dependency "ipython")))

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
  (setq elpy-modules '(elpy-module-sane-defaults
    		           elpy-module-eldoc
    		           elpy-module-flymake
    		           elpy-module-pyvenv
    		           elpy-module-yasnippet
    		           elpy-module-django))
  (defun my/python-install-lib (lib)
    (interactive "s\lib: ")
    (let ((install-cmd (or (executable-find "pip")
                           (executable-find "easy_install")))
          (async-shell-command-buffer 'new-buffer))
      (if install-cmd
          (progn
            (message "%s installing %s ..." install-cmd lib)
            (async-shell-command (format "%s install %s" install-cmd lib)
                                 (get-buffer-create (format "*%s-install-%s*" (file-name-base install-cmd) lib))))
        (message "pip/easy_install not found, please install pip/easy_install")
        )))

  (defun my--python-install-libs (libs)
    (mapc (lambda (n)
            (my/python-install-lib n))
          libs))

  (defun my-install-python-virtualenv ()
    "My install python virtualenv."
    (if (or (not (file-exists-p my-python-virtualenv-dir))
            (not (file-exists-p my-python-virtualenv-workon-dir)))
        (progn
          (let ((virtualenvbin (executable-find "virtualenv"))
                (async-shell-command-buffer 'new-buffer))
            (if (null virtualenvbin)
    	        (message "virtualenv not found, please install virtualenv")
    	      (message "%s %s ..." virtualenvbin my-python-virtualenv-workon-dir)
    	      (shell-command (format "%s %s" virtualenvbin my-python-virtualenv-workon-dir)
                             (get-buffer-create "*install-virtualenv-output*")
                             nil)

    	      (setenv "WORKON_HOME" my-python-virtualenv-dir)
    	      (pyvenv-workon my-python-virtualenv-workon-name)
              (my--python-install-libs my-python-elpy-dependency)
    	      (elpy-rpc-restart)
    	      (message "Done"))))))

  (my-install-python-virtualenv)
  (pyvenv-workon my-python-virtualenv-workon-name)
  )

(use-package python
  :commands (python-mode run-python)
  :defer t
  :bind
  (("C-x t P" . run-python))
  :init
  (setq-default python-indent-guess-indent-offset-verbose nil)
  (setq-default python-indent-offset 4)

  ;; (defvar my-python-original-buffer nil)
  ;;   (defvar my-python-switch-function 'switch-to-buffer-other-window)
  ;;   (defun my/python-switch-back ()
  ;;     "My from inferior-python-mode switch back to python file."
  ;;     (interactive)
  ;;     (if my-python-original-buffer
  ;;         (funcall my-python-switch-function my-python-original-buffer)
  ;;       (user-error "Node orignal buffer.")))
  ;;   (defun my/run-python (&optional noswitch)
  ;;     "My run python."
  ;;     (interactive)
  ;;     (remove-hook 'python-shell-first-prompt-hook 'python-shell-completion-native-turn-on-maybe-with-msg)
  ;;     (add-hook 'python-shell-first-prompt-hook 'python-shell-completion-native-turn-on)
  ;;     (get-buffer-process
  ;;      (python-shell-make-comint (python-shell-calculate-command)
  ;;                                (python-shell-get-process-name nil) t))
  ;;     (unless noswitch
  ;;       (pop-to-buffer  "*Python*")))
  ;;   (defun my/python-switch-to-shell ()
  ;;     "My from python file switch to inferior-python-mode shell.
  ;; If *Python* buffer not exists, create it."
  ;;     (interactive)
  ;;     (let ((origin-buffer (current-buffer)))
  ;;       (if (get-buffer "*Python*")
  ;;           (funcall my-python-switch-function "*Python*")
  ;;         (my/run-python))
  ;;       (setq-local my-python-original-buffer origin-buffer)))

  (defun my-python-mode-hook ()
    "My python mode hook."
    (when semantic-mode
      (semantic-mode -1))
    (subword-mode +1)
    (set (make-local-variable 'tab-width) 4)

    (elpy-enable)
    ;; (when (fboundp #'python-imenu-create-index)
    ;;   (setq-local imenu-create-index-function
	;; 	          #'python-imenu-create-index))

    ;; (unless (get-buffer "*Python*")
    ;;   (my/run-python t))
    ;; (define-key python-mode-map (kbd "C-c C-z") 'my/python-switch-to-shell)
    )
  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defun my-python-shell-mode-hook ()
    "My python shell mode hook."
    (when semantic-mode
      (semantic-mode -1))
    ;; disable python-shell-copletion-native
    (python-shell-completion-native-turn-off)
    ;; replace indent-for-tab-command
    ;; disable python-shell-completion keybind
    (define-key inferior-python-mode-map (kbd "TAB") nil)
    ;; (define-key inferior-python-mode-map [remap indent-for-tab-command] 'complete-symbol)
    ;; (define-key inferior-python-mode-map (kbd "C-c C-z") 'my/python-switch-back)
    )
  (add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

  :config
  (when (executable-find "ipython")
    (progn (setq python-shell-interpreter "ipython"
    	         python-shell-interpreter-args "--simple-prompt --no-confirm-exit -i"))
    (setq python-shell-completion-native-disabled-interpreters nil)
    ;; for python shell completion
    ;; (remove-hook 'python-shell-first-prompt-hook 'python-shell-completion-native-turn-on-maybe-with-msg)
    ;; (add-hook 'python-shell-first-prompt-hook 'python-shell-completion-native-turn-on)
    )
  (remove-hook 'python-shell-first-prompt-hook 'python-shell-completion-native-turn-on-maybe-with-msg)
  )




(provide 'my-python)

;;; my-python.el ends here
