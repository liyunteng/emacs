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
  :defer t
  :bind
  (:map elpy-mode-map
        ("C-c C-d" . elpy-doc)
        ;; ("C-c C-j" . elpy-goto-definition)
        ;; ("C-c C-J" . elpy-goto-definition-other-window)
        ("C-c C-q" . my/elpy-shell-kill)
        ("C-c C-Q" . my/elpy-shell-kill-all)
        ("C-c C-k" . kill-region)
        ("C-c C-r" . revert-buffer)
        ([remap iedit-mode] . elpy-multiedit)
        ("C-c C-e" . elpy-refactor-map)
        )
  :init
  (use-package pyenv-mode
    :ensure t
    :commands (pyenv-mode)
    ;; :init
    ;; (advice-add 'elpy-enable :before '(lambda() (pyenv-mode t)))
    )
  ;; (advice-add 'python-mode :before 'elpy-enable)

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
        	           elpy-module-yasnippet))
  )

(use-package python
  :commands (python-mode run-python)
  :defer t
  :bind
  (("C-x t P" . run-python))
  :init
  (setq-default python-indent-guess-indent-offset nil)
  (setq-default python-indent-guess-indent-offset-verbose nil)
  (setq-default python-indent-offset 4)

  ;; (setq-default python-shell-interpreter "python3")

  ;; auto-insert
  (define-auto-insert 'python-mode
    (my-header '("#!/usr/bin/env python\n" "# -*- coding: utf-8 -*-\n\n")))

  ;; lsp
  (when  (executable-find "pyls")
    (add-hook 'python-mode-hook 'lsp-deferred))

  ;; company
  (my|enable-company python-mode '(elpy-company-backend))
  (my|enable-company inferior-python-mode '(elpy-company-backend))

  ;; jump
  (my|define-jump-backends python-mode elpy-goto-definition)

  (defun my-python-mode-hook ()
    "My python mode hook."
    (when semantic-mode
      (semantic-mode -1))
    (subword-mode +1)
    (set (make-local-variable 'tab-width) 4)
    (pyenv-mode +1)
    (elpy-enable))
  (add-hook 'python-mode-hook 'my-python-mode-hook)

  (defun my-python-shell-mode-hook ()
    "My python shell mode hook."
    (when semantic-mode
      (semantic-mode -1))

    ;; disable python-shell-copletion-native
    (python-shell-completion-native-turn-off)

    ;; replace indent-for-tab-command
    ;; disable python-shell-completion keybind
    (define-key inferior-python-mode-map (kbd "TAB") nil))
  (add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

  :config
  (when (executable-find "ipython")
    (progn (setq python-shell-interpreter "ipython")
           (if  (system-is-mac)
               (setq python-shell-interpreter-args "-c exec('__import__(\\'readline\\')') --no-confirm-exit --simple-prompt -i")
             (setq python-shell-interpreter-args "--no-confirm-exit --simple-prompt -i"))))
  )

(provide 'my-python)

;;; my-python.el ends here
