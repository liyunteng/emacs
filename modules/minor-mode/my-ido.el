;;; my-ido.el --- my ido                             -*- lexical-binding: t; -*-

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

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq-default ido-enable-flex-matching t)
(setq-default ido-use-filename-at-point nil)
(setq-default ido-auto-merge-work-directories-length -1)
(setq-default ido-use-virtual-buffers t)
(setq-default org-completion-use-ido t)
(setq-default magit-completing-read-function 'magit-ido-completing-read)

(setq-default ido-context-switch-command nil)
(setq-default ido-cur-item nil)
(setq-default ido-default-item nil)
(setq-default ido-cur-list nil)
(my-require-package 'ido-ubiquitous)
(require 'ido-ubiquitous)
(setq-default ido-ubiquitous-mode t)
(my-require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq-default ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

;;;ido中的按键
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-k")
              'ido-delete-file-at-head)
            (define-key ido-completion-map (kbd "C-w")
              'ido-delete-backward-word-updir)
            (define-key ido-completion-map (kbd "C-o")
              'ido-copy-current-file-name)
            (define-key ido-completion-map (kbd "C-f")
              'ido-magic-forward-char)
            (define-key ido-completion-map (kbd "C-l")
              'ido-toggle-case)
            (define-key ido-completion-map (kbd "C-p")
              'previous-history-element)
            (define-key ido-completion-map (kbd "C-n")
              'next-history-element)
            (define-key ido-completion-map (kbd "C-y")
              'ido-copy-current-word)
            (define-key ido-completion-map (kbd "C-t")
              'ido-toggle-regexp)
            ))

(provide 'my-ido)
;;; my-ido.el ends here
