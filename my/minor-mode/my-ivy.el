;;; my-ivy.el --- my ivy                             -*- lexical-binding: t; -*-

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

(use-package ivy
  :ensure t)
(use-package flx
  :ensure t)
(use-package counsel
  :ensure t)

(diminish 'ivy-mode)

(diminish 'counsel-mode)
(setq-default ivy-use-virtual-buffers t
              ivy-count-format ""
              projectile-completion-system 'ivy
              ivy-initial-inputs-alist
              '((counsel-M-x . "^")
                (man . "^")
                (woman . "^")))

;; IDO-style directory navigation
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(defun my/enable-ivy-flx-matching ()
  "Make `ivy' matching work more like IDO."
  (interactive)
  (setq-default ivy-re-builders-alist
                '((t . ivy--regex-fuzzy))))


(setq-default counsel-mode-override-describe-bindings t)
(add-hook 'after-init-hook
          (lambda ()
            (when (bound-and-true-p ido-ubiquitous-mode)
              (ido-ubiquitous-mode -1)
              (ido-mode -1))
            (ivy-mode 1)
            (counsel-mode)))

;;(when (maybe-require-package 'swiper)
;;  (after-load 'ivy
;;    (define-key ivy-mode-map (kbd "C-s") 'swiper)))

(dolist (var '(ivy--regex-plus ivy--regex-fuzzy regexp-quote))
  (add-to-list 'ivy-re-builders-alist var))
;; (setq-default ivy-initial-inputs-alist '((man . "^") (woman . "^")))
;; (setq-default ivy-use-virtual-buffers nil)
(setq-default ivy-ignore-buffers '("\\*"))
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "C-g") #'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map (kbd "C-y") #'ivy-yank-word)
(define-key ivy-minibuffer-map (kbd "C-c C-a") #'ivy-toggle-ignore)
(define-key ivy-minibuffer-map (kbd "M-i") #'ivy-toggle-case-fold)

(provide 'my-ivy)
;;; my-ivy.el ends here
