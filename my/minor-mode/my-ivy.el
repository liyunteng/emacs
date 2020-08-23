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
  :ensure t
  :bind
  (:map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)
    ("C-g" . minibuffer-keyboard-quit)
    ("C-y" . ivy-yank-word)
    ("C-c C-a" . ivy-toggle-ignore)
    ("M-i" . ivy-toggle-case-fold)
    ("C-M-n" . ivy-next-line-and-call)
    ("C-M-p" . ivy-previous-line-and-call))
  :init
  (use-package flx :ensure t)
  (use-package swiper
    :ensure t
    :bind
    (("M-s u" . swiper-from-isearch)
      ("M-s S" . swiper-all-thing-at-point)
      ("M-s s" . swiper-thing-at-point)
      ("M-s m" . swiper)))

  (use-package smex
    :ensure t
    :config
    (smex-initialize))

  (use-package counsel
    :ensure t
    :diminish counsel-mode
    :bind
    (("C-c C-j" . counsel-imenu)
      ("C-c h r" . counsel-recentf)
      ("C-c h i" . counsel-semantic-or-imenu)
      ("C-c h f" . counsel-faces)
      ("C-c h F" . counsel-describe-face)
      ("C-c h c" . counsel-colors-emacs)
      ("C-c h w" . counsel-colors-web)
      ("C-c h x" . counsel-linux-app)
      ("C-c h b" . counsel-switch-buffer)
      ("C-c h C-b" . counsel-switch-buffer-other-window)
      ("C-c h m" . my/woman)
      ("C-c h M" . counsel-minor))
    :config
    (setq counsel-mode-override-describe-bindings t)
    (setq counsel-find-file-at-point t)
    (setq counsel-preselect-current-file t)
    ;; (add-to-list 'counsel-find-file-extern-extensions "ts")
    )

  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-ignore-buffers '("\\*"))
  (when (fboundp 'projectile-mode)
    (setq projectile-completion-system 'ivy))

  (dolist (var '(ivy--regex-plus ivy--regex-fuzzy regexp-quote))
    (add-to-list 'ivy-re-builders-alist var))

  ;; (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  ;; (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  ;; (define-key ivy-minibuffer-map (kbd "C-g") #'minibuffer-keyboard-quit)
  ;; (define-key ivy-minibuffer-map (kbd "C-y") #'ivy-yank-word)
  ;; (define-key ivy-minibuffer-map (kbd "C-c C-a") #'ivy-toggle-ignore)
  ;; (define-key ivy-minibuffer-map (kbd "M-i") #'ivy-toggle-case-fold)
  ;; (define-key ivy-minibuffer-map (kbd "C-M-n") #'ivy-next-line-and-call)
  ;; (define-key ivy-minibuffer-map (kbd "C-M-p") #'ivy-previous-line-and-call)


  (defun my/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (setq-default ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

  (defun my/woman (&optional topic re-cache)
    "Call woman in side window."
    (interactive)
    (let* ((b (current-buffer)))
      (woman topic re-cache)
      (display-buffer b 'display-buffer-in-side-window)))
  )




(diminish 'ivy-mode)
(diminish 'counsel-mode)

(add-hook 'after-init-hook
  (lambda ()
    (when (bound-and-true-p ido-ubiquitous-mode)
      (ido-ubiquitous-mode -1)
      (ido-mode -1))
    (ivy-mode 1)
    (counsel-mode)))

(provide 'my-ivy)
;;; my-ivy.el ends here
