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
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done)
        ("C-j" . ivy-immediate-done)
        ("C-RET" . ivy-immediate-done)
        ("C-g" . minibuffer-keyboard-quit)
        ("C-y" . ivy-yank-word)
        ("C-c C-a" . ivy-toggle-ignore)
        ("M-i" . ivy-toggle-case-fold)
        ("C-M-n" . ivy-next-line-and-call)
        ("C-M-p" . ivy-previous-line-and-call))
  :init
  (use-package flx :ensure t)
  (use-package ivy-rich
    :ensure t
    :config
    (setq ivy-virtual-abbreviate 'abbreviate
          ivy-rich-path-style 'abbrev)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    :init
    (add-hook 'ivy-mode-hook (lambda () (ivy-rich-mode ivy-mode))))

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
     ("C-c h C-f" . counsel-fonts)
     ("C-c h c" . counsel-colors-emacs)
     ("C-c h w" . counsel-colors-web)
     ("C-c h x" . counsel-linux-app)
     ("C-c h b" . counsel-switch-buffer)
     ("C-c h C-b" . counsel-switch-buffer-other-window)
     ("C-c h m" . my/woman)
     ("C-c h M" . counsel-major)
     ("C-c h C-m" . counsel-minor)
     ("C-c h o" . counsel-outline)
     ("C-c h j" . counsel-file-jump)
     ("C-c h l" . counsel-load-library)
     ("C-c h v" . counsel-set-variable)
     ("C-c h u" . counsel-unicode-char)
     ("M-X" . ivy-resume)
     ([remap jump-to-register] . counsel-register)
     ([remap describe-face] . counsel-describe-face)
     ([remap cd] . counsel-cd)
     ([remap locate] . counsel-locate)
     ([remap apropos] . counsel-apropos)
     ([remap yank-pop] . counsel-yank-pop)
     ([remap load-theme] . counsel-load-theme)
     ([remap org-capture] . counsel-org-capture))

    :init
    (counsel-mode +1)

    :config
    (setq counsel-mode-override-describe-bindings t)
    (setq counsel-find-file-at-point t)
    (setq counsel-preselect-current-file t)
    ;; (add-to-list 'counsel-find-file-extern-extensions "ts")
    ;; (setq ivy-initial-inputs-alist '((Man-completion-table . "^")
    ;;                                  (woman . "^")))
    )
  (ivy-mode +1);

  :config
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'fullpath
        ivy-count-format "(%d/%d) "
        ivy-magic-tilde nil
        ivy-dynamic-exhibit-delay-ms 150
        ivy-use-selectable-prompt t)
  (setq ivy-ignore-buffers '("\\*"))
  (setq-default projectile-completion-system 'ivy)

  (dolist (var '(ivy--regex-plus ivy--regex-fuzzy regexp-quote))
    (add-to-list 'ivy-re-builders-alist var))

  (defun my/woman (&optional topic re-cache)
    "Call woman in side window."
    (interactive)
    (let* ((b (current-buffer)))
      (woman topic re-cache)
      (display-buffer b 'display-buffer-in-side-window)))
  )

(diminish 'ivy-mode)
(diminish 'counsel-mode)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (when (bound-and-true-p ido-ubiquitous-mode)
;;               (ido-ubiquitous-mode -1)
;;               (ido-mode -1))
;;             (ivy-mode +1)
;;             (counsel-mode +1)))

(provide 'my-ivy)
;;; my-ivy.el ends here
