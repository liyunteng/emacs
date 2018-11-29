;;; my-magit.el --- magit                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: vc

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

(when (eq window-system 'w32)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup)
	 ("C-x v =" . magit-diff-buffer-file)
	 :map magit-status-mode-map
	 ("M-RET" . magit-diff-visit-file-other-window))
  :config
  (use-package git-blamed
    :defer t
    :ensure t)
  (use-package gitconfig-mode
    :defer t
    :ensure t)
  (use-package gitignore-mode
    :defer t
    :ensure t)
  (use-package git-timemachine
    :defer t
    :ensure t)
  ;; (use-package git-messenger
  ;;   :defer t
  ;;   :ensure t)
  (use-package smeargle
    :ensure t
    :defer t)
  ;; (use-package yagist
  ;;   :disabled
  ;;   :ensure t
  ;;   :defer t)

  (use-package git-commit
    :ensure t
    :init
    (add-hook 'git-commit-mode-hook 'goto-address-mode))

  ;; (use-package bug-reference-github
  ;;   :ensure t
  ;;   :defer t
  ;;   :init
  ;;   (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

  :config
  ;; (setq magit-branch-read-upstream-first 'fallback)
  ;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  ;; (setq magit-diff-refine-hunk t)
  (after-load 'diff-hl
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (after-load 'compile
    (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
			'(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
      (add-to-list 'compilation-error-regexp-alist-alist defn)
      (add-to-list 'compilation-error-regexp-alist (car defn))))
  ;; (add-hook 'magit-popup-mode-hook 'my/toggle-show-trailing-whitespace-off)
  )

;; 激活magit-log，可在magit-log中操作magit
;; (global-set-key (kbd "C-x v g") 'magit-log)

;; fullscreen
;; (setq-default magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

(provide 'my-magit)
;;; my-magit.el ends here
