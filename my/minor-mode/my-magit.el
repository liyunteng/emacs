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
	     ("C-x M-g" . magit-dispatch)
	     ("C-x v =" . magit-diff-buffer-file)
	     :map magit-status-mode-map
	     ("M-RET" . magit-diff-visit-file-other-window)
         :map magit-mode-map
         ("C-o" . magit-open-repo))
  :init
  (setq-default magit-diff-refine-hunk t)

  :config
  ;; (setq magit-branch-read-upstream-first 'fallback)
  ;; (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:
  ;; "))
  (with-eval-after-load 'diff-hl
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (with-eval-after-load 'compile
    (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
			            '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
      (add-to-list 'compilation-error-regexp-alist-alist defn)
      (add-to-list 'compilation-error-regexp-alist (car defn))))
  ;; (add-hook 'magit-popup-mode-hook 'my/toggle-show-trailing-whitespace-off)

  ;; Ignore recent commit
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpulled-from-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpushed-to-upstream
          magit-insert-unpushed-to-pushremote))

  ;; Opening repo externally
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url)))))

(use-package git-blamed
  :ensure t)

(use-package magit-todos
  :ensure t)

(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine-toggle))
  :commands (git-timemachine git-timemachine-toggle git-timemachine-switch-branch)
  :ensure t)

(use-package gitconfig-mode
  :ensure t
  :mode
  ("/\.gitconfig\'" . gitconfig-mode)
  ("/vcs/gitconfig\'" . gitconfig-mode))

(use-package gitignore-mode
  :ensure t)

(use-package git-commit
  :ensure t
  :init
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(use-package gitattributes-mode
  :ensure t)

(use-package git-msg-prefix
  :ensure t
  :config
  (setq git-msg-prefix-log-flags " --since='1 week ago' "
        git-msg-prefix-regex "^\\([^:]*: \\)"
        git-msg-prefix-input-method 'ivy-read)
  ;; (add-hook 'git-commit-mode-hook 'git-msg-prefix)
  )


(use-package yagist
  :ensure t)
(use-package bug-reference-github
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))
;; (use-package github-clone :ensure t)
;; (use-package forge :ensure t)
;; (use-package github-review :ensure t)

(provide 'my-magit)
;;; my-magit.el ends here
