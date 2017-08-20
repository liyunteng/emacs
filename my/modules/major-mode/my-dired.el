;;; my-dired.el --- dired

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

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package dired
  :config
  (use-package dired+
	:ensure t
	:config
	(setq diredp-hide-details-initially-flag nil
		  diredp-hide-details-propagate-flag nil
		  dired-hide-details-mode nil
		  global-dired-hide-details-mode nil
		  )
	;; 重用buffer，避免产生过多的dired buffer
	(defun my--turn-on-diredp-find-reuse-dir ()
	  (toggle-diredp-find-file-reuse-dir t))
	(add-hook 'dired-mode-hook 'my--turn-on-diredp-find-reuse-dir)
	)

  (use-package dired-aux
	:config
	(setq dired-isearch-filenames 'dwim)
	)

  (use-package dired-x
	:config
	(setq dired-omit-verbose nil
		  ;; dired忽略的上限
		  dired-omit-mode t
		  dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*"
		  )
	(dolist (ex '(".cache" ".o" ".ui"))
	  (add-to-list 'dired-omit-extensions ex))
	)

  (use-package dired-sort
	:ensure t)

  (use-package diff-hl
	:ensure t
	:defines diff-hl-dired-mode
	:config
	(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
	)

  (setq
   dired-dwim-target t
   dired-recursive-deletes 'top
   dired-recursive-copies 'top)
  ;;传给ls的参数
  (if (or (eq system-type 'linux)
		  (eq system-type 'gnu/linux))
	  (setq dired-listing-switches "-alhcDF")
	(setq dired-listing-switches "-alh"))

  ;; goto parent dir
  (defadvice dired-kill-subdir (around back-to-parent-dir activate)
	(let ((dirname (car (diredp-this-subdir))))
	  ad-do-it
	  (if dirname
		  (dired-goto-file  dirname))))

  (defun my/dired-view-file-other-window ()
    "In Dired, view this file or directory in another window."
    (interactive)
    (view-file-other-window (dired-get-file-for-visit))
    )

  (defun my/dired-run-git-command (command &optional arg file-list)
    "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode."
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list
        ;; Want to give feedback whether this file or marked files are used:
        (dired-read-shell-command "git command on %s: " current-prefix-arg files)
        current-prefix-arg
        files)))
    (unless (string-match "[*?][ \t]*\\'" command)
      (setq command (concat command " *")))
    (setq command (concat "git " command))
    (dired-do-shell-command command arg file-list)
    (message command))

  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "M-<return>") 'dired-do-find-marked-files)
  (define-key dired-mode-map (kbd "C-M-<return>") 'diredp-do-find-marked-files-recursive)
  (define-key dired-mode-map (kbd "e") 'my/dired-view-file-other-window)
  (define-key dired-mode-map (kbd "c") 'dired-kill-subdir)
  (define-key dired-mode-map (kbd "TAB") 'dired-hide-all)
  (define-key dired-mode-map (kbd "s") nil)
  (define-key dired-mode-map (kbd "s r") 'dired-sort-toggle-or-edit)
  (define-key dired-mode-map (kbd "s s") 'dired-sort-size)
  (define-key dired-mode-map (kbd "s n") 'dired-sort-name)
  (define-key dired-mode-map (kbd "s e") 'dired-sort-extension)
  (define-key dired-mode-map (kbd "s u") 'dired-sort-utime)
  (define-key dired-mode-map (kbd "s c") 'dired-sort-ctime)
  (define-key dired-mode-map (kbd "s t") 'dired-sort-time)
  (define-key dired-mode-map (kbd "C-M-f") 'find-grep)
  (define-key dired-mode-map (kbd "C-M-S-f") 'find-grep-dired)
  (define-key dired-mode-map (kbd "\\") 'my/dired-run-git-command)
  ;; (define-key dired-mode-map (kbd "=") 'dired-compare-directories)

  )


;; (setq dired-ls-sorting-switches "SXU")


;;; 使用！来使用外部程序打开
;; (setq dired-guess-shell-alist-user
;;       '(("\\.avi\\'" "totem &")
;;         ("\\.rmvb\\'" "totem &")
;;         ("\\.mkv\\'" "totem &")
;;         ("\\.mp4\\'" "totem &")
;;         ("\\.htm\\'" "firefox &")
;;         ("\\.html\\'" "firefox &")
;;         ("\\.pdf\\'" "evince &")
;;         ("\\.chm\\'" "xchm &")))

(provide 'my-dired)
;;; my-dired.el ends here
