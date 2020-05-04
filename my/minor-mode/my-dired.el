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
  :commands (dired dired-jump dired-jump-other-window)
  :bind (("C-x d" . dired)
	     ("C-x M-j" . dired-jump-other-window)
         ("C-x C-j" . dired-jump)
         ("M-s f" . find-name-dired))

  :config
  (setq dired-dwim-target t
        dired-recursive-deletes 'top
        dired-recursive-copies 'top)

  (pcase system-type
    ('darwin (setq dired-listing-switches "-l -G"))
    ('gnu/linux (setq dired-listing-switches "--group-directories-first -alhq"))
    ('windows-nt (setq dired-listing-switches "-l"))
    )

  ;; goto parent dir
  (defvar-local my--subdir-parent nil)
  (defadvice dired-maybe-insert-subdir (around dirname (&optional switches no-error-if-not-dir-p) activate)
    (progn (if (ad-get-arg 0)
  	           (setq my--subdir-parent (ad-get-arg 0)))
  	       ad-do-it))

  (defadvice dired-kill-subdir (around back-to-parent-dir activate)
    (progn
      ad-do-it
      (if my--subdir-parent
  	      (progn
  	        (dired-goto-file my--subdir-parent)
  	        (setq my--subdir-parent nil)))))

  (defun my/dired-view-file-other-window ()
    "In Dired, view this file or directory in another window."
    (interactive)
    (view-file-other-window (dired-get-file-for-visit)))

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

  (define-key dired-mode-map (kbd "M-s a") nil)
  (define-key dired-mode-map (kbd "M-s f") 'find-name-dired)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "M-<return>") 'dired-do-find-marked-files)
  (define-key dired-mode-map (kbd "C-M-<return>") 'diredp-do-find-marked-files-recursive)
  (define-key dired-mode-map (kbd "e") 'my/dired-view-file-other-window)
  (define-key dired-mode-map (kbd "c") 'dired-kill-subdir)
  (define-key dired-mode-map (kbd "TAB") 'dired-hide-all)
  (define-key dired-mode-map (kbd "C-M-f") 'find-grep-dired)
  (define-key dired-mode-map (kbd "\\") 'my/dired-run-git-command)
  (when (fboundp 'crux-open-with)
    (define-key dired-mode-map (kbd "M-o") 'crux-open-with))
  ;; (define-key dired-mode-map (kbd "=") 'dired-compare-directories)
  ;; rename filename in dired-mode
  (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode))

(use-package dired-filter
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'dired-filter-group-mode)

  :config
  (set-face-attribute 'dired-filter-group-header  nil
                      :inherit 'isearch)
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Directories" (directory))
           ("PDF"
            (extension . "pdf"))
           ("LaTeX"
            (extension "tex" "bib"))
           ("Org"
            (extension . "org"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Multimedia"
            (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3" "mkv" "webm"))))))

(use-package dired-x
  :after dired
  :config
  (setq dired-omit-verbose nil
	    ;; dired忽略的上限
	    dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*")
  (dolist (ex '(".cache" ".o" ".ui"))
    (add-to-list 'dired-omit-extensions ex))
  (add-hook 'dired-mode-hook 'dired-omit-mode))

(use-package dired-git-info
  :defines (dgi-commit-message-format)
  :after dired
  ;; :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode))
  :config
  (setq dgi-commit-message-format "%h\t%an\t %cr\t %s"))

(use-package dired-rsync
  :after dired
  :ensure t
  :bind (:map dired-mode-map
              ("r" . dired-rsync)))

(use-package dired-quick-sort
  :ensure t
  :bind (:map dired-mode-map
              ("s" . hydra-dired-quick-sort/body)))

(use-package dired-filetype-face
  :after dired
  :ensure t
  :config
  (deffiletype-face "code" "#9FC59F" "code")
  (deffiletype-face-regexp code
    :type-for-docstring "code"
    :extensions
    '("a" "ahk" "asm" "C" "c" "cc" "cpp" "cs" "css"
	  "ddl" "el" "erl" "go" "h" "hrl" "JAVA" "java" "m"
	  "mm" "lisp" "livecode" "lua" "p" "pas" "php" "pl"
	  "py" "rb" "rev" "sch" "scheme" "scm" "sql" "st"))
  (deffiletype-setup "code" "code")

  (deffiletype-face-regexp my-video
    :type-for-docstring "video"
    :extensions
    '("ts" "h264" "h265" "m3u8"))
  (deffiletype-face "my-video" "SandyBrown")
  (deffiletype-setup "my-video" "my-video")
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


(unless (version< emacs-version "26")
  (use-package treemacs
    :ensure t
    :bind (:map global-map
                ([f8]        . treemacs))
    :config
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-width                      35
          treemacs-collapse-dirs              3)

    (setq treemacs-persist-file
          (expand-file-name "treemacs/treemacs-persist" my-cache-dir)
          treemacs-last-error-persist-file
          (expand-file-name "treemacs/treemacs-persist-at-last-error" my-cache-dir))

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t))

  (use-package treemacs-projectile
    :ensure t
    :after treemacs projectile)

  (use-package treemacs-magit
    :after treemacs magit
    :ensure t)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config
    ;; (treemacs-icons-dired-mode +1)
    :hook
    (dired-mode . treemacs-icons-dired--enable-highlight-correction)
    (dired-mode . treemacs--select-icon-set )
    (dired-mode . treemacs-icons-dired-mode)
    )

  )

(provide 'my-dired)

;;; my-dired.el ends here
