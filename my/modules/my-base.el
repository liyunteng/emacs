;;; my-base.el --- base                              -*- lexical-binding: t; -*-

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
;; locale

(defun my-utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (or (string-match "UTF-8" v)
			 (string-match "utf8" v))))

(defun my-locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (my-utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (my-utf8-locale-p (getenv "LC_ALL"))
      (my-utf8-locale-p (getenv "LC_CTYPE"))
      (my-utf8-locale-p (getenv "LANG"))))

(when (or window-system (my-locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'chinese-gb18030)
  (prefer-coding-system 'chinese-gbk)
  (prefer-coding-system 'utf-8)
  )



;; disable ad redefinition warning
(setq ad-redefinition-action 'accept)

;; toggle off debug-on-error
(if my-debug
    (setq debug-on-error t)
  (setq debug-on-error nil))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

(use-package auto-compile
  :ensure t
  :commands (auto-compile-on-save-mode
			 auto-compile-on-load-mode)
  :init
  (auto-compile-on-save-mode +1)
  (auto-compile-on-load-mode +1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))

  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  )

(use-package smart-mode-line
  :ensure t
  :commands (smart-mode-line-enable sml/setup)
  :init
  (add-hook 'after-init-hook #'sml/setup)
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  ;; (sml/setup)
  )


(use-package beacon
  :ensure t
  :commands (beacon-mode)
  :diminish beacon-mode
  :init
  (beacon-mode +1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode +1))


;;========== backup =========
;; 不产生备份文件
(setq-default make-backup-files nil)
(setq-default backup-directory-alist `((".*" . ,(expand-file-name "Backup/" my-cache-dir))))
;;所有的备份文件转移到Backup目录下
(when make-backup-files
  (setq save-silently t
		version-control t
		kept-old-versions 2
		kept-new-versions 2
		delete-old-versions t
		backup-by-copying t))

;; replaced by super-save
(setq-default auto-save-default t)
(setq-default auto-save-list-file-prefix
			  (cond ((eq system-type 'ms-dos)
					 ;; MS-DOS cannot have initial dot, and allows only 8.3 names
					 (concat (expand-file-name "auto-save/" my-cache-dir) "_saves-") )
					(t
					 (concat (expand-file-name "auto-save/" my-cache-dir) ".saves-"))))


;; 将保存的文件移动到.emacs.d/目录中
(setq-default diary-file (expand-file-name "diary" my-cache-dir))

(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects" my-cache-dir))
(setq-default smex-save-file (expand-file-name "smex-items" my-cache-dir))

;; undo-tree
(setq-default undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo-tree/" my-cache-dir))))

;; saveplace remembers your location in a file when saving files
(setq-default save-place-file (expand-file-name "saveplace" my-cache-dir))

;; savehist
(setq-default savehist-file (expand-file-name "savehist" my-cache-dir))

;; recentf
(setq-default recentf-save-file (expand-file-name "recentf" my-cache-dir))

;; projectile
(setq-default projectile-cache-file (expand-file-name  "projectile.cache" my-cache-dir))
(setq-default projectile-known-projects-file (expand-file-name "projectile-bookmarks" my-cache-dir))

;; eshell
(setq-default eshell-directory-name (expand-file-name "eshell" my-cache-dir))

;; semantic
(setq-default semanticdb-default-save-directory (expand-file-name "semanticdb" my-cache-dir))
;; tramp cache files
(setq-default tramp-auto-save-directory (expand-file-name "tramp" my-cache-dir))
(setq-default tramp-persistency-file-name (expand-file-name "tramp/tramp" my-cache-dir))

(provide 'my-base)
;;; my-base.el ends here
