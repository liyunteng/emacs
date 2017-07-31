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


;;========== backup =========
;; 不产生备份文件
(setq-default make-backup-files nil)
;;所有的备份文件转移到Backup目录下
(when make-backup-files
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "Backup/" my-cache-dir)))
        version-control t
        kept-old-versions 2
        kept-new-versions 2
        delete-old-versions t
        backup-by-copying t))

;; 自动保存模式
(setq-default auto-save-default t)
(when auto-save-default
  (let ((autosave-dir (expand-file-name "auto-save/" my-cache-dir)))
    (setq auto-save-list-file-prefix
          (concat autosave-dir "saves-"))
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,autosave-dir t)))
    (add-to-list 'auto-save-file-name-transforms
                 `(".*" ,autosave-dir t))))

;; 将保存的文件移动到.emacs.d/目录中
(setq-default recentf-save-file  (expand-file-name "recentf" user-emacs-directory))
(setq-default diary-file (expand-file-name "diary" my-cache-dir))
(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects" my-cache-dir))
(setq-default ido-save-directory-list-file (expand-file-name "ido.last" my-cache-dir))
(setq-default projectile-known-projects-file (expand-file-name "projectile-bookmarks" my-cache-dir))
(setq-default smex-save-file (expand-file-name "smex-items" my-cache-dir))

;; abbrev
(setq-default abbrev-file-name (expand-file-name "abbrev_defs" my-cache-dir))

;; undo-tree
(setq-default undo-tree-history-directory-alist
              `((".*" . ,(expand-file-name "undo-tree/" my-cache-dir))))

;; saveplace remembers your location in a file when saving files
(setq-default save-place-file (expand-file-name "saveplace" my-cache-dir))

;; savehist
(setq-default savehist-file (expand-file-name "savehist" my-cache-dir))

;; recentf
(setq-default recentf-save-file (expand-file-name "recentf" my-cache-dir))

;; bookmark
(setq-default bookmark-default-file (expand-file-name "bookmarks" my-cache-dir))

;; projectile
(setq-default projectile-cache-file (expand-file-name  "projectile.cache" my-cache-dir))

;; eshell

;; semantic
(setq-default semanticdb-default-save-directory
	      (expand-file-name "semanticdb" my-cache-dir))
;; tramp cache files
(setq-default tramp-auto-save-directory (expand-file-name "tramp" my-cache-dir))

(provide 'my-base)
;;; my-base.el ends here
