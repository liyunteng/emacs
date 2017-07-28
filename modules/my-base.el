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

;;出错后显示错误原因
(setq debug-on-error t)

;; 设置个人信息
(setq user-full-name "liyunteng")
;; 设置个人邮箱
(setq user-mail-address "li_yunteng@163.com")

;;========== backup =========
;; 不产生备份文件
(setq-default make-backup-files nil)
;;所有的备份文件转移到Backup目录下
(when make-backup-files
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "Backup/" my-savefile-dir)))
        version-control t
        kept-old-versions 2
        kept-new-versions 2
        delete-old-versions t
        backup-by-copying t))

;; 自动保存模式
;; (setq-default my-auto-save t)
(setq-default auto-save-default t)
(when auto-save-default
  (let ((autosave-dir (expand-file-name "auto-save/" my-savefile-dir)))
    (setq auto-save-list-file-prefix
          (concat autosave-dir "saves-"))
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,autosave-dir t)))
    (add-to-list 'auto-save-file-name-transforms
                 `(".*" ,autosave-dir t))))

;; 将保存的文件移动到.emacs.d/目录中
(setq-default recentf-save-file  (expand-file-name "recentf" user-emacs-directory))
(setq-default diary-file (expand-file-name "diary" my-savefile-dir))
(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects" my-savefile-dir))
(setq-default ido-save-directory-list-file (expand-file-name "ido.last" my-savefile-dir))
(setq-default projectile-known-projects-file (expand-file-name "projectile-bookmarks" my-savefile-dir))
(setq-default smex-save-file (expand-file-name "smex-items" my-savefile-dir))

;; abbrev
(setq-default abbrev-file-name (expand-file-name "abbrev_defs" my-savefile-dir))

;; undo-tree
(setq-default undo-tree-history-directory-alist
              `((".*" . ,(expand-file-name "undo-tree/" my-savefile-dir))))

;; tramp cache files
(setq-default tramp-auto-save-directory (expand-file-name "tramp" my-savefile-dir))

(provide 'my-base)
;;; my-base.el ends here
