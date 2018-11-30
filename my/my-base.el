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

;; Always load newest byte code
(setq load-prefer-newer t)

;; adjust grabage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;;
(setq mouse-yank-at-point t)

;; 支持emacs和外部程序的拷贝粘贴
(setq select-enable-clipboard t)

;; 递归minibuffer
(setq enable-recursive-minibuffers t)

;; resize mini-window to fit the text displayed in them
(setq resize-mini-windows nil)

;; suggest key bindings
(setq suggest-key-bindings t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; message max
(setq message-log-max 20000)

;;设置删除记录
(setq kill-ring-max 200)

;; 行距
(setq line-spacing 0.0)

(setq adaptive-fill-regexp
      "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+\\|\\([0-9]+\\.\\)[ \t]*\\)*")
;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)
(setq-default sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(when (system-is-mswindows)
  (setq delete-by-moving-to-trash t))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; draw underline lower
(setq x-underline-at-descent-line t)

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq set-mark-command-repeat-pop t)

;; Keep focus while navigating help buffers
(setq help-window-select 'nil)

;; smooth scrolling
(setq scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; initial mode
;; (setq-default initial-major-mode
;;               'lisp-interaction-mode)
;; initial scarch message
(setq initial-scratch-message
      (concat ";; Happy Hacking, "
              user-login-name
              (if user-full-name
                  (concat " ("user-full-name ")"))
              " - Emacs ♥ you!\n\n"))

;; 锁定minibuffer行高
(setq resize-mini-windows nil)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(and (boundp 'battery-status-function)
     (display-battery-mode t))

;; Show column number in mode line
(column-number-mode +1)
(line-number-mode +1)
;; show file size in mode line
(size-indication-mode +1)
;; show which function in mode line
(which-function-mode +1)

;; mark visible
(transient-mark-mode +1)

;; replace active region
(delete-selection-mode +1)

;; 光标靠近鼠标指针时，鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;; margin width
(fringe-mode  '(8 . 0))

;; show image file
(auto-image-file-mode t)
;; (when (executable-find "convert")
;;   (setq imagemagick-render-type 1))

;; fill-column 80
;; (setq-default fill-column 80)

;; imenu
(setq-default imenu-auto-rescan t)

;; Don't try to ping things that look like domain names
(setq-default ffap-machine-p-known 'reject)

(setq-default case-fold-search t)

(setq-default tooltip-delay 1.5)

;; tab width
(setq-default tab-width 4)

;; don't use tab
(setq-default indent-tabs-mode nil)

;; add final newline
(setq-default require-final-newline t)

;; recenter
;; (setq-default recenter-positions '(top middle bottom))


;;========== backup =========
;; 不产生备份文件
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,(expand-file-name "Backup/" my-cache-dir))))
;;所有的备份文件转移到Backup目录下
(when make-backup-files
  (setq save-silently t
	    version-control nil
	    kept-old-versions 2
	    kept-new-versions 2
	    delete-old-versions t
        create-lockfiles nil
	    backup-by-copying t))


;; replaced by super-save
(setq auto-save-default t)
(setq auto-save-list-file-prefix
      (cond ((eq system-type 'ms-dos)
	         ;; MS-DOS cannot have initial dot, and allows only 8.3 names
	         (concat (expand-file-name "auto-save/" my-cache-dir) "_saves-") )
            (t
	         (concat (expand-file-name "auto-save/" my-cache-dir) ".saves-"))))


;; 将保存的文件移动到.emacs.d/目录中
(setq-default diary-file (expand-file-name "diary" my-cache-dir))

(setq-default ede-project-placeholder-cache-file (expand-file-name "ede-projects" my-cache-dir))
(setq-default smex-save-file (expand-file-name "smex-items" my-cache-dir))

;; desktop
(setq-default desktop-path (list my-cache-dir))
(setq-default desktop-dirname my-cache-dir)
(provide 'my-base)
;;; my-base.el ends here
