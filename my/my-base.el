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
;; (thingatpt help-fns radix-tree help-mode easymenu cl-loaddefs cl-lib elec-pair time-date mule-util tooltip eldoc electric uniquify ediff-hook vc-hooks lisp-float-type mwheel term/x-win x-win term/common-win x-dnd tool-bar dnd fontset image regexp-opt fringe tabulated-list replace newcomment text-mode elisp-mode lisp-mode prog-mode register page menu-bar rfn-eshadow isearch timer select scroll-bar mouse jit-lock font-lock syntax facemenu font-core term/tty-colors frame cl-generic cham georgian utf-8-lang misc-lang vietnamese tibetan thai tai-viet lao korean japanese eucjp-ms cp51932 hebrew greek romanian slovak czech european ethiopic indian cyrillic chinese composite charscript charprop case-table epa-hook jka-cmpr-hook help simple abbrev obarray minibuffer cl-preloaded nadvice loaddefs button faces cus-face macroexp files text-properties overlay sha1 md5 base64 format env code-pages mule custom widget hashtable-print-readable backquote dbusbind inotify lcms2 dynamic-setting system-font-setting font-render-setting move-toolbar gtk x-toolkit x multi-tty make-network-process emacs)


;; Always load newest byte code
(setq load-prefer-newer t)

;; adjust grabage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; 递归minibuffer
(setq enable-recursive-minibuffers t)

;; resize mini-window to fit the text displayed in them
(setq resize-mini-windows nil)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; delete duplicates minibuffer history
(setq history-delete-duplicates t)

;; message max
(setq message-log-max 20000)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(when (system-is-mswindows)
  (setq delete-by-moving-to-trash t))

;; draw underline lower
(setq x-underline-at-descent-line t)

;; dont't trancate
(setq truncate-partial-width-windows nil)
;; don't truncate line
(setq-default truncate-lines nil)

;; smooth scrolling
(setq scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; fill-column 80
;; (setq-default fill-column 80)

;; search case fold
(setq-default case-fold-search t)

;; tab width
(setq-default tab-width 4)

;; don't use tab
(setq-default indent-tabs-mode nil)

;; Show a marker in the left fringe for lines
(setq-default indicate-empty-lines t)

;; 行距
(setq-default line-spacing 0.0)



;; disable ad redefinition warning
(setq ad-redefinition-action 'accept)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; recenter
(setq recenter-positions '(top middle bottom))

(setq adaptive-fill-regexp
      "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+\\|\\([0-9]+\\.\\)[ \t]*\\)*")

;; initial mode
;; (setq-default initial-major-mode
;;               'lisp-interaction-mode)
;; initial scarch message
(add-hook 'after-init-hook
          (lambda ()
            (setq initial-scratch-message
                  (concat ";; Happy Hacking, " user-login-name
                          (if user-full-name (concat " ("user-full-name ")"))
                          " - Emacs ♥ you!\n\n"))))
;; conflic with desktop
;; (setq initial-buffer-choice t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-buffer-menu nil)


;; warn when opening files bigger than 10MB
(setq large-file-warning-threshold 10000000)
;; add final newline
(setq-default require-final-newline t)
;; (setq revert-without-query '(".*"))
(setq kill-emacs-query-functions nil)
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes t)

;; 不产生备份文件
(setq make-backup-files nil)
(setq backup-directory-alist `((".*" . ,(expand-file-name "Backup/" my-cache-dir))))
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


(provide 'my-base)
;;; my-base.el ends here
