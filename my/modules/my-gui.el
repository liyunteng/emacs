;;; my-gui.el --- gui                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

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

;; 使用系统字体
(setq-default font-use-system-font t)

;; 中文使用wqy-microhei,其他使用DejaVu Sans Mono
(when (display-graphic-p)
  (create-fontset-from-fontset-spec
   "-*-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-my,
 chinese-gbk: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1,
 chinese-iso-8bit: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1,
 chinese-big5: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"))
(setq default-frame-alist (append '((font . "fontset-my")) default-frame-alist))
(set-frame-font "fontset-my")

(defvar my--after-display-system-init-alist '())
(defadvice server-create-window-system-frame
    (after my-init-display activate)
  "After Emacs server create a frame, run functions queued in \
`MY--AFTER-DISPLAY-SYSTEM-INIT-ALIST'.
to do any setup that needs to have the display system initialized."
  (progn
    (dolist (fn (reverse my--after-display-system-init-alist))
      (funcall fn))
    (ad-disable-advice 'server-create-window-system-frame
                       'after
                       'my-init-display)
    (ad-activate 'server-create-window-system-frame)))

(defmacro my|do-after-display-system-init (&rest body)
  "If the display-system is initialized, run `BODY'.
Otherwise,add it to a queue of actions to perform after the first graphical frame is created."
  `(let ((init (cond ((boundp 'ns-initialized) ns-initialized)
                     ((boundp 'w32-initialized) (font-family-list))
                     ((boundp 'x-initialized) x-initialized)
                     (t (display-graphic-p)))))
     (if init
         (progn
           ,@body)
       (push (lambda () ,@body) my--after-display-system-init-alist))))

(unless (display-graphic-p)
  (my|do-after-display-system-init
   (my/echo "display-graphic-p is nil")))

(defun my--removes-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  ;; removes the GUI elements
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; tooltips in echo-aera
  (tooltip-mode -1)
  ;; buffer menu
  (setq buffers-menu-max-size 10)
  (menu-bar-mode -1)

  ;; suppress GUI features
  (setq use-dialog-box nil)
  (setq use-file-dialog nil)
  (setq x-gtk-use-old-file-dialog nil)
  (setq x-gtk-file-dialog-help-text nil)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message t))
(my--removes-gui-elements)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun my--console-frame-setup ()
  "Mouse in a terminal (Use shift to paste with middle button)."
  (xterm-mouse-mode 1)
  (when (fboundp 'mwheel-install)
    (mwheel-install)))

(use-package speedbar
  :commands (speedbar)
  :bind (("<f2>" . speedbar))
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-tag-hierarchy-method
	'(speedbar-prefix-group-tag-hierarchy))

  (speedbar-add-supported-extension ".go")
  (add-hook 'speedbar-mode-hook
	    (lambda ()
	      (auto-raise-mode t)
	      (setq dframe-update-speed 1)
	      ;; (add-to-list 'speedbar-frame-parameters '(top . 0))
	      ;; (add-to-list 'speedbar-frame-parameters '(left . 0))
	      )))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<f11>") 'toggle-frame-maximized)
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(provide 'my-gui)
;;; my-gui.el ends here
