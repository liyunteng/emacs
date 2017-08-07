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
;; (if (eq system-type 'linux)
;;     (custom-set-faces
;;      `(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant
;;      normal :weight normal :height 113 :width normal))))))

(my-require-package 'smart-mode-line)
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(setq sml/theme nil)
(add-hook 'after-init-hook #'sml/setup)

(my-require-package 'beacon)
(require 'beacon)
(beacon-mode +1)

(my-require-package 'which-key)
(require 'which-key)
(which-key-mode +1)

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
  (my|do-after-display-system-init))

(defun my-removes-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  ;; removes the GUI elements
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (unless (my-window-system-is-mac)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1))
  ;; suppress GUI features
  (setq use-dialog-box nil)
  (setq use-file-dialog nil)
  (setq x-gtk-use-old-file-dialog nil)
  (setq x-gtk-file-dialog-help-text nil)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message t))
(my-removes-gui-elements)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; font
;; (defvar my-default-font '("DejaVu Sans"
;;                           :size 16
;;                           :weight normal
;;                           :width normal
;;                           :powerline-scale 1.1))
(defvar my-default-font nil)

(defun my-set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (scale (plist-get props :powerline-scale))
               (font-props (my-mplist-remove
                            (my-mplist-remove props :powerline-scale)
                            :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (my/echo "Setting font \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          (setq-default powerline-scale scale)
          (setq-default powerline-height (my--compute-powerline-height))
          ;; fallback font for unicode characters used in spacemacs
          (pcase system-type
            (`gnu/linux
             (setq fallback-font-name "NanumGothic")
             (setq fallback-font-name2 "NanumGothic"))
            (`darwin
             (setq fallback-font-name "Arial Unicode MS")
             (setq fallback-font-name2 "Arial Unicode MS"))
            (`windows-nt
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (`cygwin
             (setq fallback-font-name "MS Gothic")
             (setq fallback-font-name2 "Lucida Sans Unicode"))
            (other
             (setq fallback-font-name nil)
             (setq fallback-font-name2 nil)))
          (when (and fallback-font-name fallback-font-name2)
            ;; remove any size or height properties in order to be able to
            ;; scale the fallback fonts with the default one (for zoom-in/out
            ;; for instance)
            (let* ((fallback-props (my-mplist-remove
                                    (my-mplist-remove font-props :size)
                                    :height))
                   (fallback-spec (apply 'font-spec
                                         :name fallback-font-name
                                         fallback-props))
                   (fallback-spec2 (apply 'font-spec
                                          :name fallback-font-name2
                                          fallback-props)))
              ;; window numbers
              (set-fontset-font "fontset-default"
                                '(#x2776 . #x2793) fallback-spec nil 'prepend)
              ;; mode-line circled letters
              (set-fontset-font "fontset-default"
                                '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; mode-line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
              ;; new version lighter
              (set-fontset-font "fontset-default"
                                '(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
        (throw 'break t)))
    nil))

(defun my--compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(when my-default-font
  (unless (my-set-default-font my-default-font)
    (message
     "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
     (if (listp (car my-default-font))
         (mapconcat 'car my-default-font ", ")
       (car my-default-font)))))


(defun my-fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))

(defun my-console-frame-setup ()
  (when (< emacs-major-version 23)
    (my-fix-up-xterm-control-arrows))
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (when (fboundp 'mwheel-install)
    (mwheel-install)))

(provide 'my-gui)
;;; my-gui.el ends here
