;;; my-frame.el --- gui                                -*- lexical-binding: t; -*-

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

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  :init
  (exec-path-from-shell-initialize))

(defvar my-after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame.")
(defvar my-after-make-window-system-frame-hook '()
  "Hooks to run after creating a new window-system frame.")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `my-after-make-console-frame-hooks' or
`my-after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'my-after-make-window-system-frame-hooks
                 'my-after-make-console-frame-hooks))))
(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst my--initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when my--initial-frame
                       (run-after-make-frame-hooks my--initial-frame))))


(defun my--console-frame-setup ()
  "Mouse in a terminal (Use shift to paste with middle button)."
  (xterm-mouse-mode 1)
  (mwheel-install))
(add-hook 'my-after-make-console-frame-hooks 'my--console-frame-setup)


(defun my/set-gui-frame ()
  "Supperess GUI features."
  (interactive)
  ;; removes the GUI elements
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; tooltips in echo-aera
  (tooltip-mode -1)
  ;; buffer menu
  (setq buffers-menu-max-size 10)
  (menu-bar-mode -1)

  (blink-cursor-mode -1)
  (setq use-dialog-box nil)
  (setq use-file-dialog nil)
  (setq x-gtk-use-old-file-dialog nil)
  (setq x-gtk-file-dialog-help-text nil)
  (setq inhibit-startup-screen t)

  (setq inhibit-startup-echo-area-message t)


  ;; title format
  (setq frame-title-format
        '( "Emacs - " (:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b"))))

  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border)))
(my/set-gui-frame)


;; 使用系统字体
(setq-default font-use-system-font t)

;; 中文使用wqy-microhei,其他使用DejaVu Sans Mono
(when (display-graphic-p)
  (create-fontset-from-fontset-spec
   "-*-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-my,
 chinese-gbk: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1,
 chinese-iso-8bit: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1,
 chinese-big5: -*-WenQuanYi Micro Hei-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
  (setq default-frame-alist (append '((font . "fontset-my")) default-frame-alist))
  (set-frame-font "fontset-my"))





;; 调整frame透明度
(defun my--adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(defun my/frame-opacity-increase (&optional n)
  "Increase frame opacity."
  (interactive "p")
  (let ((num (if n n 5)))
    (my--adjust-opacity nil num)))

(defun my/frame-opacity-reduce (&optional n)
  "Reduce frame opacity."
  (interactive "p")
  (let ((num (if n (- 0 n) -5)))
    (my--adjust-opacity nil num)))

(defun my/frame-opacity-reset ()
  "Reset frame opacity."
  (interactive)
  (modify-frame-parameters nil `((alpha . 100))))

(global-set-key (kbd "C-M-=") 'my/frame-opacity-increase)
(global-set-key (kbd "C-M--") 'my/frame-opacity-reduce)
(global-set-key (kbd "C-M-0") 'my/frame-opacity-reset)

(global-set-key (kbd "C-x C-=") 'text-scale-adjust)
(global-set-key (kbd "C-x C--") 'text-scale-adjust)
(global-set-key (kbd "C-x C-0") 'text-scale-adjust)

(use-package disable-mouse
  :disabled
  :ensure t
  ;; :init
  ;; (disable-mouse-global-mode +1)
  )

(use-package smart-mode-line
  :ensure t
  :commands (smart-mode-line-enable sml/setup)
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup)
  ;; (add-hook 'after-init-hook #'sml/setup)
  )

(use-package beacon
  :ensure t
  :commands (beacon-mode)
  :diminish beacon-mode
  :init
  (beacon-mode +1)
  :config
  (setq beacon-size 40))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode +1))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :diminish fci-mode
  :commands (turn-on-fci-mode
	     turn-off-fci-mode
	     fci-mode)
  :init
  (my|add-toggle fci-mode
    :status fci-mode
    :on (turn-on-fci-mode)
    :off (turn-off-fci-mode)
    :documentation "Display the fill column indicator"
    :global-key "C-# i"
    )
  :config
  (setq fci-rule-width 2
	;; fci-rule-color "#D0BF8F"
	)
  (push '(fci-mode "") minor-mode-alist))

;; (setq ring-bell-function 'ignore
;;       visible-bell nil)
;; (setq ring-bell-function
;; 		'(lambda ()
;; 		   (invert-face 'mode-line)
;; 		   (run-with-timer 0.05 nil 'invert-face 'mode-line)))
(use-package mode-line-bell
  :ensure t
  :diminish mode-line-bell-mode
  :init
  (mode-line-bell-mode +1))

(use-package dimmer
  :ensure t
  :init
  (dimmer-mode +1))

(use-package speedbar
  :defer t
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
(global-set-key (kbd "<f12>") 'toggle-menu-bar-mode-from-frame)
;; (global-set-key (kbd "<f12>") 'menu-bar-mode)

(provide 'my-frame)
;;; my-frame.el ends here
