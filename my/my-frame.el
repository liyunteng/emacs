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
        '("%F"
          (:eval (if (frame-parameter nil 'client)
                     "*"))
          "  -  " (:eval (if (buffer-file-name)
                             ;; (file-truename (buffer-file-name))
                             (abbreviate-file-name (buffer-file-name))
                           "%b"))))


  (let ((no-border '(internal-border-width . 0)))
    (add-to-list 'default-frame-alist no-border)
    (add-to-list 'initial-frame-alist no-border)))
(my/set-gui-frame)


;; fonts
(setq font-use-system-font t)
;; 中文使用WenQuanYi Micro Hei Mono, 其他使用DejaVu Sans Mono
(create-fontset-from-fontset-spec
 "-*-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-my")
(set-fontset-font "fontset-my" 'han "WenQuanYi Micro Hei Mono")
;; (create-fontset-from-fontset-spec
;;  (concat "-*-DejaVu Sans Mono-normal-normal-normal-*-15-*-*-*-m-0-fontset-my,"
;;          "chinese-gbk: -*-WenQuanYi Micro Hei Mono-normal-normal-normal-*-*-*-*-*-iso10646-1,"
;;          "chinese-iso-8bit: -*-WenQuanYi Micro Hei Mono-normal-normal-normal-*-*-*-*-*-iso10646-1,"
;;          "chinese-big5: -*-WenQuanYi Micro Hei Mono-normal-normal-normal-*-*-*-*-*-iso10646-1,"
;;          "chinese-cns11643: -*-WenQuanYi Micro Hei Mono-normal-normal-normal-*-*-*-*-*-iso10646-1"
;;          ))
(add-to-list 'default-frame-alist '(font . "fontset-my"))


;; frame opacity
(defun my--adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (min (+ incr oldalpha) 100)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(defun my/frame-opacity-adjust (inc)
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes t))
    (let* ((base (event-basic-type ev))
           (step (pcase base
                   ((or ?+ ?=) (* 5 inc))
                   (?- (* 5 (- inc)))
                   (?0 100)
                   (_ inc))))
      (my--adjust-opacity nil step))
    (and echo-keystrokes
         (message "Use +,-,0 for further adjustment"))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (mods '(() (control)))
         (dolist (key '(?- ?+ ?= ?0))
           (define-key map (vector (append mods (list key)))
             (lambda () (interactive) (my/frame-opacity-adjust (abs inc))))))
       map))))
(global-set-key (kbd "C-M-=") 'my/frame-opacity-adjust)
(global-set-key (kbd "C-M--") 'my/frame-opacity-adjust)
(global-set-key (kbd "C-M-0") 'my/frame-opacity-adjust)

(global-set-key (kbd "C-x C-=") 'text-scale-adjust)
(global-set-key (kbd "C-x C--") 'text-scale-adjust)
(global-set-key (kbd "C-x C-0") 'text-scale-adjust)

(use-package disable-mouse
  :disabled
  :ensure t
  ;; :init
  ;; (disable-mouse-global-mode +1)
  )

;; (use-package smart-mode-line
;;   :ensure t
;;   :commands (smart-mode-line-enable sml/setup)
;;   :init
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/theme 'respectful)
;;   (sml/setup)
;;   ;; (add-hook 'after-init-hook #'sml/setup)
;;   )

(use-package powerline
  :ensure t
  :init
  (defun powerline-my-theme ()
    "Setup a mode-line with major and minor modes centered."
    (interactive)
    (setq-default mode-line-format
        	      '("%e"
                    (:eval (window-number-string))
        	        (:eval
        	         (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
        		            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
        		            (face1 (if active 'powerline-active1 'powerline-inactive1))
        		            (face2 (if active 'powerline-active2 'powerline-inactive2))
        		            (separator-left (intern (format "powerline-%s-%s"
        						                            (powerline-current-separator)
        						                            (car powerline-default-separator-dir))))
        		            (separator-right (intern (format "powerline-%s-%s"
        						                             (powerline-current-separator)
        						                             (cdr powerline-default-separator-dir))))
        		            (lhs (list (powerline-raw mode-line-modified face0 'l)
                                       (when powerline-display-buffer-size
                                         (powerline-buffer-size face0 'l)
                                         (powerline-raw " " face0 'l))
                                       (when powerline-display-mule-info
                                         (powerline-raw mode-line-mule-info face0 'r))
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " " face0 'l)
        			                   (funcall separator-left face0 face1)
        			                   (powerline-narrow face1 'l)
        			                   (powerline-vc face1)))
        		            (rhs (list (powerline-raw global-mode-string face1 'r)
                                       (when (and (derived-mode-p 'prog-mode) which-function-mode)
                                         (powerline-raw which-func-format face1 'r))
        			                   (funcall separator-right face1 face0)
        			                   (powerline-raw " " face0 'r)
                                       (powerline-raw "%l:%c" face0 'r)
        			                   (powerline-raw "%4p" face0 'r)
        			                   (powerline-hud face2 face1)
        			                   (powerline-fill face0 0)))
        		            (center (list (powerline-raw " " face1)
        				                  (funcall separator-left face1 face2)
        				                  (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
        				                    (powerline-raw erc-modified-channels-object face2 'l))
        				                  (powerline-major-mode face2 'l)
        				                  (powerline-process face2)
        				                  (powerline-raw " :" face2)
        				                  (powerline-minor-modes face2 'l)
        				                  (powerline-raw " " face2)
        				                  (funcall separator-right face2 face1))))
        	           (concat (powerline-render lhs)
        		               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
        		               (powerline-render center)
        		               (powerline-fill face1 (powerline-width rhs))
        		               (powerline-render rhs)))))))
  :config
  (powerline-my-theme))

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
  (which-key-mode +1)
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order-reverse))

;; (setq ring-bell-function 'ignore
;;       visible-bell nil)p
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
;;; my-frame.el ends here1
