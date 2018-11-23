;;; my-window.el --- window                          -*- lexical-binding: t; -*-

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

(setq window-combination-resize t)

(setq-default display-buffer-reuse-frames t)
(setq-default split-height-threshold 80)
(setq-default split-width-threshold 160)
(setq-default switch-to-buffer-preserve-window-point t)
(setq-default fit-window-to-buffer-horizontally t)
(setq-default fit-frame-to-buffer t)

(use-package switch-window
  :ensure t
  :bind
  ("C-x o" . switch-window)
  :config
  (setq switch-window-shortcut-style 'alphabet)
  (setq switch-window-timeout nil))

(use-package window-numbering
  :ensure t
  :defer t
  :config
  (set-face-attribute 'window-numbering-face nil
  		      :foreground "#ABC")
  (window-numbering-mode +1))

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :defer t
  :config
  (windmove-default-keybindings))

;; enable winner-mode to manage window configurations
(use-package winner
  :bind (("C-c <left>" . winner-undo)
         ("C-c <right>" . winner-redo))
  :init
  (winner-mode +1)
  :config
  (setq winner-boring-buffers
        (append winner-boring-buffers
                '("*Completions*"
                  "*Compile-Log*"
                  "*inferior-lisp*"
                  "*Fuzzy Completions*"
                  "*Apropos*"
                  "*Help*"
                  "*cvs*"
                  "*Buffer List*"
                  "*Ibuffer*"
                  "*esh command on file*"
                  ))))

(use-package default-text-scale
  :ensure t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)
	 ("C-M-0" . default-text-scale-reset)))

(defun my/adjust-opacity (frame incr)
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

;;----------------------------------------------------------------------------
;; when splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun my-split-window-func-with-other-buffer (split-function)
  "Split window use SPLIT-FUNCTION."
  (lexical-let ((s-f split-function))
    (lambda (&optional arg)
      "split this window and switch to the new window unless arg is provided."
      (interactive "P")
      (funcall s-f)
      (let ((target-window (next-window)))
        (set-window-buffer target-window (other-buffer))
        (unless arg
          (select-window target-window))))))

(defun my/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun my/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (my-split-window-func-with-other-buffer 'split-window-horizontally))))

(defun my/split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (my-split-window-func-with-other-buffer 'split-window-vertically))))

(defun my/split-window-horizontally-then-switch ()
  (interactive)
  (save-excursion
    (funcall (my-split-window-func-with-other-buffer 'split-window-horizontally))))

(defun my/split-window-vertically-then-switch ()
  (interactive)
  (save-excursion
    (funcall (my-split-window-func-with-other-buffer 'split-window-vertically))))

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun my/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'my/split-window)
      (progn
        (jump-to-register :my/split-window)
        (setq this-command 'my/unsplit-window))
    (window-configuration-to-register :my/split-window)
    (switch-to-buffer-other-window nil)))

(defun my/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (if was-dedicated
	(setq-local mode-line-process nil)
      (setq-local mode-line-process " [D]"))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(defun my/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun my/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my-swap-windows (window1 window2)
  "Swap two windows.
WINDOW1 and WINDOW2 must be valid windows. They may contain child
windows."
  (let ((state1 (window-state-get window1))
        (state2 (window-state-get window2)))
    ;; to put state into dedicated windows, we must undedicate them first (not
    ;; needed with Emacs 25.1)
    (dolist (win (list window1 window2))
      (if (window-live-p win)
          (set-window-dedicated-p win nil)
        ;; win has sub-windows, undedicate all of them
        (walk-window-subtree (lambda (leaf-window)
                               (set-window-dedicated-p leaf-window nil))
                             win)))
    (window-state-put state1 window2)
    (window-state-put state2 window1)))

;; from @bmag
(defun my/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun my/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))
;;(define-key my-mode-map (kbd "C-c s") 'my/rotate-windows-forward)

(defun my/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (my/rotate-windows-forward (* -1 count)))


(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'my/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") 'my/split-window-vertically-then-switch)
(global-set-key (kbd "C-x 3") 'my/split-window-horizontally-then-switch)
(global-set-key (kbd "C-x |") 'my/split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'my/split-window-vertically-instead)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-x C-n") 'my/toggle-current-window-dedication)
(global-set-key (kbd "<f1>") 'my/split-window)
;; 调整window大小
(global-set-key (kbd "C-M-)") 'balance-windows)
(global-set-key (kbd "C-M-+") 'enlarge-window)
(global-set-key (kbd "C-M-_") 'shrink-window)
(global-set-key (kbd "C-M-<") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M->") 'shrink-window-horizontally)
;; 调整window透明度
(global-set-key (kbd "M-C-8") (lambda () (interactive) (my/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (my/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)


(provide 'my-window)
;;; my-window.el ends here
