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

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------

;; Make "C-x o" prompt for a target window when there are more than 2

(my-require-package 'switch-window)
(my-require-package 'window-numbering)

(setq-default display-buffer-reuse-frames t)
(setq-default split-height-threshold 80)
(setq-default split-width-threshold 160)
(setq-default switch-to-buffer-preserve-window-point t)
(setq-default fit-window-to-buffer-horizontally t)
(setq-default fit-frame-to-buffer t)
;; important for golden-ratio to better work
(setq window-combination-resize t)

(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)

(window-numbering-mode t)

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
(defun my/split-window-horizontally ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (my-split-window-func-with-other-buffer 'split-window-horizontally))))

(defun my/split-window-vertically ()
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

(defun my/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
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

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))
;;(define-key my-mode-map (kbd "C-c C-s") 'my/rotate-windows-backward)

(defun my--revert-buffer-function (ignore-auto noconfirm)
  "Revert buffer if buffer without file, or call revert-buffer--default"
  (if (buffer-file-name)
      (funcall #'revert-buffer--default ignore-auto noconfirm)
    (call-interactively major-mode)
    ))
(setq revert-buffer-function 'my--revert-buffer-function)

(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

(provide 'my-window)
;;; my-window.el ends here
