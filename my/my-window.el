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
  :init
  :config
  ;; insert a " "
  ;; (defun window-numbering-get-number-string (&optional window)
  ;;   (let ((s (concat (int-to-string (window-numbering-get-number window)) " ")))
  ;;     (propertize s 'face 'window-numbering-face)))
  (set-face-attribute 'window-numbering-face nil
  		      :foreground "orange")
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

(defun my/window-toggle-show ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun my/window-split ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'my/split-window)
      (progn
        (jump-to-register :my/split-window)
        (setq this-command 'my/unsplit-window))
    (window-configuration-to-register :my/window-split)
    (switch-to-buffer-other-window nil)))

(defun my/window-delete (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

;;----------------------------------------------------------------------------
;; when splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun my-window-split-func-with-other-buffer (split-function)
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

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun my/window-split-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (my-window-split-func-with-other-buffer 'split-window-horizontally))))

(defun my/window-split-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (my-window-split-func-with-other-buffer 'split-window-vertically))))

(defun my/window-split-horizontally-then-switch ()
  (interactive)
  (save-excursion
    (funcall (my-window-split-func-with-other-buffer 'split-window-horizontally))))

(defun my/window-split-vertically-then-switch ()
  (interactive)
  (save-excursion
    (funcall (my-window-split-func-with-other-buffer 'split-window-vertically))))


(defun my/window-toggle-current-file-dedication ()
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
(defun my/window-rotate-forward (count)
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

(defun my/window-rotate-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (my/window-rotate-forward (* -1 count)))

(defun my/window-rotate (&optional arg)
  "Rotate use `my/window-rotate-backward'."
  (interactive "P")
  (if arg
      (my/window-rotate-forward -1)
    (my/window-rotate-forward 1)))

(global-set-key (kbd "<f1>") 'my/window-toggle-show)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'my/window-toggle-show)
(global-set-key (kbd "C-x 2") 'my/window-split-vertically-then-switch)
(global-set-key (kbd "C-x 3") 'my/window-split-horizontally-then-switch)
(global-set-key (kbd "C-x |") 'my/window-split-horizontally-instead)
(global-set-key (kbd "C-x _") 'my/window-split-vertically-instead)
(global-set-key (kbd "C-x C-n") 'my/window-toggle-current-file-dedication)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-c C-s") 'my/window-rotate)

(defvar my-window-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") 'my/window-rotate-forward)
    (define-key map (kbd "r") 'my/window-rotate-backward)

    (define-key map (kbd "<") 'enlarge-window-horizontally)
    (define-key map (kbd ">") 'shrink-window-horizontally)
    (define-key map (kbd "+") 'enlarge-window)
    (define-key map (kbd "_") 'shrink-window)
    (define-key map (kbd ")") 'balance-windows)

    (define-key map (kbd "o") 'other-window)
    (define-key map (kbd "m") 'my/window-toggle-show)
    (define-key map (kbd "q") 'my/window-delete)
    (define-key map (kbd "k") 'kill-buffer-and-window)

    (define-key map (kbd "d") 'delete-window)
    (define-key map (kbd "t") 'my/window-toggle-show)
    (define-key map (kbd "2") 'my/window-split-vertically-then-switch)
    (define-key map (kbd "|") 'my/window-split-vertically-instead)
    (define-key map (kbd "3") 'my/window-split-horizontally-then-switch)
    (define-key map (kbd "_") 'my/window-split-horizontally-instead)

    (define-key map (kbd "n") 'my/window-toggle-current-file-dedication)
    map)
  "My window group keymap.")
(define-key ctl-x-map "w" my-window-keymap)

;; replace (compose-mail)
;; (global-set-key (kbd "C-x m") 'my/window-toggle-show)
;; (global-set-key (kbd "C-x C-m") 'my/window-split)

(provide 'my-window)
;;; my-window.el ends here
