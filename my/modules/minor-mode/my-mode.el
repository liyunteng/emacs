;;; my-mode.el --- my mode                           -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author:  <lyt@gentoo>
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
(my-require-package 'move-text)
(my-require-package 'imenu-anywhere)
(my-require-package 'crux)
(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)
    (define-key map (kbd "C-c g") 'my/baidu)
    ;; (define-key map (kbd "C-c G") 'my/github)
    ;; (define-key map (kbd "C-c y") 'my/youtube)
    ;; (define-key map (kbd "C-c U") 'my/duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
    (define-key map [(shift return)] 'crux-smart-open-line)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map [(control shift return)] 'crux-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'crux-recentf-ido-find-file)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-user-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    ;; make some use of the Super key
    ;; (define-key map (kbd "s-g") 'god-local-mode)
    (define-key map (kbd "s-r") 'crux-recentf-ido-find-file)
    (define-key map (kbd "s-j") 'crux-top-join-line)
    (define-key map (kbd "s-k") 'crux-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-log-buffer-file)
    (define-key map (kbd "s-m b") 'magit-blame)
    (define-key map (kbd "s-o") 'crux-smart-open-line-above)

    map)
  "Keymap for my mode.")

(defun my-mode-add-menu ()
  "Add a menu entry for `prelude-mode' under Tools."
  (easy-menu-add-item nil '("Tools")
                      '("MY"
                        ("Files"
                         ["Open with..." crux-open-with]
                         ["Delete file and buffer" crux-delete-file-and-buffer]
                         ["Rename buffer and file" crux-rename-buffer-and-file])

                        ("Buffers"
                         ["Clean up buffer or region" crux-cleanup-buffer-or-region]
                         ["Kill other buffers" crux-kill-other-buffers])

                        ("Editing"
                         ["Insert empty line" prelude-insert-empty-line]
                         ["Move line up" prelude-move-line-up]
                         ["Move line down" prelude-move-line-down]
                         ["Duplicate line or region" prelude-duplicate-current-line-or-region]
                         ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
                         ["Insert date" crux-insert-date]
                         ["Eval and replace" crux-eval-and-replace]
                         )

                        ("Windows"
                         ["Swap windows" crux-swap-windows])

                        ("General"
                         ["Visit term buffer" crux-visit-term-buffer]
                         ["Search in Baidu" my/baidu]
                         ["View URL" crux-view-url]))
                      "Search Files (Grep)...")

  (easy-menu-add-item nil '("Tools") '("--") "Search Files (Grep)..."))

(defun my-mode-remove-menu ()
  "Remove `my-mode' menu entry."
  (easy-menu-remove-item nil '("Tools") "MY")
  (easy-menu-remove-item nil '("Tools") "--"))

(defun turn-on-my-mode ()
  "Turn on `my-mode'."
  (my-mode +1))

(defun turn-off-my-mode ()
  "Turn off `my-mode'."
  (my-mode -1))

;; define minor mode
(define-minor-mode my-mode
  "Minor mode to consolidate Emacs Prelude extensions.

\\{my-mode-map}"
  :lighter " M"
  :keymap my-mode-map
  (if my-mode
      ;; on start
      (my-mode-add-menu)
    ;; on stop
    (my-mode-remove-menu)))
(define-globalized-minor-mode global-my-mode my-mode turn-on-my-mode)

(global-my-mode +1)
(provide 'my-mode)
;;; my-mode.el ends here
