;;; my-gud.el --- gud                                -*- lexical-binding: t; -*-

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

(defvar gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight activate)
  "Highlight current line."
  (let* ((ov gud-overlay)
          (bf (gud-find-file true-file)))
    (with-current-buffer bf
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
        (current-buffer)))))

(defun gud-kill-buffer ()
  (if (derived-mode-p 'gud-mode)
    (delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)

;; {{ hack buffer
;; move the cursor to the end of last line if it's gud-mode
(defun hack-gud-mode ()
  (when (derived-mode-p 'gud-mode)
    (goto-char (point-max)))
  ;; (when (string= major-mode "gud-mode")
  ;;   (goto-char (point-max)))
  )

(defadvice switch-to-buffer (after switch-to-buffer-after activate)
  (hack-gud-mode))

(defadvice switch-window (after switch-window activate)
  (hack-gud-mode))

;; windmove-do-window-select is from windmove.el
(defadvice windmove-do-window-select (after windmove-do-window-select-after activate)
  (hack-gud-mode))
;; }}

(defun gud-cls (&optional num)
  "clear gud screen"
  (interactive "p")
  (let ((old-window (selected-window)))
    (save-excursion
      (cond
        ((buffer-live-p (get-buffer "*gud-main*"))
          (select-window (get-buffer-window "*gud-main*"))
          (end-of-buffer)
          (recenter-top-bottom)
          (if (> num 1) (recenter-top-bottom))
          (select-window old-window))
        (t (error "GUD buffer doesn't exist!"))
        ))
    ))

(after-load 'gud
  (gud-def gud-kill "kill" "\C-k" "Kill the debugee"))

(defun gud-kill-yes ()
  "gud-kill and confirm with y"
  (interactive)
  (let ((old-window (selected-window)))
    (save-excursion
      (cond
        ((buffer-live-p (get-buffer "*gud-main*"))
          (gud-kill nil)
          (select-window (get-buffer-window "*gud-main*"))
          (insert "y")
          (comint-send-input)
          (recenter-top-bottom)
          (select-window old-window))
        (t (error "GUD buffer doesn't exist!"))
        ))
    ))

;; (global-set-key "\C-x\C-a\C-g" 'gud-run)

(provide 'my-gud)
;;; my-gud.el ends here
