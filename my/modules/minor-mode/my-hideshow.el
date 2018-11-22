;;; my-hideshow.el --- hideshow                      -*- lexical-binding: t; -*-

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


(use-package hide-comnt
  :ensure t
  :bind ("C-c m ;" . hide/show-comments-toggle))

(use-package hideshow
  :ensure t
  :commands (hs-minor-mode)
  :bind (:map hs-minor-mode-map
	      ("C-c m h" . hs-hide-block)
	      ("C-c m s" . hs-show-block)
	      ("C-c m H" . hs-hide-all)
	      ("C-c m S" . hs-show-all)
	      ("C-c m l" . hs-hide-level)
	      ("C-c m m" . hs-toggle-hiding)
	      ("C-c m M" . my/hs-toggle-hiding-all)
	      ("C-c m i" . my/hs-toggle-initial-comment-block)
	      ([shfit mouse-1] . hs-mouse-toggle-hiding))

  :init

  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun my/hs-toggle-hiding-all ()
    "Toggle hideshow all."
    (interactive)
    (setq my-hs-hide (not my-hs-hide))
    (if my-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (defun hs-show-initial-comment-block ()
    "Hide the first block of comments in a file.
This can be useful if you have huge RCS logs in those comments."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (skip-chars-forward " \t\n\f")
       (hs-show-block))))

  (defun my/hs-toggle-initial-comment-block ()
    "Hide the first block of comments in a file.
This can be useful if you have huge RCS logs in those comments."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n\f")
      (if (and (hs-inside-comment-p)
	       (hs-already-hidden-p))
	  (hs-show-initial-comment-block)
	(hs-hide-initial-comment-block))))

  :config
  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format " ... <%d>"
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'font-lock-type-face)))))
  )

(provide 'my-hideshow)
;;; my-hideshow.el ends here
