;;; my-whitespace-cleanup-mode.el --- my whitespace cleanup mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  StreamOcean

;; Author: liyunteng <liyunteng@streamocean.com>
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

(defgroup whitespace-cleanup-mode nil
  "My whitespce cleanup."
  :group 'my)

;;;###autoload
(defcustom my-whitespace-cleanup 'trailing
  "My whitespace cleanup.

`all' cleanup all whitespace.
`trailing' cleanup trailing whitespace.
`changed' cleanup change lines, need ws-bulet-mode."

  :group 'whitespace-cleanup-mode
  :type '(choice (const :tag "all whitespace" 'all)
		 (const :tag "trailing whitespace" 'trailing)
		 (const :tag "changed lines" 'changed)))

;;;###autoload
(define-minor-mode whitespace-cleanup-mode
  "Minor mode to clean whitespace.

The minor mode is based on the value of the dotfile variable
 `dotspacemacs-whitespace-cleanup' to determine the behavior
of the cleanup."
  :lighter " WSC"
  :
  :group 'whitespace-cleanup-mode
  (if whitespace-cleanup-mode
      (whitespace-cleanup//turn-on)
    (whitespace-cleanup//turn-off)))

;;;###autoload
(define-globalized-minor-mode global-whitespace-cleanup-mode
  whitespace-cleanup-mode whitespace-cleanup//turn-on)

(defun whitespace-cleanup/on-message ()
  "Return a string to display when the mode is activated. GLOBAL?"
  (pcase my-whitespace-cleanup
    (`all
     (format "whitespace-cleanup enabled (all whitespace)"))
    (`trailing
     (format "whitespace-cleanup enabled (trailing whitespace)"))
    (`changed
     (format "whitespace-cleanup enabled (changed lines)"))))

(defun whitespace-cleanup//turn-on ()
  "Turn on `my-whitespace-cleanup-mode'. GLOBAL?"
  (pcase my-whitespace-cleanup
    (`all
     (add-hook 'before-save-hook 'whitespace-cleanup nil t))
    (`trailing
     (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
    (`changed
     (when (fboundp 'ws-butler-mode) (ws-butler-mode)))))

(defun whitespace-cleanup//turn-off ()
  "Turn off `my-whitespace-cleanup-mode'. GLOBAL?"
  (pcase my-whitespace-cleanup
    (`all
     (remove-hook 'before-save-hook 'whitespace-cleanup t))
    (`trailing
     (remove-hook 'before-save-hook 'delete-trailing-whitespace t))
    (`changed
     (when (fboundp 'ws-butler-mode) (ws-butler-mode -1)))))

(provide 'my-whitespace-cleanup-mode)
;;; my-whitespace-cleanup-mode.el ends here
