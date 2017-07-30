;;; my-hideshow.el --- hideshow

;; Copyright (C) 2014  liyunteng

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

(my-require-package 'hideshow)
(require 'hideshow)

;;代码折叠
(dolist (hook '(prog-mode-hook
                nxml-mode
                html-mode
                web-mode))
  (add-hook hook 'hs-minor-mode))

(defvar hs-command-prefix)
(define-prefix-command 'hs-command-prefix)
(define-key hs-minor-mode-map (kbd "C-c M-h") 'hs-command-prefix)

(define-key hs-command-prefix (kbd "h") 'hs-hide-block)
(define-key hs-command-prefix (kbd "s") 'hs-show-block)
(define-key hs-command-prefix (kbd "H") 'hs-hide-all)
(define-key hs-command-prefix (kbd "S") 'hs-show-all)
(define-key hs-command-prefix (kbd "l") 'hs-hide-level)
(define-key hs-command-prefix (kbd "m") 'hs-toggle-hiding)
(define-key hs-minor-mode-map [(shift mouse-2)] 'hs-mouse-toggle-hiding)

(provide 'my-hideshow)
;;; my-hideshow.el ends here
