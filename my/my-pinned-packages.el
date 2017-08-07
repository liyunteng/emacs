;;; my-pinned-packages.el --- package pinned packages  -*- lexical-binding: t; -*-

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
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-archives '(
                         ("melpa" . "http://mirrors.163.com/elpa/melpa/")
                         ("melpa-stable" . "http://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("org" . "http://mirrors.163.com/elpa/org/")
						 ("marmalade" . "http://mirrors.163.com/elpa/marmalade/")
						 ))
(add-to-list 'package-pinned-packages
             '(switch-window . "melpa-stable"))

(provide 'my-pinned-packages)
;;; my-pinned-packages.el ends here
