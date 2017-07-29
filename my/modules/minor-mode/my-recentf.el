;;; my-recentf.el --- my recentf                     -*- lexical-binding: t; -*-

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

(require 'recentf)

(setq-default
 recentf-max-saved-items 1000
 recentf-max-menu-items 15
 recentf-exclude '("/tmp/" "/ssh:" "/root@" "/sudo:"
                   "/TAGS$" "/GTAGS$" "/GRAGS" "/GPATH$"))
(add-hook 'after-init-hook
          'recentf-load-list 'recentf-cleanup)


(provide 'my-recentf)
;;; my-recentf.el ends here
