;;; my-themes.el --- themes                          -*- lexical-binding: t; -*-

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
;; (use-package color-theme
;;   :ensure t
;;   :disabled t)
;; (use-package color-theme-sanityinc-solarized
;;   :ensure t
;;   :disabled t)
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :disabled t)
;; (use-package monokai-theme
;;   :ensure t
;;   :disabled t)
(use-package color)

(use-package zenburn-theme
  :ensure t
  :if (eq (display-color-cells) 8)
  :init
  (defvar my-zenburn-override-colors-alist
  	'(("zenburn-fg+1"     . "#FFFFFF")
  	  ("zenburn-fg"       . "#FFFFFF")
  	  ("zenburn-fg-1"     . "#FFFFFF")
  	  ("zenburn-bg-2"     . "#FFFFFF")
  	  ("zenburn-bg-1"     . "#FFFF00")
  	  ("zenburn-bg-05"    . "#FFFF00")
  	  ("zenburn-bg"       . "#000000")
  	  ("zenburn-bg+05"    . "#0000FF")
  	  ("zenburn-bg+1"     . "#0000FF")
  	  ("zenburn-bg+2"     . "#0000FF")
  	  ("zenburn-bg+3"     . "#FFFFFF")
  	  ("zenburn-red+1"    . "#FF0000")
  	  ("zenburn-red"      . "#FF0000")
  	  ("zenburn-red-1"    . "#FF0000")
  	  ("zenburn-red-2"    . "#FF0000")
  	  ("zenburn-red-3"    . "#FF0000")
  	  ("zenburn-red-4"    . "#FF0000")
  	  ("zenburn-orange"   . "#FFFF00")
	  ("zenburn-yellow"   . "#FFFF00")
  	  ("zenburn-yellow-1" . "#FFFF00")
  	  ("zenburn-yellow-2" . "#FFFF00")
  	  ("zenburn-green-1"  . "#00FF00")
  	  ("zenburn-green"    . "#00FF00")
  	  ("zenburn-green+1"  . "#00FF00")
  	  ("zenburn-green+2"  . "#00FF00")
  	  ("zenburn-green+3"  . "#00FF00")
  	  ("zenburn-green+4"  . "#00FF00")
  	  ("zenburn-cyan"     . "#00FFFF")
  	  ("zenburn-blue+1"   . "#0000FF")
  	  ("zenburn-blue"     . "#0000FF")
  	  ("zenburn-blue-1"   . "#0000FF")
  	  ("zenburn-blue-2"   . "#0000FF")
  	  ("zenburn-blue-3"   . "#0000FF")
  	  ("zenburn-blue-4"   . "#0000FF")
  	  ("zenburn-blue-5"   . "#0000FF")
  	  ("zenburn-magenta"  . "#FF00FF")))
  (setq zenburn-override-colors-alist my-zenburn-override-colors-alist)
  )

(defcustom my-theme 'zenburn
  "My theme.")

(load-theme my-theme t)

(provide 'my-themes)
;;; my-themes.el ends here
