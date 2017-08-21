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
  	  ("zenburn-fg-1"     . "#FFFF00")
  	  ("zenburn-bg-2"     . "#FFFF00")
  	  ("zenburn-bg-1"     . "#FFFF00")
  	  ("zenburn-bg-05"    . "#FFFF00")
  	  ("zenburn-bg"       . "#000000")
  	  ("zenburn-bg+05"    . "#0000FF")
  	  ("zenburn-bg+1"     . "#0000FF")
  	  ("zenburn-bg+2"     . "#0000FF")
  	  ("zenburn-bg+3"     . "#FFFFFF")
  	  ("zenburn-red+1"    . "#FFFF00")
  	  ("zenburn-red"      . "#FF0000")
  	  ("zenburn-red-1"    . "#FFFFFF")
  	  ("zenburn-red-2"    . "#FFFFFF")
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
  	  ("zenburn-blue"     . "#FFFFFF")
  	  ("zenburn-blue-1"   . "#0000FF")
  	  ("zenburn-blue-2"   . "#0000FF")
  	  ("zenburn-blue-3"   . "#0000FF")
  	  ("zenburn-blue-4"   . "#0000FF")
  	  ("zenburn-blue-5"   . "#0000FF")
  	  ("zenburn-magenta"  . "#FF00FF")))
  (setq zenburn-override-colors-alist my-zenburn-override-colors-alist)
  :config
  (after-load 'company
  	(custom-theme-set-faces
  	 'zenburn
  	 `(company-tooltip ((t (:background "#000000"))))
  	 `(company-tooltip-selection ((t (:background "#00FFFF" :weight bold))))
  	 `(company-tooltip-search ((t (:background "#000000" :foreground "red"))))
  	 `(company-tooltip-search-selection ((t (:background "#0000FF" :foreground "red" :weight bold))))
  	 ))
  (after-load 'helm
  	(custom-theme-set-faces
  	 'zenburn
  	 `(helm-selection ((t (:background "#00FFFF" :weight bold :foreground "#FFFF00"))))))
  )

(defcustom my-theme 'zenburn
  "My theme.")

(load-theme my-theme t)

;; (custom-set-faces
;;  `(company-tooltip-mouse ((t (:background "#2b2b2b"))))
;;  `(company-tooltip ((t (:background "#4f4f4f" :foreground "#dcdccc"))))
;;  `(company-tooltip-selection ((t (:background "#2b2b2b" :foreground "#dcdccc"))))
;;  `(company-tooltip-search ((t (:background "#333333" :foreground "red"))))
;;  `(company-tooltip-search-selection ((t (:background "#2b2b2b" :foreground "red" :weight bold))))
;;  `(company-tooltip-common ((t (:foreground "#9fc59f" :weight bold))))
;;  `(company-tooltip-common-selection ((t (:foreground "#9fc59f" :weight bold))))

;;  `(company-tooltip-annotation ((t (:background "#4f4f4f" :foreground "#dfaf8f"))))
;;  `(company-tooltip-annotation-selection ((t (:background "#2b2b2b" :foreground "#dfaf8f"))))

;;  `(company-scrollbar-fg ((t (:background "#2b2b2b"))))
;;  `(company-scrollbar-bg ((t (:background "#5f5f5f"))))


;;  `(company-template-field ((t (:background "dim gray" :foreground "dark red"))))

;;  `(company-preview ((t (:background "#9fc59f" ))))
;;  `(company-preview-common ((t (:background "#2b2b2b" :foreground "#9fc59f"))))
;;  `(company-preview-search ((t (:background "blue1"))))
;;  `(company-echo ((t (:background "#656555"))))
;;  `(company-echo-common ((t (:foreground "firebrick1")))))

;; (let ((bg (face-attribute 'default :background))
;;       (fg (face-attribute 'default :foreground)))
;;   (if (equal fg "unspecified-fg")
;;       (setq fg "#DCDCCC"))
;;   (if (equal bg `unspecified)
;;       (setq bg "#000000"))
;;   (if (equal bg "unspecified-bg")
;;       (setq bg "#000000"))
;;   (if (equal bg "#000000")
;;       (setq bg "#000000"))
;;   (message bg)
;;   (custom-set-faces
;;    `(hl-line ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 2)))))
;;    `(company-tooltip-selection ((t (:background ,(color-darken-name bg 10) :weight bold))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
;;    `(company-tooltip-search ((t (:background ,(color-darken-name bg 50) :foreground "red"))))
;;    `(company-tooltip-search-selection ((t (:background ,(color-darken-name bg 30) :weight bold))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 20)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-template-field ((t (:inherit font-lock-variable-name-face :background ,(color-lighten-name bg 5)))))))

(provide 'my-themes)
;;; my-themes.el ends here
