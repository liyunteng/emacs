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
;;   :ensure t)
(require 'color)

(use-package zenburn-theme
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package nzenburn-theme
  :ensure t
  :defer t)

(use-package monokai-theme
  :ensure t
  :defer t)

(defcustom my-theme 'zenburn
  "My theme."
  :type 'symbol
  :group 'my-config)

(defun my-load-stand-zenburn-theme ()
  (require 'zenburn-theme)
  (setq zenburn-override-colors-alist nil)
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   ;; change comment color
   ;; `(font-lock-comment-face ((t (:foreground "#75715E"))))
   `(font-lock-comment-face ((t (:foreground "grey50"))))
   `(font-lock-comment-delimiter-face ((t (:foreground "grey50"))))
   ;; change string color
   `(font-lock-string-face ((t (:foreground "#7F9F7F"))))
   `(font-lock-constant-face ((t (:foreground "#BFEBBF" :weight light))))
   ;; change function name color
   `(font-lock-function-name-face ((t (:foreground "#93E0E3" :weight bold))))
   `(company-tooltip-search
     ((t (:background
          ,(color-darken-name (face-attribute 'default :background) 60)
          :foreground "red"))))
   `(company-tooltip-search-selection
     ((t (:background
          ,(color-darken-name (face-attribute 'default :background) 60)
          :foreground "red" :weight bold))))
   `(company-template-field
     ((t (:background
          ,(color-darken-name (face-attribute 'default :background) 30)))))))

;; (set-face-attribute 'dired-directory nil
;;                     :foreground "#93E0E3"
;;                     :weight 'bold
;;                     :inherit 'unspecified
;;                     :slant 'italic
;;                     :box 'unspecified
;;                     :overline nil
;;                     :underline nil
;;                     :strike-through nil
;;                     )

(defun my-load-tty-zenburn-theme ()
  "Enable modified zenburn theme in tty mode."
  (message "####################Emacs in 8 color####################")
  (require 'zenburn-theme)
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
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(company-tooltip-search
     ((t (:background "red"))))
   `(company-tooltip-search-selection
     ((t (:background "red" :weight bold))))
   `(company-tooltip-annotation-selection
     ((t (:foreground "#00FFFF" :weight bold))))
   `(company-template-field
     ((t (:background "#00FFFF"))))
   `(helm-ff-dotted-directory
     ((t (:foreground "#00FFFF" :background "#000000"))))))

(defun my/load-theme (&optional frame)
  (interactive)
  (when frame (select-frame frame))
  (if (equal my-theme 'zenburn)
      (if (<= (display-color-cells) 8)
          (my-load-tty-zenburn-theme)
        (my-load-stand-zenburn-theme))
    (load-them my-theme t)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/load-theme)
  (my/load-theme))

(provide 'my-themes)
;;; my-themes.el ends here
