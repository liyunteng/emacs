;;; my-flycheck.el --- flycheck                      -*- lexical-binding: t; -*-

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

(use-package flycheck
  :ensure t
  :if (fboundp 'global-flycheck-mode)
  :init
  (defvar syntax-checking-use-original-bitmaps t)
  (when (and (fboundp 'define-fringe-bitmap)
			 (not syntax-checking-use-original-bitmaps))
	(define-fringe-bitmap 'my-flycheck-fringe-indicator
	  (vector #b00000000
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00011100
			  #b00111110
			  #b00111110
			  #b00111110
			  #b00011100
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00000000
			  #b00000000)))
  (let ((bitmap (if syntax-checking-use-original-bitmaps
					'flycheck-fringe-bitmap-double-arrow
				  'my-flycheck-fringe-indicator)))
	(flycheck-define-error-level 'error
	  :severity 2
	  :overlay-category 'flycheck-error-overlay
	  :fringe-bitmap bitmap
	  :fringe-face 'flycheck-fringe-error)
	(flycheck-define-error-level 'warning
	  :severity 1
	  :overlay-category 'flycheck-warning-overlay
	  :fringe-bitmap bitmap
	  :fringe-face 'flycheck-fringe-warning)
	(flycheck-define-error-level 'info
	  :severity 0
	  :overlay-category 'flycheck-info-overlay
	  :fringe-bitmap bitmap
	  :fringe-face 'flycheck-fringe-info))


  (use-package flycheck-package
	:ensure t
	:init
	(flycheck-package-setup))
  (add-hook 'prog-mode-hook 'flycheck-mode)

  :config
  (when (display-graphic-p)
	(use-package flycheck-pos-tip
	  :ensure t
	  :config
	  (after-load 'flycheck
		(flycheck-pos-tip-mode 1))))

  (when (boundp 'popwin:special-display-config)
	(push '("^\\*Flycheck.+\\*$"
			:regexp t
			:dedicated t
			:position bottom
			:stick t
			:noselect t)
		  popwin:special-display-config))

  (global-flycheck-mode +1)
  )


;;; Code:



(provide 'my-flycheck)
;;; my-flycheck.el ends here
