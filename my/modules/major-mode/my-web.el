;;; my-web.el --- web-mode                           -*- lexical-binding: t; -*-

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

(use-package web-mode
  :ensure t
  :mode (("\\.xml\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
	 ("\\.blade\\.php\\'" . web-mode)
	 ("\\.jsp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
	 ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
	 )

  :config
  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil
	web-mode-indent-style 4)

  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
		   :unless '(sp-in-string-p)
		   :post-handlers '(((lambda (&rest _ignored)
				       (just-one-space)
				       (save-excursion (insert " ")))
				     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))
  )

(provide 'my-web)
;;; my-web.el ends here
