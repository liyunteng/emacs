;;; my-yas.el --- yas

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

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-minor-mode-on)
  :ensure t
  :defer t
  :bind (
         ;; replace expand-abbrev
         ("C-x '" . yas-expand)
         )
  :init
  (my|add-toggle yas-minor-mode
    :mode yas-minor-mode
    :documentation "yas minor mode")


  (use-package yasnippet-snippets
    :ensure t
    :defer t)

  :config
  ;; (dolist (hook '(prog-mode-hook
  ;;       	  latex-mode-hook
  ;;       	  plain-text-mode
  ;;       	  nxml-mode-hook
  ;;       	  html-mode-hook
  ;;       	  web-mode

  ;;       	  autoconf-mode-hook
  ;;       	  conf-unix-mode-hook
  ;;       	  cmake-mode-hook
  ;;       	  css-mode-hook
  ;;       	  m4-mode-hook
  ;;       	  ;; org-mode-hook
  ;;       	  makefile-gmake-mode-hook
  ;;       	  sql-mode-hook
  ;;       	  snippet-mode-hook
  ;;       	  udev-mode-hook
  ;;       	  ))
  ;;   (add-hook hook 'yas-minor-mode-on))

  ;; (use-package dropdown-list
  ;; 	:ensure t)
  ;; (setq yas-snippet-dirs (list 'yas-installed-snippets-dir yas--default-user-snippets-dir))

  (setq yas-triggers-in-field t
  	    yas-wrap-around-region t)
  ;; (setq yas-prompt-functions '(yas-completing-prompt))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  (yas-global-mode +1)
  ;c; (add-hook 'after-init-hook 'yas-reload-all)

  )

(provide 'my-yas)
;;; my-yas.el ends here
