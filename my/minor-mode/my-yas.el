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
  :bind (;; replace expand-abbrev
          ("C-x '" . yas-describe-tables))
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode-on)

  :config
  ;; (use-package dropdown-list
  ;; 	:ensure t)
  ;; (setq yas-snippet-dirs (list 'yas-installed-snippets-dir yas--default-user-snippets-dir))

  (setq yas-triggers-in-field t
  	yas-wrap-around-region t)
  ;; (setq yas-prompt-functions '(yas-completing-prompt))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  )

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(provide 'my-yas)
;;; my-yas.el ends here
