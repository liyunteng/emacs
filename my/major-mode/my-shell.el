;;; my-shell.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package sh-script
  :init
  (use-package company-shell
    :ensure t
    :defer t
    :commands (company-shell
	           company-shell-env))
  ;; auto-insert
  (define-auto-insert 'sh-mode (my-header '("#!/usr/bin/bash\n\n")))

  ;;  company
  (my|enable-company sh-mode '(company-shell company-shell-env)))

(provide 'my-shell)
;;; my-shell.el ends here
