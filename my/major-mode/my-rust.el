;;; my-rust.el ---                                   -*- lexical-binding: t; -*-

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
(use-package flycheck-rust
  :ensure t)

(use-package rust-mode
  :ensure t
  :bind ((
          :map rust-mode-map
          ("C-c C-c" . rust-run)
          ("C-c RET" . rust-compile)
          ("C-c t" . rust-test)
          ("C-M-a" . rust-beginning-of-defun)
          ("C-M-e" . rust-end-of-defun)))
  :init
  ;; (when (or (executable-find "rust-analyzer") (executable-find "rls"))
  ;;   (add-hook 'rust-mode-hook 'lsp-deferred))

  :config
  ;; (setq rust-format-on-save t)
  )

(provide 'my-rust)
;;; my-rust.el ends here
