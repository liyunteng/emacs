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

(use-package rustic
  :ensure t
  :bind ((
          :map rustic-mode-map
          ("C-c u" . rustic-compile)
          ("C-c g" . rustic-recompile)
          ("C-c b" . rustic-cargo-build)
          ("C-c f" . rustic-cargo-fmt)
          ("C-c r" . rustic-cargo-run)
          ("C-c c" . rustic-cargo-clippy)
          ("C-c o" . rustic-cargo-outdated)
          ("C-c e" . rustic-cargo-clean)
          ("C-c k" . rustic-cargo-check)
          ("C-c t" . rustic-cargo-test)
          ("C-c d" . rustic-cargo-doc)

          ("C-c C-t" . rustic-cargo-current-test)
          ("C-c C-d" . rustic-racer-describe)
          ("M-:" . rustic-docstring-dwim)

          ("C-c x" . rustic-rustfix)
          ("C-c C-f" . rustic-format-file)
          ("C-c RET" . my/smart-compile)

          ("C-c C-c" . rustic-cargo-run)
          ("C-M-a" . rustic-beginning-of-defun)
          ("C-M-e" . rustic-end-of-defun)
          ))
  :config
  (add-hook 'rustic-cargo-run-mode-hook 'view-mode-enable)
  )

;; (use-package rust-mode
;;   :ensure t
;;   :bind ((
;;           :map rust-mode-map
;;           ("C-c C-c" . rust-run)
;;           ("C-c RET" . rust-compile)
;;           ("C-M-a" . rust-beginning-of-defun)
;;           ("C-M-e" . rust-end-of-defun)))
;;   :init
;;   ;; (when (or (executable-find "rust-analyzer") (executable-find "rls"))
;;   ;;   (add-hook 'rust-mode-hook 'lsp-deferred))

;;   :config
;;   ;; (setq rust-format-on-save t)
;;   )

(provide 'my-rust)
;;; my-rust.el ends here
