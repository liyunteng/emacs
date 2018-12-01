;;; my-flyspell.el --- flyspell                      -*- lexical-binding: t; -*-

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

(use-package flyspell
  :defer t
  :diminish "flyspell"
  :ensure t
  :config
  (use-package flyspell-correct
    :ensure t
    :init
    (flyspell-correct-auto-mode t))

  (when (boundp 'helm-mode)
    (use-package flyspell-correct-helm
      :ensure t
      :bind (:map flyspell-mode-map
                  ("C-;" . flyspell-correct-wrapper))
      ))

  ;; ;; better performance
  (setq flyspell-issue-message-flag nil)
  )


(provide 'my-flyspell)
;;; my-flyspell.el ends here
