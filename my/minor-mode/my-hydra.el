;;; my-hydra.el --- hydra                            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  liyunteng

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defhydra my/rectangle-hydra (:body-pre (rectangle-mark-mode 1)
                                        :color pink
                                        :post (deactivate-mark))
  "
  ^_k_^        _d_elete       _s_tring
_h_  _l_       _o_k           _y_ank
 ^_j_^         _n_ew-copy     _r_eset
^^^^           _e_xchange     _u_ndo
^^^^           ^ ^            _p_aste
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil)
  )

(defhydra my/search-hydra (:hint none :color pink)
  "
_f_orward
_b_ackward                     _q_: quit"
  ("f" isearch-forward nil)
  ("b" isearch-backward nil)
  ("q" nil nil)
  )


(defhydra my/search-hydra ())
(require 'hydra-examples)

(provide 'my-hydra)
;;; my-hydra.el ends here
