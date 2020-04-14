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

(defhydra hydra-main (:color teal :hint nil)
  "
    Main helper

    Org. related          Help                Zooming        bookmarks          other helpers     completion
    ------------------------------------------------------------------------------------------------------------------
    _c_: org-capture        _f_: function doc.    _+_: zoom in     _b_: list bookmarks  _p_: prodigy        _y_: ivy-yasnippet
    _g_: org-web-get-url    _v_: variable doc.    _-_: zoom out    _B_: bookmark file   _m_: new mail

    Backups
    ------------------------------------------------------------------------------------------------------------------
    _s_: list backups
    _S_: snapshot timemachine

    "
  ("B" bookmark-set)
  ("b" list-bookmarks)
  ("c" counsel-org-capture)
  ("f" describe-function)
  ("g" org-web-tools-read-url-as-org)
  ("m" mu4e-compose-new)
  ("p" prodigy)
  ("s" snapshot-timeline)
  ("S" snapshot-timemachine)
  ("v" describe-variable)
  ("y" ivy-yasnippet)
  ("+" text-scale-increase :color pink)
  ("-" text-scale-decrease :color pink)
  ("<ESC>" nil "quit" :color blue)
  ("q"   nil "cancel" :color blue))

(defhydra hydra-project (:color teal :hint nil)
  "
         Project/Source management

     Projects              Version control        On-the-fly
    ------------------------------------------------------------------------------------------
    _d_: dash projects     _m_: magit             _f_: fixme listing
    _p_: projectile        _t_: travis status     _F_: flycheck

    "
  ("<ESC>" nil "quit")
  ("d"   org-dashboard-display)
  ("p"   hydra-projectile/body)
  ("f"   fic-view-listing)
  ("F"   hydra-flycheck/body)
  ("m"   hydra-magit/body)
  ("t"   show-my-travis-projects)
  ("q"   nil "cancel" :color blue))
(global-set-key (kbd "<f4>") 'hydra-project/body)


(defhydra my/search-hydra ())
(require 'hydra-examples)

(provide 'my-hydra)
;;; my-hydra.el ends here
