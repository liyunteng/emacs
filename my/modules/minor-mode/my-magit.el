;;; my-magit.el --- magit                            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: vc

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
(setq-default magit-branch-read-upstream-first 'fallback)
(setq-default magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
(when (eq window-system 'w32)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

;; fullscreen
;; (setq-default magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(provide 'my-magit)
;;; my-magit.el ends here
