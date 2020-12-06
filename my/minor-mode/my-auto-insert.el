;;; my-auto-insert.el --- auto-insert

;; Copyright (C) 2014  liyunteng

;; Author: liyunteng <li_yunteng@163.com>
;; Keywords: lisp, autoinsert

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

(defconst gpl-license-content
  "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.")

(defconst bsd-license-content
  "Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.")

(cl-defstruct my-auto-insert-header
  "my auto insert header struct."
  (short-description t :type bool)
  (license nil :type string)
  (copyright nil :type string)
  (author (concat (user-full-name) " <" user-mail-address ">" ) :type string)
  (time t :type bool)
  (update-time t :type bool)
  (license-content nil :type string))

(defvar test-header (make-my-auto-insert-header
                     :license "GPL-2.0"
                     :copyright (concat (format-time-string "%Y ")
                                        " Yunteng Li <li_yunteng@163.com>")
                     :author "liyunteng"
                     :license-content gpl-license-content))

(defvar streamocean-header (make-my-auto-insert-header
                            :author (concat (user-full-name) " <liyunteng@streamocean.com>")
                            :copyright (concat (format-time-string "%Y")
                                               " StreamOcean, Inc. All rights reserved.")
                            :update-time nil)
  "streamocean auto-insert header.")

(defvar mega-header (make-my-auto-insert-header
                     :author (concat (user-full-name) " <liyunteng@megarobo.tech>")
                     :copyright (concat (format-time-string "%Y")
                                        " Megarobo, Inc. All rights reserved.")))

(defvar addx-header (make-my-auto-insert-header
                     :short-description nil
                     :author (concat "Yunteng Li <yli@addx.ai>")
                     :copyright (concat (format-time-string "%Y")
                                        " Addx, Inc. All rights reserved.")
                     :time t
                     :update-time nil
                     ))

(defvar gpl-header (make-my-auto-insert-header
                    ;; :license "GPL-2.0"
                    :short-description nil
                    :license nil
                    :author nil
                    :copyright (concat (format-time-string "%Y")
                                       " Yunteng Li <li_yunteng@163.com>")
                    :license-content gpl-license-content
                    :update-time nil)
  "gpl auto-insert header.")

(defvar bsd-header (make-my-auto-insert-header
                    :short-description nil
                    :license nil
                    :author nil
                    :copyright (concat (format-time-string "%Y")
                                       " Yunteng Li <li_yunteng@163.com>"
                                       )
                    :license-content bsd-license-content
                    :update-time nil))

(defvar null-header (make-my-auto-insert-header
                     :short-description nil
                     :license nil
                     :copyright nil
                     :license-content nil
                     :author nil
                     :update-time nil
                     :time nil)
  "null auto-insert header.")

(defvar my-header (make-my-auto-insert-header
                   :license nil
                   :author nil
                   ;; :copyright (concat (format-time-string "%Y") " "(user-full-name) " " user-mail-address)
                   :update-time nil)
  "my auto-insert header")

(defcustom my-auto-insert-header-alist '(null-header
                                         test-header
                                         my-header
                                         streamocean-header
                                         mega-header
                                         addx-header
                                         gpl-header
                                         bsd-header)
  "My auto insert headers."
  :type 'list
  :group 'my-config)

(defcustom auto-insert-header 'null-header
  "Auto insert used license."
  ;; :type 'my-auto-insert-header
  :type 'symbol
  :group 'my-config)
;; (setq auto-insert-header streamocean-header)
;; (setq auto-insert-header 'mega-header)
(setq auto-insert-header 'my-header)

(use-package autoinsert
  :commands (auto-insert auto-insert-mode)
  :init
  ;; don't auto-insert to custom.el
  (defadvice auto-insert (around check-custom-file-auto-insert activate)
    (when custom-file
      (if (not (equal (buffer-file-name) (file-truename custom-file)))
	      ad-do-it)))

  (defun my/auto-insert-select-header (&optional h)
    (interactive)
    (let ((p (completing-read "which header: "
                              my-auto-insert-header-alist)))
      (setq auto-insert-header (intern p))
      (setq this-command 'auto-insert)
      (funcall-interactively #'auto-insert)))

  (auto-insert-mode t)

  :config
  (setq auto-insert-query nil)
  ;; (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  ;; (define-auto-insert "\.c" "c-temp.c")

  (use-package time-stamp
    :init
    (setq time-stamp-line-limit 15)
    (setq time-stamp-start "Last-Updated:[ \t]+\\\\?[\"<]+")
    ;; (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S %U")
    (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S")
    (defun my/update-time-stamp ()
      (when time-stamp-active (time-stamp)))
    (add-hook 'write-file-functions #'my/update-time-stamp))
  (require 'time-stamp)

  (defun my-header (&optional prefix postfix)
    "My header with PREFIX and POSTFIX."
    (append
     '('(goto-char (point-min)))
     (if prefix prefix)
     '('(setq-local auto-insert--begin (point))
       '(setq-local auto-insert-header-value (symbol-value auto-insert-header))
       (when (my-auto-insert-header-copyright auto-insert-header-value)
         (concat "Copyright (C) " (my-auto-insert-header-copyright auto-insert-header-value) "\n\n"))

       ;; "Description: " (read-string "Description: ") "\n\n"
       (when (my-auto-insert-header-short-description auto-insert-header-value)
         (concat (file-name-nondirectory (buffer-file-name)) " - " (file-name-base (buffer-file-name)) "\n\n"))

       (when  (my-auto-insert-header-author auto-insert-header-value)
         (concat "Author : " (my-auto-insert-header-author auto-insert-header-value) "\n"))

       (when (my-auto-insert-header-time auto-insert-header-value)
         (concat "Date   : " (format-time-string "%Y/%m/%d") "\n"))

       (when (my-auto-insert-header-license auto-insert-header-value)
         (concat "License: " (my-auto-insert-header-license auto-insert-header-value) "\n"))

       ;; (when (my-auto-insert-header-copyright auto-insert-header-value)
       ;;   (concat "\nCopyright (C) " (my-auto-insert-header-copyright auto-insert-header-value) "\n"))

       (when (my-auto-insert-header-update-time auto-insert-header-value)
         (concat "Last-Updated: <>\n"))

       (when (my-auto-insert-header-license-content auto-insert-header-value)
         (concat "\n" (my-auto-insert-header-license-content auto-insert-header-value) "\n"))

       (if (not (equal (point) (point-min)))
           (let ((comment-style-origin comment-style))
             (setq comment-style 'extra-line)
             (comment-region auto-insert--begin (point))
             (setq comment-style comment-style-origin)
             nil)))
     (if postfix postfix)))
  )


(provide 'my-auto-insert)
;;; my-auto-insert.el ends here
