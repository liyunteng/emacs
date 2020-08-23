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

(cl-defstruct my-auto-insert-header
  "my auto insert header struct."
  (short-description t :type bool)
  (license nil :type string)
  (copyright nil :type string)
  (author (concat (user-full-name) " <" user-mail-address ">" ) :type string)
  (time t :type bool)
  (update-time t :type bool)
  (license-content nil :type string))

(defvar streamocean-header (make-my-auto-insert-header
                             :author (concat (user-full-name) " <liyunteng@streamocean.com>")
                             :copyright (concat (format-time-string "%Y")
                                          " StreamOcean, Inc."
                                          "\n"
                                          "All rights reserved.")
                             :update-time nil)
  "streamocean auto-insert header.")
(defvar mega-header (make-my-auto-insert-header
                      :author (concat (user-full-name) " <liyunteng@megarobo.tech>")
                      :copyright (concat (format-time-string "%Y")
                                   " Megarobo, Inc."
                                   "\n"
                                   "All rights reserved.")))

(defvar gpl-header (make-my-auto-insert-header
                     :license "GPL-2.0"
                     :copyright nil
                     :license-content gpl-license-content
                     :update-time nil)
  "gpl auto-insert header.")

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
                                          my-header
                                          streamocean-header
                                          mega-header
                                          gpl-header)
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
  (auto-insert-mode t)

  ;; don't auto-insert to custom.el
  (defadvice auto-insert (around check-custom-file-auto-insert activate)
    (when custom-file
      (if (not (equal (buffer-file-name) custom-file))
	      ad-do-it)))
  :config
  (use-package time-stamp
    :config
    (defun my/update-time-stamp ()
      (when time-stamp-active
	      (time-stamp)))
    (setq time-stamp-line-limit 15)
    (setq time-stamp-start "Last-Updated:[ \t]+\\\\?[\"<]+")
    ;; (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S %U")
    (setq time-stamp-format "%04Y/%02m/%02d %02H:%02M:%02S")
    (add-hook 'write-file-functions #'my/update-time-stamp))

  ;; (setq auto-insert t)
  (setq auto-insert-query nil)
  ;; (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  ;; (define-auto-insert "\.c" "c-temp.c")
  (defun my-header (&optional prefix postfix)
    "My header with PREFIX and POSTFIX."
    (append
      '('(goto-char (point-min)))
      (if prefix prefix)
      '('(setq-local auto-insert--begin (point))
         '(setq-local auto-insert-header-value (symbol-value auto-insert-header))
         ;; "Description: " (read-string "Description: ") "\n\n"
         (when (my-auto-insert-header-short-description auto-insert-header-value)
           (concat (file-name-nondirectory (buffer-file-name)) " - " (file-name-base (buffer-file-name)) "\n\n"))

         (when  (my-auto-insert-header-author auto-insert-header-value)
           (concat "Author : " (my-auto-insert-header-author auto-insert-header-value) "\n"))

         (when (my-auto-insert-header-time auto-insert-header-value)
           (concat "Date   : " (format-time-string "%Y/%m/%d") "\n"))

         (when (my-auto-insert-header-license auto-insert-header-value)
           (concat "License: " (my-auto-insert-header-license auto-insert-header-value) "\n"))

         (when (my-auto-insert-header-copyright auto-insert-header-value)
           (concat "\nCopyright (C) " (my-auto-insert-header-copyright auto-insert-header-value) "\n"))

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
      (if postfix postfix)
      ))

  (define-auto-insert 'sh-mode (my-header '("#!/usr/bin/bash\n")))
  (define-auto-insert 'python-mode
    (my-header '("#!/usr/bin/env python\n" "# -*- coding: utf-8 -*-\n\n")))
  (define-auto-insert 'go-mode (my-header))
  (define-auto-insert '("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header") (my-header))
  (define-auto-insert '("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program") (my-header))

  (defun my/auto-insert-select-header (&optional h)
    (interactive)
    (let ((p (completing-read "which header: "
               my-auto-insert-header-alist)))
      (setq auto-insert-header (intern p)))
    (funcall-interactively #'auto-insert))
  )


(provide 'my-auto-insert)
;;; my-auto-insert.el ends here
