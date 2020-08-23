;;; my-qt.el --- my qt                               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

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

(require 'semantic)
(require 'semantic/bovine/c)

;; (setenv "QTDIR" "/opt/Qt5.3.2/5.3/gcc_64")
;; (setq qt-base-directory "/opt/Qt5.3.2/5.3/gcc_64")
(setq qt-include-directory "/usr/include/qt")
;; (setenv "PATH" (concat (concat (getenv "QTDIR") "bin" ) "; " (getenv "PATH")))
;; (setenv "PATH" (concat (concat qt-base-directory "mingw\\bin") ";" (getenv "PATH")))
;; (setenv "PATH" (concat (concat qt-base-directory "Desktop\\Qt\\4.7.3\\mingw\\lib")
;; ";" (getenv "PATH")))



;; (setq
;; qt-source-directory (expand-file-name "QtSources/4.7.3/src" qt-base-directory)
;; qt-include-directory (expand-file-name
;; "Desktop/Qt/4.7.3/mingw/include/" qt-base-directory))
;; (add-to-list 'auto-mode-alist (cons qt-source-directory 'c++-mode))
;; (add-to-list 'cc-search-directories qt-source-directory))

(add-to-list 'auto-mode-alist (cons qt-include-directory 'c++-mode))
(dolist (file (directory-files qt-include-directory))
  (let ((path (expand-file-name file qt-include-directory)))
    (when (and (file-directory-p path)
            (not (or (equal file ".") (equal file ".."))))
      (progn
        (semantic-add-system-include path 'c++-mode)
        (semantic-add-system-include path 'c-mode)
        (add-to-list 'semantic-c-dependency-system-include-path path)
        ;; (add-to-list 'cc-search-directories path)
        ))))

(semantic-add-system-include qt-include-directory 'c++-mode)
(semantic-add-system-include qt-include-directory 'c-mode)
;; (add-to-list 'cc-search-directories qt-include-directory)
;; (add-to-list 'semantic-c-dependency-system-include-path qt-include-directory)


(dolist (file (list "QtCore/qconfig.h" "QtCore/qconfig-dist.h"
                "QtCore/qconfig-large.h" "QtCore/qconfig-nacl.h"
                "QtCore/qconfig-medium.h" "QtCore/qconfig-minimal.h"
                "QtCore/qconfig-small.h" "QtCore/qglobal.h"
                "QtCore/qglobalstatic.h"
                "Gentoo/gentoo-qconfig.h" "Gentoo/qtdbus-qconfig.h"
                "Gentoo/qtgui-qconfig.h" "Gentoo/qtnetwork-qconfig.h"
                "Gentoo/qtprintsupport-qconfig.h"
                ))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (expand-file-name file qt-include-directory)))



;; (dolist (dirs (list
;;                (concat "-I" qt-include-directory)
;;                (concat "-I" qt-include-directory "/QtCore")
;;                (concat "-I" qt-include-directory "/QtWidgets")
;;                (concat "-I" qt-include-directory "/QtGui")
;;                ))
;;   (add-to-list 'ac-clang-flags dirs))

;; (dolist (dirs (list (concat "-I" qt-include-directory)
;;                     (concat "-I" qt-include-directory "/QtCore")
;;                     (concat "-I" qt-include-directory "/QtWidgets")
;;                     (concat "-I" qt-include-directory "/QtGui")))
;;   (add-to-list 'flycheck-gcc-include-path dirs))


(c-add-style "qt-gnu" '("gnu"
                         (c-access-key .
                           "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
                         (c-basic-offset . 4)))

;; syntax-highlighting for Qt
;; (based on work by Arndt Gulbrandsen, Troll Tech)
(defun jk/c-mode-common-hook ()
  "Set up C-MODE and related modes.
Includes support for Qt code (signal, slots and alikes)."

  ;; base-style
  ;; (c-set-style "stroustrup")
  ;; set auto cr mode
  ;; (c-toggle-auto-hungry-state 1)

  ;; qt keywords and stuff ...
  ;; set up indenting correctly for new qt kewords
  (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                           "\\|protected slot\\|private\\|private slot"
                           "\\)\\>")
    c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                       "\\|public slots\\|protected slots\\|private slots"
                       "\\)\\>[ \t]*:"))
  (progn
    ;; modify the colour of slots to match public, private, etc ...
    (font-lock-add-keywords 'c++-mode
      '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
    ;; make new font for rest of qt keywords
    (make-face 'qt-keywords-face)
    (set-face-foreground 'qt-keywords-face "BlueViolet")
    ;; qt keywords
    (font-lock-add-keywords 'c++-mode
      '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
      '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
    (font-lock-add-keywords 'c++-mode
      '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
    ))
(add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)


;; Other things I like are, for example,

;; automatic indent on return in cc-mode
;; (define-key c-mode-base-map (kbd "RETURN") 'newline-and-indent)

;; Do not check for old-style (K&R) function declarations;
;; this speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Switch fromm *.<impl> to *.<head> and vice versa
(defun switch-cc-to-h ()
  "Switch cc to h."
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
           (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp")
              (cond ((file-exists-p (concat name ".h"))
                      (find-file (concat name ".h"))
                      )
                ((file-exists-p (concat name ".hh"))
                  (find-file (concat name ".hh"))
                  )
                ))
        ((string-match suffix "h\\|hh")
          (cond ((file-exists-p (concat name ".cc"))
                  (find-file (concat name ".cc"))
                  )
            ((file-exists-p (concat name ".C"))
              (find-file (concat name ".C"))
              )
            ((file-exists-p (concat name ".cpp"))
              (find-file (concat name ".cpp"))
              )
            ((file-exists-p (concat name ".c"))
              (find-file (concat name ".c"))
              )))))))

(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public
slots\\|public signals\\|private slots\\|private signals\\|protected
slots\\|protected signals\\)\\>" . font-lock-constant-face)))

(provide 'my-qt)
;;; my-qt.el ends here
