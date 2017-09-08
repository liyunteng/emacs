;;; My-auto-complete.el --- my auto complete

;; Copyright (C) 2014  liyunteng

;; Author: liyunteng(require-package 'auto-complete) <li_yunteng@163.com>
;; Keywords: lisp, comm

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

(use-package auto-complete)
(use-package popup)
(use-package ac-c-headers)
(use-package auto-complete-clang)
(use-package ac-dabbrev)
(use-package pos-tip)
(use-package fuzzy)
;;(use-package auto-complete-clang-async)
;;(use-package ac-etags)
(use-package go-autocomplete)
(use-package go-eldoc)

(require 'auto-complete-config)
(require 'popup)
(require 'pos-tip)
(require 'ac-c-headers)
(require 'auto-complete-clang)
(require 'ac-dabbrev)
;;(ac-etags-ac-setup)
(require 'go-autocomplete)
(require 'go-eldoc)

(ac-config-default)
(setq-default ac-use-quick-help t)
(setq-default ac-quick-help-delay 0.3)
(setq-default ac-trigger-commands
              (cons 'backward-delete-char-untabify ac-trigger-commands))
(setq-default ac-quick-help-prefer-pos-tip t)
(setq-default ac-expand-on-auto-complete t)
(setq-default ac-auto-start nil)
(setq-default ac-dwim t) ; To get pop-ups with docs even if a word is
                                        ; uniquely completed
(setq-default ac-dabbrev-sort t)
(setq-default ac-inline nil)

(global-auto-complete-mode nil)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq-default tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; (add-to-list 'completion-styles 'substring t)
(setq-default completion-cycle-threshold 5)

;; TODO: find solution for php, haskell and other modes where TAB always does something

(setq-default c-tab-always-indent nil c-insert-tab-function 'indent-for-tab-command)

;; hook AC into completion-at-point
(defun my-auto-complete-at-point ()
  "Used by 'set-auto-complete-as-completion-at-point-function'."
  (when (and (not (minibufferp))
             (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (auto-complete)))

;; (defun my/never-indent ()
;;   "Not used."
;;   (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun my-set-auto-complete-as-completion-at-point-function ()
  "Set auto complete as completion at point hook."
  (setq completion-at-point-functions
        (cons 'my-auto-complete-at-point
              (remove 'my-auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook
          'my-set-auto-complete-as-completion-at-point-function)

(defun my-gcc-include (dir)
  "Return gcc include +DIR."
  (let ((gcc-include-prefix (shell-command-to-string "gcc -print-search-dirs|sed -n 1p|awk -F : '{print $2}'|tr -d [:space:]")))
    (concat "-I"
            gcc-include-prefix
            dir
            )))

(setq-default ac-clang-flags
              (list
               (my-gcc-include "include/g++-v4")
               (my-gcc-include "include/g++-v4/x86_64-pc-linux-gnu")
               (my-gcc-include "include/g++-v4/backward")
               (my-gcc-include "include")
               (my-gcc-include "include-fixed")
               "-I/usr/include"
			   "-I/usr/local/include"
               "-I/usr/src/linux/include"
               "-I/usr/src/linux/arch/x86/include"
               ))

(set-default 'ac-sources
             '(
               ac-source-yasnippet
               ac-source-semantic
               ;; ac-source-semantic-raw
               ac-source-features
               ac-source-emacs-lisp-features
               ;; ac-source-gtags
               ;; ac-source-symbols
               ;; ac-source-functions
               ;; ac-source-abbrev
               ;; ac-source-imenu
               ac-source-dabbrev
               ;; ac-source-dictionary
               ;; ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ;; ac-source-words-in-all-buffer
               ))
(dolist (mode '(magit-log-edit-mode
                log-edit-mode
                org-mode
                text-mode
                haml-mode
                git-commit-mode
                sass-mode
                yaml-mode
                csv-mode
                espresso-mode
                haskell-mode
                html-mode
                nxml-mode
                sh-mode
                smarty-mode
                clojure-mode
                emacs-lisp-mode
                lisp-mode
                lisp-interaction-mode
                textile-mode
                markdown-mode
                tuareg-mode
                js3-mode
                css-mode
                less-css-mode
                sql-mode
                sql-interactive-mode
                c-mode
                makefile-gmake-mode
                c++-mode
                python-mode
                py-python-shell-mode
                inferior-emacs-lisp-mode
                go-mode
                ))
  (add-to-list 'ac-modes mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-clang)
            ;; (add-to-list 'ac-sources 'ac-source-c-headers)
            ;;(add-to-list 'ac-sources 'ac-source-c-header-symbols t)
            ;; (add-to-list 'ac-sources 'ac-source-semantic)
            ;; (add-to-list 'ac-sources 'ac-source-semantic-raw)
            ))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'go-mode-hook (lambda ()
                          (add-to-list 'ac-sources 'ac-source-go)))

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Exclude very large buffers from dabbrev
(defun my/dabbrev-friend-buffer (other-buffer)
  "Exclude very large 'OTHER-BUFFER' from dabbrev."
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'my/dabbrev-friend-buffer)

(provide 'my-auto-complete)
;;; my-auto-complete.el ends here
