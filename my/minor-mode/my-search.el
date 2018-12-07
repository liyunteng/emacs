;;; my-search.el --- my search                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  liyunteng

;; Author: liyunteng;; Show number of matches while searching <li_yunteng@163.com>
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
;;; grep 默认递归查找
(use-package grep
  :commands (grep-mode
             grep
             grep-find
             find-grep
             lgrep
             rgrep
             zrgrep
             rzgrep)
  :bind (("M-s /" . find-grep)
         ("M-s g" . grep))
  :config
  (setq grep-command "grep --color --exclude=\"archive-contents\" -nHE -r -e "
        grep-highlight-matches t
        grep-scroll-output t
        ))

(use-package isearch
  :bind  (
	      :map isearch-mode-map
	      ("C-o" . isearch-occur)
	      ("C-q" . isearch-del-char)
	      ;; ("C-w" . isearch-delete-char)
	      ("C-w" . my/isearch-backward-kill-word)

	      ("C-y" . isearch-yank-word-or-char)
	      ("M-l" . isearch-yank-line)
	      ("M-w" . isearch-yank-kill)
	      ("C-M-w" . my/isearch-yank-symbol)
	      ("C-M-y" . isearch-yank-pop)

	      ("C-e" . isearch-edit-string)

	      ("M-i" . isearch-toggle-case-fold)
	      ("M-r" . isearch-toggle-regexp)

	      ([(control return)] . my/isearch-exit-other-end)
	      )

  :config
  ;; backward kill word in isearch
  (defun my--drop-until-backwards (re string)
    (let ((match (string-match re (string-reverse string))))
      (if match (substring string 0 (- (length string) match 1)) "")))

  (defun my/isearch-backward-kill-word ()
    (interactive)
    (if (null (cdr isearch-cmds))
        (ding)
      (let* ((current-state (car isearch-cmds))
             (current-string (isearch--state-string current-state))
             (new-string (my--drop-until-backwards "[[:word:]][^[:word:]]" current-string)))
        (while (not (string-equal current-string new-string))
          (setf isearch-cmds (cdr isearch-cmds)
                current-state (car isearch-cmds)
                current-string (isearch--state-string current-state)))
        (isearch--set-state (car isearch-cmds))))
    (isearch-update))

  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun my/isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (if sym
          (progn
            (setq isearch-regexp t
                  isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
                  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
	    (ding)))
    (isearch-search-and-update))

  ;; http://www.emacswiki.org/emacs/ZapToISearch
  (defun my/isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  )
;; need install the_silver_searcher
(when (executable-find "ag")
  (use-package ag
    :ensure t
    :bind (("M-s a" . ag)
           ("M-s d" . ag-dired))
    :config
    (use-package wgrep-ag :ensure t)
    (setq ag-highlight-search t)))

(use-package anzu
  :ensure t
  :bind (([remap query-replace-regexp]. anzu-query-replace-regexp)
	     ([remap query-replace] . anzu-query-replace)
	     ("M-s r" . anzu-query-replace)
	     ("M-s M-r" . anzu-query-replace-regexp))
  :diminish anzu-mode
  :init
  (defun my-anzu--update-mode-line (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s) "
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replace) " total))
                      (replace (format "(%d/%d) " here total)))))
	    status)))

  (setq anzu-mode-line-update-function 'my-anzu--update-mode-line)
  (global-anzu-mode +1))


;; highlight
(use-package hi-lock
  :commands (hi-lock-mode global-hi-lock-mode)
  :diminish hi-lock-mode
  :bind (("M-s h l" . highlight-lines-matching-regexp)
         ("M-s h f" . hi-lock-find-patterns)
         ("M-s h r" . highlight-regexp)
         ("M-s h p" . highlight-phrase)
         ("M-s h ." . highlight-symbol-at-point)
         ("M-s h u" . unhighlight-regexp)
         ("M-s h b" . hi-lock-write-interactive-patterns)
         ))


(provide 'my-search)
;;; my-arch.el ends here
