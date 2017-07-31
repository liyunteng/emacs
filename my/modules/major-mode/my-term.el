;;; my-term.el --- term                -*- lexical-binding: t; -*-

;; Copyright (C) 2014  liyunteng

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
(my-require-package 'multi-term)
;; (autoload 'multi-term-next "multi-term" nil)

(after-load 'comint
  (setq comint-scroll-to-bottom-on-input nil)
  (setq comint-scroll-to-bottom-on-output nil)
  (setq comint-scroll-show-maximum-output nil)
  (setq comint-prompt-read-only t)
  )

(defadvice shell-command-on-region
    (after my-shell-command-in-view-mode
           (start end command &optional output-buffer &rest other-args)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

;; 退出gdb和term shell eshell时自动关闭buffer
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when 'shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))
                                (when (> (count-windows) 1)
                                  (delete-window))))))))
(dolist (hook '(term-mode-hook
                terminal-mode-hook
                shell-mode-hook
                eshell-mode-hook
                inferior-python-mode-hook
                comint-mode-hook
                ))
  (add-hook hook 'kill-buffer-when-shell-command-exit))


(defvar my-term-shell "/bin/bash")
(setq-default explicit-shell-file-name my-term-shell)
(setq-default term-input-ring-file-name
              (expand-file-name "term" my-cache-dir))

(setq-default multi-term-program my-term-shell)
;; (setq-default multi-term-program "/bin/bash")
;; (setq multi-term-dedicated-close-back-to-open-buffer-p t)
;; (setq multi-term-dedicated-select-after-open-p t)
;; (setq-default system-uses-terminfo t)
;; (setq-default term-buffer-maximum-size 0)
(setq-default multi-term-scroll-to-bottom-on-output nil)
(setq-default multi-term-scroll-show-maximum-output nil)
(setq-default multi-term-switch-after-close nil)

(after-load 'multi-term
  (defun ash-term-hooks ()
    "Ash term hooks."
    ;; dabbrev-expand in term
    (define-key term-raw-escape-map "/"
      (lambda ()
        (interactive)
        (let ((beg (point)))
          (dabbrev-expand nil)
          (kill-region beg (point)))
        (term-send-raw-string (substring-no-properties (current-kill 0)))))
    ;; yank in term (bound to C-c C-y)
    (define-key term-raw-escape-map "\C-y"
      (lambda ()
        (interactive)
        (term-send-raw-string (current-kill 0)))))
  (add-hook 'term-mode-hook 'ash-term-hooks)

  ;; (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  ;;   "Kill the buffer when terminal is exited."
  ;;   (if (memq (process-status proc) '(signal exit))
  ;;       (let ((buffer (process-buffer proc)))
  ;;         ad-do-it
  ;;         (kill-buffer buffer))
  ;;     ad-do-it))
  ;; (ad-activate 'term-sentinel)


  (defadvice ansi-term (before force-bash)
    "Always use bash."
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term)

  (defun my-term-use-utf8 ()
    "Use utf8."
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)

  (defun my/toggle-term-mode ()
    (interactive)
    (if (term-in-char-mode)
        (term-line-mode)
      (term-char-mode)))

  (add-hook 'term-mode-hook
            (lambda ()
              (my-mode -1)
              (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
              (setq-local mouse-yank-at-point t)
              (setq-local transient-mark-mode nil)
              (setq-local global-hl-line-mode nil)
              (auto-fill-mode nil)))

  (defvar my-multi-term-dedicated-old-buf nil)
  (defun my/multi-term-dedicated-toggle-and-select ()
    "My term dedicated toggle and select."
    (interactive)
    (if (multi-term-dedicated-exist-p)
        (progn
          (multi-term-dedicated-close)
          (switch-to-buffer my-multi-term-dedicated-old-buf))
      (progn
        (setq my-multi-term-dedicated-old-buf (current-buffer))
        (multi-term-dedicated-open)
        (multi-term-dedicated-select))))


  ;; (add-to-list 'term-bind-key-alist '("C-c C-a") 'term-bol)
  ;; (add-to-list 'term-bind-key-alist '("C-c C-m" . my/toggle-term-mode))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-q" . term-quit-subjob))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-z" . term-stop-subjob))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))
  ;; (add-to-list 'term-bind-key-alist '("C-l" . term-send-raw))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-p" . term-previous-prompt))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-n" . term-next-prompt))

  (add-to-list 'term-bind-key-alist '("C-c C-a" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-q" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-z" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-l" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-m" . my/toggle-term-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-p" . term-previous-prompt))
  (add-to-list 'term-bind-key-alist '("C-c C-n" . term-next-prompt))

  (add-to-list 'term-bind-key-alist '("C-x ." . multi-term-next))
  (add-to-list 'term-bind-key-alist '("C-x ," . multi-term-prev))

  (define-key term-mode-map (kbd "C-c C-m") 'my/toggle-term-mode)
  (define-key term-mode-map (kbd "C-l") 'term-send-raw)
  (define-key term-mode-map (kbd "C-c C-p") 'term-previous-prompt)
  (define-key term-mode-map (kbd "C-c C-n") 'term-next-prompt)
  )

(after-load 'eshell
  (setq-default eshell-cmpl-cycle-completions nil
                ;; auto truncate after 20k lines
                eshell-buffer-maximum-lines 20000
                ;; history size
                eshell-history-size 350
                ;; no duplicates in history
                eshell-hist-ignoredups t
                ;; buffer shorthand -> echo foo > #'buffer
                eshell-buffer-shorthand t
                ;; my prompt is easy enough to see
                eshell-highlight-prompt nil
                ;; treat 'echo' like shell echo
                eshell-plain-echo-behavior t

                eshell-send-direct-to-subprocesses t
                eshell-scroll-to-bottom-on-input nil
                eshell-scroll-to-bottom-on-output nil
                )
  (defun my--protect-eshell-prompt ()
    "Protect Eshell's prompt like Comint's prompts.

E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
    (let ((inhibit-field-text-motion t))
      (add-text-properties
       (point-at-bol)
       (point)
       '(rear-nonsticky t
                        inhibit-line-move-field-capture t
                        field output
                        read-only t
                        front-sticky (field inhibit-line-move-field-capture)))))

  (add-hook 'eshell-after-prompt-hook 'my--protect-eshell-prompt)
  (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

  (defun eshell/clear ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (eshell-send-input))
  (defun my--init-eshell ()
    "Stuff to do when enabling eshell."
    (setq-default pcomplete-cycle-completions nil)
    (if (bound-and-true-p linum-mode) (linum-mode -1))
    (when semantic-mode
      (semantic-mode -1))
    (when (boundp 'eshell-output-filter-functions)
      (push 'eshell-truncate-buffer eshell-output-filter-functions))

    ;; Caution! this will erase buffer's content at C-l
    (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)
    (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
    (setq-local global-hl-line-mode nil)
    )
  (add-hook 'eshell-mode-hook 'my--init-eshell)
  )

(provide 'my-term)
;;; my-term.el ends here