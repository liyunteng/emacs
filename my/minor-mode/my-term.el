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

;; (autoload 'multi-term-next "multi-term" nil)

(defadvice shell-command-on-region
    (after my-shell-command-in-view-mode
           (start end command &optional output-buffer &rest other-args)
           activate)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless output-buffer
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))

;; close buffer when process exit
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when 'shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))
                                (when (> (count-windows) 1)
                                  (delete-window))
                                ))))))

(dolist (hook '(term-mode-hook
                eshell-mode-hook
                comint-mode-hook
                ;; comint-exec-hook
                shell-mode-hook
                inferior-python-mode-hook
                ))
  (add-hook hook 'kill-buffer-when-shell-command-exit))


(defvar my-term-shell (or (executable-find "zsh") (executable-find "bash")))
(setq-default explicit-shell-file-name my-term-shell)
(setq-default term-input-ring-file-name
              (expand-file-name "term" my-cache-dir))

(use-package comint
  :config
  (setq comint-scroll-to-bottom-on-input t
	    comint-scroll-to-bottom-on-output t
	    comint-scroll-show-maximum-output nil
	    comint-prompt-read-only t
	    comint-move-point-for-output t
	    )

  ;; (setq comint-input-sender
  ;;       (lambda (proc command)
  ;;         (cond
  ;;          ;; Check for clear command and execute it.
  ;;          ((string-match "^[ \t]*clear[ \t]*$" command)
  ;;           (comint-send-string proc "\n")
  ;;           (erase-buffer))
  ;;          ;; Check for man command and execute it.
  ;;          ((string-match "^[ \t]*man[ \t]*" command)
  ;;           (comint-send-string proc "\n")
  ;;           (setq command (replace-regexp-in-string
  ;;   		               "^[ \t]*man[ \t]*" "" command))
  ;;           (setq command (replace-regexp-in-string
  ;;   		               "[ \t]+$" "" command))
  ;;           (funcall 'man command))
  ;;          ;; Send other commands to the default handler.
  ;;          (t (comint-simple-send proc command)))))

  (defun my-comint-mode-hook ()
    (setq-local mouse-yank-at-point t)
    (setq-local transient-mark-mode nil)
    (setq-local global-hl-line-mode nil)
    (setq-local beacon-mode nil)
    (setq-local scroll-margin 0))
  (add-hook 'comint-mode-hook 'my-comint-mode-hook))

(use-package shell
  :commands (shell)
  :bind (("C-x t s" . shell))
  :config
  (if (fboundp 'helm-comint-input-ring)
      (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)))

(use-package term
  :config
  (defadvice ansi-term (before set-shell activate)
    "Set Shell Program."
    (interactive (list my-term-shell)))

  (defadvice term (before set-shell activate)
    "Set Shell Program."
    (interactive (list my-term-shell)))

  (defadvice term-mode (before set-term activate)
    "Set TERM=linux if in 8-color."
    (if (<= (display-color-cells) 8)
        (set-variable 'term-term-name "linux")))

  (defadvice term-mode (after set-company activate)
    "Disable company-mode in term"
    (when (and (featurep 'company) company-mode)
      (company-mode -1)))

  (defun my-term-mode-hook ()
    ;; (term-set-escape-char ?\C-x)
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
    (setq-local mouse-yank-at-point t)
    (setq-local transient-mark-mode nil)
    (setq-local global-hl-line-mode nil)
    (setq-local beacon-mode nil)
    (setq-local scroll-margin 0)
    (setq-default system-uses-terminfo t)
    )

  (add-hook 'term-mode-hook 'my-term-mode-hook)
  )

(use-package multi-term
  :ensure t
  :commands (multi-term)
  :bind (
         ;; ("C-x e" . my/multi-term)
         ("C-x t x" . term)
         ("C-x t a" . ansi-term)
         ("C-x t i" . my/multi-term-dedicated-toggle-and-select)
         ("C-x t m" . my/multi-term)
         ("C-x t t" . multi-term)
         ("C-x t n" . multi-term-next)
         ("C-x t p" . multi-term-prev)
         ("C-x t l" . my/multi-term-list-buffer))

  :config
  (setq multi-term-program (or (executable-find "zsh") (executable-find "bash")))

  (defun my-term-use-utf8 ()
    "Use utf8."
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook 'my-term-use-utf8)

  (defun my/term-mode-toggle-line-mode ()
    (interactive)
    (if (eq major-mode 'term-mode)
        (if (term-in-char-mode)
            (term-line-mode)
          (term-char-mode))
      (message "not term-mode")))

  (setq multi-term-program my-term-shell
        multi-term-scroll-to-bottom-on-output t
        multi-term-scroll-show-maximum-output nil
        multi-term-switch-after-close nil
        )

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

  (defun my/multi-term-list-buffer ()
    (interactive)
    (let ((p (completing-read "term: "
                              (mapcar 'buffer-name multi-term-buffer-list))))
      (switch-to-buffer p)
      ))

  (defun multi-term-internal ()
    "Override multi-term-internal."
    (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
    (add-hook 'term-mode-hook 'multi-term-keystroke-setup)
    ;; Load term mode
    (term-mode)
    (term-char-mode)
    ;; Handle `output' variable.
    (setq term-scroll-show-maximum-output multi-term-scroll-show-maximum-output
          term-scroll-to-bottom-on-output multi-term-scroll-to-bottom-on-output)
    (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook))

  (defun my/multi-term ()
    "My term start and select."
    (interactive)
    (let ((term-buffer (car  multi-term-buffer-list)))
      (unless term-buffer
        (setq term-buffer (multi-term-get-buffer current-prefix-arg))
        (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
        (set-buffer term-buffer)
        (multi-term-internal))
      (switch-to-buffer-other-window term-buffer)))

  ;; (setq-default multi-term-program "/bin/bash")
  ;; (setq multi-term-dedicated-close-back-to-open-buffer-p t)
  ;; (setq multi-term-dedicated-select-after-open-p t)
  ;; (setq-default system-uses-terminfo t)
  ;; (setq-default term-buffer-maximum-size 0)

  ;; (add-to-list 'term-bind-key-alist '("C-c C-a") 'term-bol)
  ;; (add-to-list 'term-bind-key-alist '("C-c C-m" . my/toggle-term-mode))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-q" . term-quit-subjob))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-z" . term-stop-subjob))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))
  ;; (add-to-list 'term-bind-key-alist '("C-l" . term-send-raw))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-p" . term-previous-prompt))
  ;; (add-to-list 'term-bind-key-alist '("C-c C-n" . term-next-prompt))


  ;; char-mode
  (add-to-list 'term-bind-key-alist '("C-c C-a" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-q" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-z" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("TAB" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-l" . term-send-raw))
  (add-to-list 'term-bind-key-alist '("C-c C-m" . my/term-mode-toggle-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-p" . term-previous-prompt))
  (add-to-list 'term-bind-key-alist '("C-c C-n" . term-next-prompt))

  (add-to-list 'term-bind-key-alist '("C-x ." . multi-term-next))
  (add-to-list 'term-bind-key-alist '("C-x ," . multi-term-prev))

  ;; line-mode
  (define-key term-mode-map (kbd "C-c C-m") 'my/term-mode-toggle-line-mode)
  (define-key term-mode-map (kbd "C-l") 'term-send-raw)
  (define-key term-mode-map (kbd "C-c C-p") 'term-previous-prompt)
  (define-key term-mode-map (kbd "C-c C-n") 'term-next-prompt))

(use-package eshell
  :defer t
  :bind (("C-x t e" . eshell))
  :config
  (setq eshell-directory-name (expand-file-name "eshell" my-cache-dir))
  (setq eshell-cmpl-cycle-completions nil
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

  ;; fix scroll to bottom copy from eshell/clear
  (defun my-eshell/clear ()
    (interactive)
    (eshell/clear-scrollback)
    (let ((eshell-input-filter-functions
           (remq 'eshell-add-to-history eshell-input-filter-functions)))
      (eshell-send-input)))

  (defun my--init-eshell ()
    "Stuff to do when enabling eshell."
    (setq-default pcomplete-cycle-completions nil)
    (if (bound-and-true-p linum-mode) (linum-mode -1))
    (when semantic-mode
      (semantic-mode -1))
    (when (boundp 'eshell-output-filter-functions)
      (push 'eshell-truncate-buffer eshell-output-filter-functions))

    ;; Caution! this will erase buffer's content at C-l
    (define-key eshell-mode-map (kbd "C-l") 'my-eshell/clear)
    (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof)
    (setq-local global-hl-line-mode nil)
    )

  (add-hook 'eshell-mode-hook 'my--init-eshell))

(provide 'my-term)
;;; my-term.el ends here
