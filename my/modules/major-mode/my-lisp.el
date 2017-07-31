;;; my-lisp.el --- lisp                              -*- lexical-binding: t; -*-

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

(my-require-package 'lively)
(my-require-package 'ipretty)
;; (my-require-package 'hl-sexp)
(my-require-package 'immortal-scratch)
(my-require-package 'cl-lib-highlight)
(my-require-package 'aggressive-indent)
(my-require-package 'rainbow-mode)
(my-require-package 'rainbow-delimiters)
(my-require-package 'eldoc-eval)
(my-require-package 'macrostep)
(my-require-package 'highlight-quoted)
(my-require-package 'flycheck-package)
(my-require-package 'elisp-slime-nav)
(my-require-package 'auto-compile)

(require 'elisp-slime-nav)
(diminish 'elisp-slime-nav-mode "Nav")
(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook
                help-mode-hook
                messages-buffer-mode-hook
                completion-list-mode-hook
                debugger-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))
(define-key elisp-slime-nav-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

;; Make C-x C-e run 'eval-region if the region is active
(defun my/eval-last-sexp-or-region (prefix)
  "Eval PREFIX if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))
(global-set-key [remap eval-expression] 'pp-eval-expression)

(ipretty-mode 1)

(defadvice pp-display-expression (after my-make-read-only (expression out-buffer-name) activate)
  "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))

(defun my-maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'my-maybe-set-bundled-elisp-readonly)


;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.
(defvar my-repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")
(make-variable-buffer-local 'my-repl-original-buffer)

(defvar my-repl-switch-function 'switch-to-buffer-other-window)

(defun my/switch-to-ielm ()
  "Switch to ielm."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall my-repl-switch-function "*ielm*")
      (ielm))
    (setq my-repl-original-buffer orig-buffer)))

(defun my/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if my-repl-original-buffer
      (funcall my-repl-switch-function my-repl-original-buffer)
    (error "No original buffer")))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'my/switch-to-ielm)
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'my/switch-to-ielm)
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'my/repl-switch-back))


;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(when (require 'auto-compile)
  (auto-compile-on-save-mode 1)
  (auto-compile-on-load-mode 1))

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)

;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
;; (after-load 'hl-sexp
;;   (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
;;     (when turn-on
;;       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(add-hook 'after-init-hook 'immortal-scratch-mode)


;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun my/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))

;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun my-enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

;; (defun my-disable-indent-guide ()
;;   (when (bound-and-true-p indent-guide-mode)
;;     (indent-guide-mode -1)))

(defun my-lispy-modes-setup ()
  "My lispy mode hooks."
  (turn-on-eldoc-mode)
  ;; hl-sexp-mode
  (aggressive-indent-mode)
  ;; my-disable-indent-guide
  (my-enable-check-parens-on-save)
  (turn-on-smartparens-strict-mode)
  (rainbow-mode +1)
  (rainbow-delimiters-mode +1)
  (local-set-key (kbd "C-c C-b") 'eval-buffer)
  (local-set-key (kbd "C-c C-c") 'eval-defun)
  (local-set-key (kbd "C-x C-e") 'my/eval-last-sexp-or-region)

  (if (boundp 'yas-minor-mode)
      (yas-minor-mode -1))
  (if (eq major-mode 'emacs-lisp-mode)
      (setq mode-name "EL"))
  (if (eq major-mode 'lisp-interaction-mode)
      (setq mode-name "LI"))
  )

(defun my-emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst my-elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst my-lispy-modes
  (append my-elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name my-lispy-modes))
  (add-hook hook 'my-lispy-modes-setup))

(dolist (hook (mapcar #'derived-mode-hook-name my-elispy-modes))
  (add-hook hook 'my-emacs-lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (require 'eldoc-eval)
  (eldoc-in-minibuffer-mode 1))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


(after-load 'lisp-mode
  (cl-lib-highlight-initialize))

;; ----------------------------------------------------------------------------
;; Delete .elc files when reverting the .el from VC or magit
;; ----------------------------------------------------------------------------

;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.

;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.

(defvar my-vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defadvice revert-buffer (after my-maybe-remove-elc activate)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when my-vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
			   buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(defadvice magit-revert-buffers (around my-reverting activate)
  (let ((my-vc-reverting t))
    ad-do-it))
(defadvice vc-revert-buffer-internal (around my-reverting activate)
  (let ((my-vc-reverting t))
    ad-do-it))

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

;; Extras for theme editing

(defvar my-theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun my-run-theme-mode-hooks-if-theme ()
  "Run `my-theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'my-theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook 'my-run-theme-mode-hooks-if-theme t)

(when (require 'rainbow-mode nil t)
  (add-hook 'my-theme-mode-hook 'rainbow-mode))

;; Can be prohibitively slow with very long forms
(when (require 'aggressive-indent)
  (add-to-list 'my-theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))

(when (require 'highlight-quoted)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(when (require 'flycheck)
  (my-require-package 'flycheck-package)
  (after-load 'flycheck
    (flycheck-package-setup)))

;; ERT
(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))

(defun my/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$" form)
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))

(defconst my-lisp-scratch-buffer-name "*scratch*")
(defun my/lisp-scratch ()
  "Create a 'lisp-interaction-mode' scratch buffer."
  (interactive)
  (pop-to-buffer (my--lisp-find-or-create-scratch-buffer)))

(defun my--lisp-find-or-create-scratch-buffer ()
  "Find or create the 'lisp-interaction-mode' scratch buffer."
  (or (get-buffer my-lisp-scratch-buffer-name)
      (my--lisp-create-scratch-buffer)))

(defun my--lisp-create-scratch-buffer ()
  "Create a new 'lisp-interaction-mode' scratch buffer."
  (with-current-buffer (get-buffer-create my-lisp-scratch-buffer-name)
    (lisp-interaction-mode)
    (insert ";; Happy hacking " (or user-login-name "") " - Emacs ♥ you!\n\n")
    (current-buffer)))




(provide 'my-lisp)
;;; my-lisp.el ends here
