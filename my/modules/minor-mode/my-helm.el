;;; my-helm.el --- helm                              -*- lexical-binding: t; -*-

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

(my-require-package 'helm)
;;(my-require-package 'helm-directory)
(my-require-package 'helm-ag)
(my-require-package 'helm-descbinds)
(my-require-package 'helm-smex)
(my-require-package 'helm-swoop)
(my-require-package 'helm-projectile)
(require 'helm)
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds)
(require 'helm-smex)
(require 'helm-swoop)

(diminish 'helm-mode)
(when (executable-find "curl")
  (setq-default helm-google-suggest-use-curl-p t))

(setq-default helm-split-window-in-side-p t
			  helm-buffers-fuzzy-matching t
              helm-move-to-line-cycle-in-source t
              helm-ff-search-library-in-sexp t
			  helm-ff-file-compressed-list t
			  helm-ff-file-name-history-use-recentf t
              helm-scroll-amount 8
              helm-echo-input-in-header-line nil)

(setq helm-swoop-split-with-multiple-windows t
      helm-swoop-split-direction 'split-window-vertically
      helm-swoop-speed-or-color t
      helm-swoop-split-window-function 'helm-default-display-buffer
      helm-swoop-pre-input-function (lambda () ""))
(global-set-key (kbd "M-s e") 'helm-swoop)

(defun my/helm-swoop-region-or-symbol ()
  "Call `helm-swoop' with default input."
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda ()
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (let ((thing (thing-at-point 'symbol t)))
               (if thing thing ""))))))
    (call-interactively 'helm-swoop)))

(defun my--helm-cleanup ()
  "Cleanup some helm related states when quitting."
  ;; deactivate any running transient map (transient-state)
  (setq overriding-terminal-local-map nil))

(defun my--helm-do-grep-region-or-symbol
    (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts
                         default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                              (if (region-active-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))
                                (thing-at-point 'symbol t))))
                 (quoted-input (when new-input
                                 (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts
                     default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets   (if targs
                      targs
                    (helm-read-file-name
                     "Search in file(s): "
                     :marked-candidates t
                     :preselect (if helm-ff-transformer-show-only-basename
                                    (helm-basename preselection)
                                  preselection)))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

(defun my/helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (my--helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))
(global-set-key [remap helm-do-grep] 'my/helm-file-do-grep)

(defun my/helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point."
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))
;; (global-set-key [remap helm-find-files] 'my/helm-find-files)

(defun my--helm-find-files-edit (candidate)
  "Opens a dired buffer and immediately switches to editable mode."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))

(defun my/helm-find-files-edit ()
  "Exits helm, opens a dired buffer and immediately switches to editable mode."
  (interactive)
  (helm-exit-and-execute-action 'my--helm-find-files-edit))

(defun my--helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))
(add-hook 'helm-minibuffer-set-up-hook 'my--helm-hide-minibuffer-maybe)

(add-hook 'helm-cleanup-hook 'my--helm-cleanup)
(after-load 'helm-locate
  (helm-locate-set-command)
  (setq-default helm-locate-fuzzy-match (string-match "locate" helm-locate-command)))

(defun my-helm-bookmark-keybindings ()
  (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
  (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
  (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
  (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
  )
(add-hook 'helm-mode-hook 'my-helm-bookmark-keybindings)


;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-h i") 'helm-info)
(define-key my-mode-map (kbd "C-c f") 'helm-recentf)

(define-key helm-command-prefix (kbd "o")     'helm-occur)
(define-key helm-command-prefix (kbd "g")     'helm-do-grep)
(define-key helm-command-prefix (kbd "C-c w") 'helm-wikipedia-suggest)
(define-key helm-command-prefix (kbd "SPC")   'helm-all-mark-rings)
(define-key helm-command-prefix (kbd "x") 'my/helm-faces)
(define-key helm-command-prefix (kbd "m") 'helm-man-woman)

(define-key helm-find-files-map (kbd "C-c C-e") 'my/helm-find-files-edit)
(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; swap <tab> and C-z
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-z") 'helm-select-action)

(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-y") 'helm-yank-text-at-point)
(define-key helm-map (kbd "C-M-y") 'yank)

(defun my/helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

;; shell history.
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq-default projectile-completion-system 'helm)
(helm-descbinds-mode)
(helm-mode 1)

;; enable Helm version of Projectile with replacment commands
(helm-projectile-on)

(provide 'my-helm)
;;; my-helm.el ends here
