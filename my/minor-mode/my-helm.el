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


;;(use-package helm-directory)

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands (helm-mode)
  :bind (([remap execute-extended-command] . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap switch-to-buffer] . helm-mini)
         ([remap list-registers] . helm-registers)
         ([remap list-bookmarks] . helm-bookmarks)
         ([remap browse-kill-ring] . helm-show-kill-ring)
         ([remap info] . helm-info)
         ([remap find-name-dired] . helm-find)
         ([remap find-tag] . helm-etags-select)
         ("C-c C-j" . helm-imenu)
         ("M-U" . helm-resume))
  :init
  (setq helm-kill-ring-separator "\f")
  (helm-mode +1)
  :config
  (require 'helm-config)
  (require 'helm-grep)

  (if (executable-find "ag")
      (use-package helm-ag
        :ensure t
        :bind
        (([remap ag] . helm-ag)
         ([remap ag-dired] . helm-do-ag)
         :map helm-ag-map
         ("C-M-n" . helm-ag--next-file)
         ("C-M-p" . helm-ag--previous-file)
         )
        :config
        (setq next-error-function 'next-error)))

  (use-package helm-descbinds
    :ensure t
    :init
    (setq helm-descbinds-window-style 'one-window))

  ;; (use-package helm-gtags
  ;; 	:ensure t
  ;; 	:init
  ;; 	(progn
  ;; 	  (setq helm-gtags-ignore-case t
  ;; 			helm-gtags-auto-update t
  ;; 			helm-gtags-use-input-at-cursor t
  ;; 			helm-gtags-pulse-at-cursor t
  ;; 			helm-gtags-prefix-key "\C-cg"
  ;; 			helm-gtags-suggested-key-mapping t)

  ;; 	  ;; Enable helm-gtags-mode in Dired so you can jump to any tag
  ;; 	  ;; when navigate project tree with Dired
  ;; 	  (add-hook 'dired-mode-hook 'helm-gtags-mode)

  ;; 	  ;; Enable helm-gtags-mode in Eshell for the same reason as above
  ;; 	  (add-hook 'eshell-mode-hook 'helm-gtags-mode)

  ;; 	  ;; Enable helm-gtags-mode in languages that GNU Global supports
  ;; 	  (add-hook 'c-mode-hook 'helm-gtags-mode)
  ;; 	  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  ;; 	  (add-hook 'java-mode-hook 'helm-gtags-mode)
  ;; 	  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  ;; 	  ;; key bindings
  ;; 	  (with-eval-after-load 'helm-gtags
  ;; 		(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  ;; 		(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  ;; 		(define-key helm-gtags-mode-map (kbd "C-c g .") 'helm-gtags-dwim)
  ;; 		;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  ;; 		(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  ;; 		(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))))

  ;; (use-package helm-smex
  ;; 	:ensure t)

  (use-package helm-projectile
    :ensure t
    :commands (helm-projectile-switch-to-buffer
	           helm-projectile-find-dir
	           helm-projectile-dired-find-dir
	           helm-projectile-recentf
	           helm-projectile-find-file
	           helm-projectile-grep
	           helm-projectile
	           helm-projectile-switch-project)
    :init
    (setq projectile-switch-project-action 'helm-projectile
	      projectile-completion-system 'helm)
    (helm-projectile-on)
    ;; (add-hook 'after-init-hook 'helm-projectile-on)
    )

  (use-package helm-swoop
    :ensure t
    :bind
    (("M-s i" . helm-swoop)
     ("M-s I" . helm-swoop-back-to-last-point)
     ("M-s M-i" . helm-multi-swoop)
     ("M-s M-I" . helm-multi-swoop-all)
     :map helm-swoop-map
     ("C-w" . backward-kill-word))

    :init
    (setq helm-multi-swoop-edit-save t
	      helm-swoop-split-with-multiple-windows t
	      helm-swoop-split-direction 'split-window-vertically
	      helm-swoop-speed-or-color t
	      ;; helm-swoop-split-window-function 'helm-default-display-buffer
          )
    :config
    (defun my--helm-swoop-region-or-symbol ()
      (if (region-active-p)
	      (buffer-substring-no-properties (region-beginning)
					                      (region-end))
	    (let ((thing (thing-at-point 'symbol t)))
	      (if thing thing ""))))
    (setq helm-swoop-pre-input-function (lambda ()))
    ;; (setq helm-swoop-pre-input-function (lambda () (thing-at-point 'symbol)))
    )

  (setq helm-split-window-inside-p t
        helm-mode-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-imenu-fuzzy-match nil
        helm-semantic-fuzzy-match nil
        helm-ff-fuzzy-matching nil
        ;; helm-apropos-fuzzy-match t
        ;; helm-lisp-fuzzy-completion t

        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-ff-file-compressed-list t
        helm-ff-file-name-history-use-recentf t
        helm-scroll-amount 8
        helm-echo-input-in-header-line nil
        helm-display-header-line nil
        helm-always-two-windows t
        helm-org-headings-fontify t

        helm-buffer-skip-remote-checking t
        helm-bookmark-show-location t
        helm-mode-reverse-history t
        helm-M-x-reverse-history nil
        )
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  (when (executable-find "curl")
    (after-load 'helm-net
      (setq helm-net-prefer-curl t)))

  (defun my--helm-cleanup ()
    "Cleanup some helm related states when quitting."
    ;; deqactivate any running transient map (transient-state)
    (setq overriding-terminal-local-map nil))
  (add-hook 'helm-cleanup-hook 'my--helm-cleanup)

  (defun my/resume-last-search-buffer ()
    "open last helm-ag or hgrep buffer."
    (interactive)
    (cond ((get-buffer "*helm ag results*")
	       (switch-to-buffer-other-window "*helm ag results*"))
	      ((get-buffer "*helm-ag*")
	       (helm-resume "*helm-ag*"))
	      ((get-buffer "*hgrep*")
	       (switch-to-buffer-other-window "*hgrep*"))
	      (t
	       (message "No previous search buffer found"))))

  (defun my--helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	    (overlay-put ov 'window (selected-window))
	    (overlay-put ov 'face
		             (let ((bg-color (face-background 'default nil)))
		               `(:background ,bg-color :foreground ,bg-color)))
	    (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook 'my--helm-hide-minibuffer-maybe)

  (when (executable-find "locate")
    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command)))

  (after-load 'helm-bookmarks
    (defun my-helm-bookmark-keybindings ()
      (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
      (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
      (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
      ;; (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
      )
    (add-hook 'helm-mode-hook 'my-helm-bookmark-keybindings))

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (define-key helm-command-prefix (kbd "r") 'helm-recentf)
  (define-key helm-command-prefix (kbd "o") 'helm-occur)
  (define-key helm-command-prefix (kbd "C-l") 'helm-locateI-library)
  (define-key helm-command-prefix (kbd "i") 'helm-semantic-or-imenu)
  (define-key helm-command-prefix (kbd "I") 'helm-imenu)
  (define-key helm-command-prefix (kbd "C-c w") 'helm-wikipedia-suggest)
  (define-key helm-command-prefix (kbd "SPC") 'helm-all-mark-rings)
  (define-key helm-command-prefix (kbd "x") 'my/helm-faces)
  (define-key helm-command-prefix (kbd "m") 'helm-man-woman)
  (define-key helm-command-prefix (kbd "u") 'my/resume-last-search-buffer)



  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

  (define-key helm-map (kbd "C-z") 'helm-toggle-suspend-update)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "C-y") 'helm-yank-text-at-point)
  (define-key helm-map (kbd "C-M-y") 'yank)
  ;; disable helm-select-action
  (define-key helm-map (kbd "C-i") nil)
  (define-key helm-map (kbd "M-x") 'helm-select-action)

  (defun my/helm-faces ()
    "Describe face."
    (interactive)
    (let ((default (or (face-at-point) (thing-at-point 'symbol))))
      (helm :sources (helm-def-source--emacs-faces
		              (format "%s" (or default "default")))
	        :buffer "*helm faces*")))


  ;;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; use helm to list eshell history
  ;; (add-hook 'eshell-mode-hook
  ;;           #'(lambda ()
  ;;       	(substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)))

  ;; (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  )

(provide 'my-helm)
;;; my-helm.el ends here
