;;; my-extension.el --- extension package            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  liyunteng

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; linum replaced by nlinum
(use-package nlinum
  :ensure t
  :commands (nlinum-mode)
  :init
  (my|add-toggle linum-mode
    :status nlinum-mode
    :on (nlinum-mode +1)
    :off (nlinum-mode -1)
    :documentation "Show line number")
  ;; (add-hook 'prog-mode-hook 'my/toggle-linum-mode-on)
  )

(use-package json-mode
  :ensure t
  :commands (json-mode)
  :defer t)

(use-package fill-column-indicator
  :ensure t
  :diminish fci-mode
  :commands (turn-on-fci-mode
	         turn-off-fci-mode
	         fci-mode)
  :init
  (my|add-toggle fci-mode
    :status fci-mode
    :on (turn-on-fci-mode)
    :off (turn-off-fci-mode)
    :documentation "Display the fill column indicator"
    :global-key "C-# i"
    )
  :config
  (setq fci-rule-width 2
	    ;; fci-rule-color "#D0BF8F"
	    ))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

(use-package uptimes
  :ensure t
  :defines (uptimes-database)
  :preface
  (setq uptimes-database (expand-file-name ".uptime.el" my-cache-dir)))

(use-package keyfreq
  :ensure t
  :diminish keyfreq-mode
  :init
  (setq keyfreq-file (expand-file-name ".keyfreq" my-cache-dir))
  (setq keyfreq-file-lock (expand-file-name ".keyfreq.lock" my-cache-dir))
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

(use-package move-dup
  :ensure t
  :diminish move-dup-mode
  :config
  (my|add-toggle move-dup-mode
    :mode move-dup-mode
    :on (move-dup-mode +1)
    :off (move-dup-mode)
    :documentation "Move line up/down.")
  (global-move-dup-mode +1))

(use-package highlight-escape-sequences
  :ensure t
  :init
  (hes-mode +1))

;; diff-hl
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode global-diff-hl-mode)
  :bind ("C-x v f" . 'diff-hl-diff-goto-hunk)
  :init
  (my|add-toggle diff-hl-mode
    :mode diff-hl-mode
    :documentation "Highlight diff")
  (my|add-toggle global-diff-hl-mode
    :mode global-diff-hl-mode
    :documentation "Global highlight diff")
  (global-diff-hl-mode +1)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; auto indent
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands (aggressive-indent-mode)
  :init
  (my|add-toggle aggressive-indent-mode
    :mode aggressive-indent-mode
    :documentation "Always keep code indent.")
  (aggressive-indent-mode -1))

;; delete space
;; (use-package hungry-delete
;;   :ensure t
;;   :config
;;   (global-hungry-delete-mode +1))

;; expand-region
(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind (("C-=" . er/expand-region))
  :config
  (setq expand-region-contract-fast-key ",")
  (setq expand-region-smart-cursor nil))

;; easy-kill
(use-package easy-kill
  :ensure t
  :commands (easy-kill easy-mark)
  :bind (([remap kill-ring-save] . easy-kill)))

;; page-break-lines "s-q C-l"
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode +1)
  (push 'prog-mode page-break-lines-modes))

;; smarter kill-ring navigation
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring))
  :init
  (setq browse-kill-ring-separator "\f")
  (after-load 'page-break-lines
    (push 'browse-kill-ring-mode page-break-lines-modes))
  :config
  (browse-kill-ring-default-keybindings))

;; projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("A a" . projectile-add-known-project)
              ("A d" . projectile-remove-known-project))
  :ensure t
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-cache-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks" my-cache-dir))
  (setq projectile-mode-line-prefix " PJ")
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-mode +1))

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo-tree/" my-cache-dir))))
  (global-undo-tree-mode +1)

  ;; fix undo-tree maybe cause menu-bar can't work
  ;; (remove-hook 'menu-bar-update-hook 'undo-tree-update-menu-bar)
  (defun undo-tree-update-menu-bar ()
    "Update `undo-tree-mode' Edit menu items."
    (if undo-tree-mode
        (progn
	      ;; save old undo menu item, and install undo/redo menu items
	      (setq undo-tree-old-undo-menu-item
	            (cdr (assq 'undo (lookup-key global-map [menu-bar edit]))))
	      (define-key (lookup-key global-map [menu-bar edit])
	        [undo] '(menu-item "Undo" undo-tree-undo
			                   :enable (and undo-tree-mode
					                        (not buffer-read-only)
					                        (not (eq t buffer-undo-list))
                                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                            ;; add buffer-undo-tree judgement ;;
                                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					                        (and buffer-undo-tree
                                                 (undo-tree-node-previous
					                              (undo-tree-current buffer-undo-tree))))
			                   :help "Undo last operation"))
	      (define-key-after (lookup-key global-map [menu-bar edit])
	        [redo] '(menu-item "Redo" undo-tree-redo
			                   :enable (and undo-tree-mode
					                        (not buffer-read-only)
					                        (not (eq t buffer-undo-list))
                                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                            ;; add buffer-undo-tree judgement ;;
                                            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					                        (and buffer-undo-tree
                                                 (undo-tree-node-next
					                              (undo-tree-current buffer-undo-tree))))
			                   :help "Redo last operation")
	        'undo))
      ;; uninstall undo/redo menu items
      (define-key (lookup-key global-map [menu-bar edit])
        [undo] undo-tree-old-undo-menu-item)
      (define-key (lookup-key global-map [menu-bar edit])
        [redo] nil))))

;; use settings from .editorconfig file when present
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode +1))

;;; indent-guide
(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :commands (indent-guide-mode)
  :init
  (add-hook 'python-mode-hook 'indent-guide-mode)
  :config
  (setq indent-guide-delay 0.3))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-x M <" . mc/mark-previous-like-this)
         ("C-x M >" . mc/mark-next-like-this)
         ("C-x M C-<" . mc/mark-all-like-this)
         ("C-x M r" . set-rectangular-region-anchor)
         ("C-x M c" . mc/edit-ines)
         ("C-x M e" . mc/edit-ends-of-lines)
         ("C-x M a" . mc/edit-beginnings-of-lines)))

;; discover-my-major
(use-package discover-my-major
  :ensure t
  :bind (("C-h RET" . discover-my-major)))

(use-package symbol-overlay
  :ensure t
  :diminish symbol-overlay-mode
  :bind (:map symbol-overlay-mode-map
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)
              ("M-i" . my/symbol-overlay-put))
  :init
  (defun my/symbol-overlay-put ()
    "Replace `symbol-overlay-put' with `tab-do-tab-stop' when no symbol."
    (interactive)
    (if (thing-at-point 'symbol)
        (call-interactively 'symbol-overlay-put)
      (call-interactively  'tab-to-tab-stop)))
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook css-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
                      :background
                      (face-attribute 'isearch
                                      :background)
                      :foreground
                      (face-attribute 'isearch
                                      :foreground)))

(use-package iedit
  :ensure t
  :commands iedit-mode
  :bind (("C-;" . iedit-mode))
  :config
  (setq iedit-toggle-key-default nil)
  (add-hook 'iedit-mode-hook
            '(lambda ()
               (define-key iedit-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
               (define-key iedit-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))))

;; multi major mode
(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))

;; print unicode
(use-package list-unicode-display
  :ensure t
  :defer t)

;; (use-package vlf
;;   :ensure t
;;   :init
;;   (defun ffap-vlf ()
;;     "Find file at point with VLF."
;;     (interactive)
;;     (let ((file (ffap-file-at-point)))
;;       (unless (file-exists-p file)
;;         (error "File does not exist: %s" file))
;;       (vlf file)))
;;   :config
;;   (require 'vlf-setup))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap open-line] . crux-smart-open-line)
         ("C-x M-;" . crux-duplicate-and-comment-current-line-or-region)
         ("C-x M-o" . crux-open-with)
         ;; ("C-c u" . crux-view-url)
         ("C-x M-k" . crux-kill-other-buffers)
         ("M-J" . crux-top-join-line))
  :init
  (defadvice crux-open-with (after my-after-crux-open-with-ad activate)
    (message "opend with external application.")))

(use-package youdao-dictionary
  :ensure t
  :defer t)

;; make useless word
(use-package lorem-ipsum
  :ensure
  :init
  (lorem-ipsum-use-default-bindings))

;; GTAGS
;; (use-package ggtags
;;   :ensure t
;;   :commands (ggtags-mode ggtags-find-project ggtags-find-tag-dwim)
;;   :bind
;;   (:map ggtags-mode-map
;;              ("C-c g s" . ggtags-find-other-symbol)
;;              ("C-c g h" . ggtags-view-tag-history)
;;              ("C-c g r" . ggtags-find-reference)
;;              ("C-c g f" . ggtags-find-file)
;;              ("C-c g c" . ggtags-create-tags)
;;              ("C-c g u" . ggtags-update-tags)
;;              ("C-c g a" . helm-gtags-tags-in-this-function)
;;              ("C-c g ." . ggtags-find-tag-dwim)
;;              ("C-c g g" . ggtags-find-definition)
;;              ;; ("M-." . ggtags-find-tag-dwim)
;;              ;; ("M-," . pop-tag-mark)
;;              ("C-c <" . ggtags-prev-mark)
;;              ("C-c >" . ggtags-next-mark)
;;              )
;;   :init
;;   (add-hook 'c-mode-common-hook
;;                      (lambda ()
;;                        (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;                              (ggtags-mode +1))))
;;   )


(provide 'my-extension)
;;; my-extension.el ends here
