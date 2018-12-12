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
  :bind (("M-U" . helm-resume))

  :config
  (use-package helm-config
    :init
    (setq helm-command-prefix-key "C-c h")

    (defun my/helm-faces ()
      "Describe face."
      (interactive)
      (require 'helm-elisp)
      (let ((default (or (face-at-point) (thing-at-point 'symbol))))
        (helm :sources (helm-def-source--emacs-faces
		                (format "%s" (or default "default")))
	          :buffer "*helm faces*")))

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

    :config
    (define-key helm-command-prefix (kbd "R") 'helm-regexp)
    (define-key helm-command-prefix (kbd "r") 'helm-recentf)
    (define-key helm-command-prefix (kbd "o") 'helm-occur)
    (define-key helm-command-prefix (kbd "C-l") 'helm-locate-library)
    (define-key helm-command-prefix (kbd "I") 'helm-semantic-or-imenu)
    (define-key helm-command-prefix (kbd "i") 'helm-imenu-in-all-buffers)
    (define-key helm-command-prefix (kbd "C-c w") 'helm-wikipedia-suggest)
    (define-key helm-command-prefix (kbd "SPC") 'helm-all-mark-rings)
    (define-key helm-command-prefix (kbd "m") 'helm-man-woman)
    (define-key helm-command-prefix (kbd "x") 'helm-run-external-command)
    (define-key helm-command-prefix (kbd "X") 'my/helm-faces)
    (define-key helm-command-prefix (kbd "A") 'helm-manage-advice)
    (define-key helm-command-prefix (kbd "T") 'helm-timers)
    (define-key helm-command-prefix (kbd "u") 'my/resume-last-search-buffer))

  (use-package helm-adaptive
    :init
    (helm-adaptive-mode +1)
    :config
    (setq helm-adaptive-history-file (expand-file-name "helm-adaptive-history" my-cache-dir)))

  (use-package helm-bookmark
    :bind (([remap list-bookmarks] . helm-filtered-bookmarks)
           ([remap bookmark-jump] . helm-filtered-bookmarks))
    :config
    (setq helm-bookmark-show-location t)
    (define-key helm-bookmark-map (kbd "C-d") 'delete-char)
    (define-key helm-bookmark-map (kbd "C-]") 'helm-bookmark-toggle-filename)
    (define-key helm-bookmark-map (kbd "C-c d") 'helm-bookmark-run-delete)
    (define-key helm-bookmark-map (kbd "C-c e") 'helm-bookmark-run-edit)
    (define-key helm-bookmark-map (kbd "C-x C-d") 'helm-bookmark-run-browse-project)
    (define-key helm-bookmark-map (kbd "C-c o") 'helm-bookmark-run-jump-other-window)
    (define-key helm-bookmark-map (kbd "C-c c-o") 'helm-bookmark-run-jump-other-frame))

  (use-package helm-buffers
    :bind (([remap switch-to-buffer] . helm-mini))
    :config
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-buffer-details-flag t)
    (setq helm-buffer-skip-remote-checking t)
    (define-key helm-buffer-map (kbd "M-s g") 'helm-buffer-run-zgrep)
    (define-key helm-buffer-map (kbd "M-s r") 'helm-buffer-run-query-replace)
    (define-key helm-buffer-map (kbd "M-s M-r") 'helm-buffer-run-query-replace-regexp)
    (define-key helm-buffer-map (kbd "C-s") 'helm-buffers-run-multi-occur)
    (define-key helm-buffer-map (kbd "C-c =") 'helm-buffer-run-ediff)
    (define-key helm-buffer-map (kbd "C-=") 'helm-buffer-diff-persistent)
    (define-key helm-buffer-map (kbd "M-R") 'helm-buffer-run-rename-buffer)
    (define-key helm-buffer-map (kbd "C-c C-r") 'helm-buffer-revert-persistent))

  (use-package helm-command
    :bind (([remap execute-extended-command] . helm-M-x))
    :config
    (setq helm-M-x-always-save-history t)
    (setq helm-M-x-reverse-history nil)
    (setq helm-M-x-fuzzy-match t))

  (use-package helm-files
    :bind (([remap find-file] . helm-find-files))
    :config
    (setq helm-tramp-verbose t)
    (setq helm-ff-fuzzy-matching t)
    (setq helm-ff-no-preselect nil)
    (setq helm-ff-search-library-in-sexp t)
    (setq helm-ff-file-name-history-use-recentf t)
    (define-key helm-find-files-map (kbd "M-s s") 'helm-ff-run-zgrep)
    (define-key helm-find-files-map (kbd "M-s a") 'helm-ff-run-grep-ag)
    (define-key helm-find-files-map (kbd "M-s g") 'helm-ff-run-git-grep)
    (define-key helm-find-files-map (kbd "M-s /") 'helm-ff-run-find-sh-command)
    (define-key helm-find-files-map (kbd "C-x M-o") 'helm-ff-run-open-file-with-default-tool))

  (use-package helm-for-files
    :commands (helm-for-files helm-multi-files helm-recentf)
    :config
    (setq helm-recentf-fuzzy-match nil))

  (use-package helm-ring
    :bind (([remap browse-kill-ring] . helm-show-kill-ring)
           ([remap list-registers] . helm-register)
           :map helm-kill-ring-map
           ("C-n" . helm-next-line)
           ("C-p" . helm-previous-line)
           ("C-d" . helm-kill-ring-delete)
           ("C-]" . helm-kill-ring-toggle-truncated)
           ("C-k" . helm-kill-ring-kill-selection))
    :config
    (setq helm-kill-ring-separator "\f"))

  (use-package helm-locate
    :bind (("M-s l" . helm-localte)))

  (use-package helm-find
    :bind (([remap find-name-dired] . helm-find)))

  (use-package helm-imenu
    :commands (helm-imenu helm-imenu-in-all-buffers)
    :config
    (setq helm-imenu-fuzzy-match nil)
    (setq helm-imenu-delimiter "  "))

  (use-package helm-semantic
    :bind (("C-c C-j" . helm-semantic-or-imenu))
    :config
    (setq helm-semantic-fuzzy-match t))

  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode +1)
    :config
    (setq helm-mode-fuzzy-match t)
    (setq helm-mode-reverse-history t))

  (use-package helm-info
    :bind (([remap info] . helm-info)))

  (use-package helm-tags
    :bind (([remap find-tag] . helm-etags-select)))

  (when (executable-find "curl")
    (after-load 'helm-net
      (setq helm-net-prefer-curl t)))

  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source nil
        helm-scroll-amount 8
        helm-echo-input-in-header-line nil
        helm-display-header-line nil
        helm-always-two-windows t
        helm-candidate-number-limit 200)

  (define-key helm-map (kbd "C-z") 'helm-toggle-suspend-update)
  (define-key helm-map (kbd "C-w") 'backward-kill-word)
  (define-key helm-map (kbd "C-y") 'helm-yank-text-at-point)
  (define-key helm-map (kbd "C-M-y") 'yank)
  ;; disable helm-select-action
  (define-key helm-map (kbd "C-i") (lambda () (interactive) t))
  (define-key helm-map (kbd "M-x") 'helm-select-action)

  ;;; Save current position to mark ring
  ;; (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  )

(when (executable-find "ag")
  (use-package helm-ag
    :ensure t
    :bind (([remap ag] . helm-ag)
           ([remap ag-dired] . helm-do-ag)
           :map helm-ag-map
           ("C-M-n" . helm-ag--next-file)
           ("C-M-p" . helm-ag--previous-file))
    :config
    (setq next-error-function 'next-error)))

(use-package helm-descbinds
  :ensure t
  :commands (helm-descbinds)
  :config
  (setq helm-descbinds-window-style 'one-window))

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
  (add-hook 'after-init-hook 'helm-projectile-on)
  :config
  (setq projectile-switch-project-action 'helm-projectile
	    projectile-completion-system 'helm))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-s i" . helm-swoop)
   ("M-s I" . helm-swoop-back-to-last-point)
   ("M-s M-i" . helm-multi-swoop)
   ("M-s M-I" . helm-multi-swoop-all)
   :map helm-swoop-map
   ("C-w" . backward-kill-word))

  :config
  (setq helm-multi-swoop-edit-save t
	    helm-swoop-split-with-multiple-windows t
	    helm-swoop-split-direction 'split-window-vertically
	    helm-swoop-speed-or-color t
	    ;; helm-swoop-split-window-function 'helm-default-display-buffer
        )
  (defun my--helm-swoop-region-or-symbol ()
    (if (region-active-p)
	    (buffer-substring-no-properties (region-beginning)
					                    (region-end))
	  (let ((thing (thing-at-point 'symbol t)))
	    (if thing thing ""))))
  (setq helm-swoop-pre-input-function (lambda ()))
  ;; (setq helm-swoop-pre-input-function (lambda () (thing-at-point 'symbol)))
  )


(provide 'my-helm)
;;; my-helm.el ends here
