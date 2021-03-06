;;; my-org.el --- org                                -*- lexical-binding: t; -*-

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

(use-package org
  :commands (org-load-modules-maybe)
  :defer t
  :bind (("C-x l" . org-store-link))

  :config
  (use-package ob-ditaa
    :defer t
    :init
    (setq org-ditaa-jar-path (expand-file-name "ditaa0_9.jar" my-cache-dir))
    ;; Lots of stuff from http://doc.norang.ca/org-mode.html
    (defun my-grab-ditaa (url jar-name)
      "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
      ;; TODO: handle errors
      (message "Grabbing %s for org." jar-name)
      (let ((zip-temp (expand-file-name (make-temp-name "emacs-ditaa") temporary-file-directory)))
        (unwind-protect
            (progn
              (when (executable-find "unzip")
    	        (url-copy-file url zip-temp)
    	        (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
    			                       " " (shell-quote-argument jar-name) " > "
    			                       (shell-quote-argument org-ditaa-jar-path)))))
          (when (file-exists-p zip-temp)
            (delete-file zip-temp)))))
    :config
    (unless (and (boundp 'org-ditaa-jar-path)
    	         (file-exists-p org-ditaa-jar-path))
      (let ((jar-name "ditaa0_9.jar")
            (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
        (unless (file-exists-p org-ditaa-jar-path)
          (my-grab-ditaa url jar-name)))))

  ;; (after-load 'ob-plantuml
  ;;   (let ((jar-name "plantuml.jar")
  ;;         (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
  ;;     (setq org-plantuml-jar-path (expand-file-name jar-name my-cache-dir))
  ;;     (unless (file-exists-p org-plantuml-jar-path)
  ;;       (url-copy-file url org-plantuml-jar-path))))

  (use-package org-clock
    :config
    ;; Org clock
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)
    (setq org-clock-persist-file (expand-file-name "org-clock-save" my-cache-dir))

    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Save state changes in the LOGBOOK drawer
    (setq org-log-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Show the clocked-in task - if any - in the header line
    (defun my-show-org-clock-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun my-hide-org-clock-from-header-line ()
      (setq-default header-line-format nil))

    ;; remove empty logbook drawers on clock out
    (defun my/remove-empty-drawer-on-clock-out ()
      (interactive)
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point))))

    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)
    (add-hook 'org-clock-in-hook 'my-show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'my-hide-org-clock-from-header-line)
    (add-hook 'org-clock-cancel-hook 'my-hide-org-clock-from-header-line)
    (add-hook 'org-clock-out-hook 'my/remove-empty-drawer-on-clock-out 'append)
    )

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (use-package org-duration
    :config
    (setq org-duration-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

  (use-package org-faces
    :config
    (set-face-attribute 'org-level-1 nil :height 1.6 :bold t)
    (set-face-attribute 'org-level-2 nil :height 1.4 :bold t)
    (set-face-attribute 'org-level-3 nil :height 1.2 :bold t)
    (set-face-attribute 'org-level-4 nil :height 1.0 :bold t))

  (setq org-directory (expand-file-name "org" user-emacs-directory))
  (defvar my-org-inbox-file (expand-file-name "inbox.org" org-directory))
  (defvar my-org-task-file (expand-file-name "task.org" org-directory))
  (defvar my-org-note-file (expand-file-name "note.org" org-directory))
  (defvar my-org-project-file (expand-file-name "project.org" org-directory))
  (defvar my-org-finished-file (expand-file-name "finished.org" org-directory))
  (setq org-log-done t
	    org-edit-timestamp-down-means-later t
	    org-hide-emphasis-markers t
	    org-catch-invisible-edits 'show
	    org-fast-tag-selection-single-key 'expert
	    org-tags-column 80
	    org-support-shift-select t)

  (setq org-refile-use-cache nil)
  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  ;; Targets start with the file name - allows creating level 1 tasks
  ;; (setq org-refile-use-outline-path (quote file))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;;; To-do settings
  (setq org-todo-keywords
	    (quote ((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d!/!)")
		        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
		        (sequence "|" "CANCELLED(c@/!)")))
	    org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
	    (quote (("NEXT" :inherit warning))))

  (setq org-tag-alist
	    '((:startgroup . nil)
	      ("@home" . ?h)
	      ("@work" . ?w)
	      (:endgroup . nil)
	      ("NOTE" . ?n)
	      ("INBOX" . ?p)))

  (setq org-startup-with-inline-images t
	    org-src-fontify-natively t
	    ;; this is consistent with the value of
	    ;; `helm-org-headings-max-depth'.
	    org-imenu-depth 8)

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (java . t)
     (python . t)
     (emacs-lisp . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (matlab .t)
     (latex . t)
     (R . t)
     (ditaa . t)
     (dot . t)
     (lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (ruby . t)
     (screen . nil)
     (sql . nil)
     (sqlite . t)))


  (font-lock-add-keywords
   'org-mode '(("\\(<kbd>\\)\\(.*\\)\\(</kbd>\\)"
		        (1 font-lock-comment-face prepend)
		        (2 font-lock-function-name-face)
		        (3 font-lock-comment-face prepend))))

  ;; exclude done state tasks from refile targets
  (defun my-verify-refile-target ()
    "exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'my-verify-refile-target)

  (defun my/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "a version of `org-refile' which suppresses `org-refile-target-verify-function'."
    (interactive "p")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))


  (defun my-org-mode-hook ()
    (if (boundp 'move-dup-mode)
        (move-dup-mode -1)))
  (add-hook 'org-mode-hook 'my-org-mode-hook)

  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive))

(use-package htmlize
  :defer t
  :ensure t)

(use-package org-capture
  :defer t
  :commands (org-capture)
  :bind (("C-x c" . org-capture))
  :config
  (setq org-capture-templates
	    `(("t" "todo" entry (file+headline my-org-task-file "TASKS")
	       "* TODO %?\n%U\n" :clock-resume t)

	      ("n" "note" entry (file+headline my-org-note-file "NOTES")
	       "* %? :NOTE:\n%U\n%a\n" :clock-resume t)

	      ("i" "Inbox" entry (file+headline my-org-inbox-file "INBOX")
	       "* %? :INBOX:\n%U\n"))))


(use-package org-agenda
  :defer t
  :commands (org-agenda)
  :defines (org-agenda-files)
  :bind (("C-x a" . org-agenda))
  :config
  (setq org-agenda-files (list
			              my-org-inbox-file
			              my-org-task-file
			              my-org-note-file
			              my-org-project-file
			              my-org-finished-file))

  ;; (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
  ;;; Agenda views
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

  (setq  org-agenda-compact-blocks nil
         org-agenda-block-separator ""
         org-agenda-sticky t
         org-agenda-start-on-weekday 1
         org-agenda-span 'week
         org-agenda-include-diary nil
         org-agenda-sorting-strategy
         '((agenda habit-down time-up user-defined-up effort-up category-keep)
           (todo category-up effort-up)
           (tags category-up effort-up)
           (search category-up))
         org-agenda-window-setup 'current-window
         org-agenda-custom-commands
         `(("n" "Notes" tags "NOTE"
            ((org-agenda-overriding-header "Notes")
             (org-tags-match-list-sublevels t)))
           ("g" "GTD"
            ((agenda "")
             (tags "INBOX"
                   ((org-agenda-overriding-header "Inbox")
                    (org-tags-match-list-sublevels nil)))
             (tags-todo "-INBOX"
                        ((org-agenda-overriding-header "Next Tasks")
                         (org-agenda-tags-todo-honor-ignore-options t)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-skip-function
                          '(lambda ()
                             (org-agenda-skip-entry-if 'nottodo '("NEXT"))))
                         (org-tags-match-list-sublevels t)
                         (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))
             (tags-todo "-INBOX/-NEXT"
                        ((org-agenda-overriding-header "Todo Tasks")
                         (org-agenda-tags-todo-honor-ignore-options t)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-skip-function
                          '(lambda ()
                             (or (org-agenda-skip-subtree-if 'scheduled)
                                 (org-agenda-skip-subtree-if 'nottodo '("TODO")))))
                         (org-tags-match-list-sublevels t)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-INBOX/-NEXT"
                        ((org-agenda-overriding-header "Scheduled Tasks")
                         (org-agenda-tags-todo-honor-ignore-options nil)
                         ;; (org-agenda-todo-ignore-with-date nil)
                         ;; (org-agenda-todo-ignore-scheduled nil)
                         (org-agenda-skip-function
                          '(lambda ()
                             (or (org-agenda-skip-subtree-if 'notscheduled)
                                 (org-agenda-skip-subtree-if 'notodo '("TODO")))))
                         (org-tags-match-list-sublevels t)
                         (org-agenda-sorting-strategy
                          '(todo-state-down category-keep))))
             (tags-todo "-INBOX/-NEXT"
                        ((org-agenda-overriding-header "Other Tasks")
                         (org-agenda-tags-todo-honor-ignore-options t)
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-skip-function
                          '(lambda ()
                             (org-agenda-skip-entry-if 'todo '("TODO" "NEXT"))))
                         (org-tags-match-list-sublevels t)
                         (org-agenda-sorting-strategy
                          '(todo-state-down category-keep)))))))))

;; (use-package htmlize
;;   :ensure t
;;   :after org)

(use-package org-pomodoro
  :ensure t
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))

;; (use-package org-cliplink
;;   :ensure t
;;   :after org)

(use-package org-bullets
  :ensure t
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        (delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        ;; (flyspell-mode 1)
        (when (fboundp 'visual-line-mode)
          (visual-line-mode 1)))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'show-trailing-whitespace)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode -1))))

(provide 'my-org)
;;; my-org.el ends here
