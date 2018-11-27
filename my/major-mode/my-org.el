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

;; (require 'org)
;; (require 'org-agenda)
;; (require 'org-clock)
(setq-default org-directory (expand-file-name "org" user-emacs-directory))
(defvar my-org-inbox-file (expand-file-name "inbox.org" org-directory))
(defvar my-org-task-file (expand-file-name "task.org" org-directory))
(defvar my-org-note-file (expand-file-name "note.org" org-directory))
(defvar my-org-project-file (expand-file-name "project.org" org-directory))
(defvar my-org-finished-file (expand-file-name "finished.org" org-directory))

(use-package org
  :defer t
  :bind (("C-x c" . org-capture)
	 ("C-x l" . org-stored-links)
	 ("C-x a" . org-agenda))

  :config
  (use-package ox
    :config
    (setq org-export-coding-system 'utf-8))
  (use-package org-archive
    :config
    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::* Archive"))
  (use-package org-capture)
  (use-package org-pomodoro
    :defer t
    :ensure t
    :config
    (setq org-pomodoro-keep-killed-pomodoro-time t))
  ;; (use-package org-fstree
  ;; 	:ensure t
  ;; 	:defer t
  ;; 	)
  (use-package org-cliplink
    :defer t
    :ensure t)
  (use-package ob-ditaa
    :defer t
    :config
    ;; Lots of stuff from http://doc.norang.ca/org-mode.html
    (defun my-grab-ditaa (url jar-name)
      "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
      ;; TODO: handle errors
      (message "Grabbing %s for org." jar-name)
      (let ((zip-temp (make-temp-name "emacs-ditaa")))
	(unwind-protect
	    (progn
	      (when (executable-find "unzip")
		(url-copy-file url zip-temp)
		(shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
				       " " (shell-quote-argument jar-name) " > "
				       (shell-quote-argument org-ditaa-jar-path)))))
	  (when (file-exists-p zip-temp)
	    (delete-file zip-temp)))))

    (unless (and (boundp 'org-ditaa-jar-path)
		 (file-exists-p org-ditaa-jar-path))
      (let ((jar-name "ditaa0_9.jar")
	    (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
	(setq org-ditaa-jar-path (expand-file-name jar-name my-cache-dir))
	(unless (file-exists-p org-ditaa-jar-path)
	  (my-grab-ditaa url jar-name)))))

  (setq org-agenda-files (list
			  my-org-inbox-file
			  my-org-task-file
			  my-org-note-file
			  my-org-project-file
			  my-org-finished-file))

  (setq org-log-done t
	org-edit-timestamp-down-means-later t
	org-hide-emphasis-markers t
	org-catch-invisible-edits 'show
	org-fast-tag-selection-single-key 'expert
	org-html-validation-link nil
	org-tags-column -80
	org-support-shift-select t
	)

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
	(quote ((sequence "TODO(t)" "|" "DONE(d!/!)")
		(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
		(sequence "PROJECT(p)" "|" "DONE(d!/!)")
		(sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)")
		(sequence "|" "CANCELLED(c@/!)")
		))
	org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
	(quote (("NEXT" :inherit warning)
		("PROJECT" :inherit font-lock-string-face))))

  (setq org-capture-templates
	`(
	  ;; ("t" "todo" entry (file+olp (concat org-directory
	  ;;                                     "/inbox.org") "todo")
	  ;;  "* TODO %? %T :TODO:\n\n%a\n\n")

	  ("t" "todo" entry (file+olp my-org-task-file "TASKS")
	   "* TODO %? %T \n  %l\n\n")

	  ("n" "note" entry (file+olp my-org-note-file "NOTES")
	   "* %?  %T :NOTE:\n  %l\n\n" :clock-resume t)

	  ("i" "Inbox" entry (file+olp my-org-inbox-file "INBOX")
	   "* %? %T :INBOX:\n  %l\n" :clock-resume t)
	  ))

  (setq org-tag-alist
	'((:startgroup . nil)
	  ("@home" . ?h)
	  ("@work" . ?w)
	  (:endgroup . nil)
	  ("NOTE" . ?n)
	  ("INBOX" . ?p)
	  ))

  (let ((active-project-match "-INBOX/PROJECT"))
    (setq org-stuck-projects
	  `(,active-project-match ("NEXT")))
    (setq org-agenda-compact-blocks t
	  org-agenda-sticky t
	  org-agenda-start-on-weekday nil
	  org-agenda-span 'day
	  org-agenda-include-diary nil
	  org-agenda-sorting-strategy
	  '((agenda habit-down time-up user-defined-up effort-up category-keep)
	    (todo category-up effort-up)
	    (tags category-up effort-up)
	    (search category-up))
	  org-agenda-window-setup 'current-window
	  org-agenda-custom-commands
	  `(("n" "Notes"
	     ((tags "NOTE"
		    ((org-agenda-overriding-header "NOTES")
		     (org-tags-match-list-sublevels nil)
		     (org-agenda-sorting-strategy
		      '(category-keep))))))
	    ("g" "GTD"
	     ((agenda "" nil)
	      (tags "INBOX"
		    ((org-agenda-overriding-header "Inbox")
		     (org-tags-match-list-sublevels nil)))
	      (stuck ""
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-tags-todo-honor-ignore-options t)
		      (org-tags-match-list-sublevels t)
		      (org-agenda-todo-ignore-scheduled 'future)))
	      (tags-todo "-INBOX/NEXT"
			 ((org-agenda-overriding-header "Next Actions")
			  (org-agenda-tags-todo-honor-ignore-options t)
			  (org-agenda-todo-ignore-scheduled 'future)
			  ;; TODO: skip if a parent is WAITING or HOLD
			  (org-tags-match-list-sublevels t)
			  (org-agenda-sorting-strategy
			   '(todo-state-down effort-up category-keep))))
	      (tags-todo ,active-project-match
			 ((org-agenda-overriding-header "Projects")
			  (org-tags-match-list-sublevels t)
			  (org-agenda-sorting-strategy
			   '(category-keep))))
	      (tags-todo "-INBOX/-NEXT"
			 ((org-agenda-overriding-header "Orphaned Tasks")
			  (org-agenda-tags-todo-honor-ignore-options t)
			  (org-agenda-todo-ignore-scheduled 'future)
			  ;; TODO: skip if a parent is a project
			  (org-agenda-skip-function
			   '(lambda ()
			      (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
				  (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
			  (org-tags-match-list-sublevels t)
			  (org-agenda-sorting-strategy
			   '(category-keep))))
	      (tags-todo "/WAITING"
			 ((org-agenda-overriding-header "Waiting")
			  (org-agenda-tags-todo-honor-ignore-options t)
			  (org-agenda-todo-ignore-scheduled 'future)
			  (org-agenda-sorting-strategy
			   '(category-keep))))
	      (tags-todo "/DELEGATED"
			 ((org-agenda-overriding-header "Delegated")
			  (org-agenda-tags-todo-honor-ignore-options t)
			  (org-agenda-todo-ignore-scheduled 'future)
			  (org-agenda-sorting-strategy
			   '(category-keep))))
	      (tags-todo "-INBOX/HOLD"
			 ((org-agenda-overriding-header "On Hold")
			  ;; TODO: skip if a parent is WAITING or HOLD
			  (org-tags-match-list-sublevels nil)
			  (org-agenda-sorting-strategy
			   '(category-keep))))
	      ;; (tags-todo "-NEXT"
	      ;;            ((org-agenda-overriding-header "All other TODOs")
	      ;;             (org-match-list-sublevels t)))
	      )))))


  ;;; Org clock
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

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
	'(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;;; Agenda views
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

  ;; ;; Show iCal calendars in the org agenda
  ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
  ;;   (setq org-agenda-include-diary t
  ;;         org-agenda-custom-commands
  ;;         '(("I" "Import diary from iCal" agenda ""
  ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

  ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
  ;;             (lambda ()
  ;;               (goto-char (point-min))
  ;;               (save-excursion
  ;;                 (while (re-search-forward "^[a-z]" nil t)
  ;;                   (goto-char (match-beginning 0))
  ;;                   (insert "0:00-24:00 ")))
  ;;               (while (re-search-forward "^ [a-z]" nil t)
  ;;                 (goto-char (match-beginning 0))
  ;;                 (save-excursion
  ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
  ;;                 (insert (match-string 0))))))

  ;; (defun my/prettify-org-buffer ()
  ;;     "Apply visual enchantments to the current buffer.
  ;; The buffer's major mode should be `org-mode'."
  ;;     (interactive)
  ;;     (unless (derived-mode-p 'org-mode)
  ;;       (user-error "org-mode should be enabled in the current buffer."))

  ;;     ;; Make ~SPC ,~ work, reference:
  ;;     ;; http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode
  ;;     (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n")
  ;;     (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;;     (setq-local org-emphasis-alist '(("*" bold)
  ;; 				     ("/" italic)
  ;; 				     ("_" underline)
  ;; 				     ("=" org-verbatim verbatim)
  ;; 				     ("~" org-kbd)
  ;; 				     ("+"
  ;; 				      (:strike-through t))))
  ;;     (when (require 'space-doc nil t)
  ;;       (space-doc-mode)))

  (defun my/view-org-file (file &optional anchor-text expand-scope)
    "Open org file and apply visual enchantments.
FILE is the org file to be opened.
If ANCHOR-TEXT  is `nil' then run `re-search-forward' with ^ (beginning-of-line).
If ANCHOR-TEXT is a GitHub style anchor then find a corresponding header.
If ANCHOR-TEXT isn't a GitHub style anchor then run `re-search-forward' with
ANCHOR-TEXT.
If EXPAND-SCOPE is `subtree' then run `outline-show-subtree' at the matched line.
If EXPAND-SCOPE is `all' then run `outline-show-all' at the matched line."
    (interactive "F")
    (find-file file)
    (my-prettify-org-buffer)
    (goto-char (point-min))
    (when anchor-text
      ;; If `anchor-text' is GitHub style link.
      (if (string-prefix-p "#" anchor-text)
	  ;; If the toc-org package is loaded.
	  (if (memq 'toc-org features)
	      ;; For each heading. Search the heading that corresponds
	      ;; to `anchor-text'.
	      (while (and (re-search-forward "^[\\*]+\s\\(.*\\).*$" nil t)
			  (not (string= (toc-org-hrefify-gh (match-string 1))
					anchor-text))))
	    ;; This is not a problem because without the space-doc package
	    ;; those links will be opened in the browser.
	    (message (format (concat "Can't follow the GitHub style anchor: '%s' "
				     "without the org layer.") anchor-text)))
	(re-search-forward anchor-text)))
    (beginning-of-line)
    (cond
     ((eq expand-scope 'subtree)
      (show-subtree))
     ((eq expand-scope 'all)
      (show-all))
     (t nil)))

  (require 'org-faces)
  (set-face-attribute 'org-level-1 nil :height 1.6 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.4 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.2 :bold t)
  (set-face-attribute 'org-level-4 nil :height 1.0 :bold t)
  (org-clock-persistence-insinuate)
  ;; (add-hook 'org-mode-hook 'buffer-face-mode)

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

  ;; remove empty logbook drawers on clock out
  (defun my/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "logbook" (point))))

  ;;; Show the clocked-in task - if any - in the header line
  (defun my-show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun my-hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))


  (after-load 'org-clock
    (add-hook 'org-clock-in-hook 'my-show-org-clock-in-header-line)
    (add-hook 'org-clock-out-hook 'my-hide-org-clock-from-header-line)
    (add-hook 'org-clock-out-hook 'my/remove-empty-drawer-on-clock-out 'append)
    (add-hook 'org-clock-cancel-hook 'my-hide-org-clock-from-header-line)
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)
    )

  (after-load 'org-agenda
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry)
    (add-hook 'org-agenda-mode-hook 'hl-line-mode)
    ;; (define-key org-agenda-mode-map (kbd "p") 'org-pomodoro)
    )

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t)))

  (setq org-startup-with-inline-images t
	org-src-fontify-natively t
	;; this is consistent with the value of
	;; `helm-org-headings-max-depth'.
	org-imenu-depth 8)
  (font-lock-add-keywords
   'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
		(1 font-lock-comment-face prepend)
		(2 font-lock-function-name-face)
		(3 font-lock-comment-face prepend))))

  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)

  ;; confilct with move-dup
  (define-key org-mode-map (kbd "M-<up>") 'org-metaup)
  (define-key org-mode-map (kbd "M-<down>") 'org-metadown)
  )


(use-package org-bullets
  :ensure t
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
