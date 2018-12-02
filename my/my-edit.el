;;; my-edit.el --- edit                              -*- lexical-binding: t; -*-

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

;; (use-package linum
;;   :init
;;   (global-linum-mode +1)
;;   :init
;;   (setq linum-delay t)
;;   (setq linum-format 'dynamic)

;;   (my|add-toggle linum-mode
;;     :status linum-mode
;;     :on (linum-mode +1)
;;     :off (linum-mode -1)
;;     :documentation "Show line number")

;;   :config
;;   (defadvice linum-schedule (around my-linum-schedule () activate)
;;     "Updated line number every second."
;;     (run-with-idle-timer 1 nil #'linum-update-current)
;;     ad-do-it)
;;   (add-hook 'prog-mode-hook 'my/toggle-linum-mode-on)
;;   )

;; highlight current line
(use-package hl-line
  :commands (global-hl-line-mode
             hl-line-mode)
  :init
  (my|add-toggle hl-line-mode
    :status hl-line-mode
    :on (hl-line-mode +1)
    :off (progn
           (hl-line-mode nil)
           (setq-local global-hl-line-mode nil))
    :documentation "Show trailing-whitespace")
  ;; can't in :config
  (global-hl-line-mode +1))


;; (use-package cua-base
;;   :init
;;   ;; When called with no active region, do not activate mark.
;;   (defadvice cua-exchange-point-and-mark (before deactivate-mark activate compile)
;;     "When called with no active region, do not activate mark."
;;     (interactive
;;      (list (not (region-active-p)))))
;;   :config
;;   (setq cua-auto-mark-last-change t)
;;   (cua-selection-mode +1))

;;
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  ;; (setq uniquify-buffer-name-style 'forward)
  ;; (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

;; ediff
(use-package ediff
  :commands (ediff)
  :config
  (setq
   ediff-window-setup-function 'ediff-setup-windows-plain
   ediff-split-window-function 'split-window-horizontally
   ediff-merge-split-window-function 'split-window-horizontally)
  (require 'outline)
  (add-hook 'ediff-prepare-buffer-hook #'show-all)
  (add-hook 'ediff-quit-hook #'winner-undo)
  )

;; clean up obsolete buffers automatically
(use-package midnight
  :init
  (midnight-mode +1))

;; bookmark
(use-package bookmark
  :bind (("C-x r b" . bookmark-jump)
         ("C-x r m" . bookmark-set)
         ("C-x r l" . list-bookmarks))
  :config
  (setq bookmark-save-flag t)
  (setq bookmark-default-file (expand-file-name "bookmarks" my-cache-dir))
  (push '(".emacs.d" (filename . "~/.emacs.d") (front-context-string . "my"))
        bookmark-alist))

;; abbrev config
(when (file-exists-p abbrev-file-name)
  (use-package abbrev
    :diminish abbrev-mode
    :config
    (setq abbrev-file-name (expand-file-name "abbrev_defs" my-cache-dir))
    (abbrev-mode +1)))

;; make a shell script executable automatically on save
(use-package executable
  :commands (executable-make-buffer-file-executable-if-script-p)
  :init
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

;; saner regex syntax
(use-package re-builder
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'string))

;; Auto revert
(use-package autorevert
  :diminish
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
  :init
  (add-hook 'after-init-hook 'global-auto-revert-mode))

;; print current in which function in mode line
(use-package which-func
  :config
  (which-function-mode +1))

;; whitespace 设置
(use-package whitespace
  :commands (whitespace-mode)
  :init
  ;; (set-face-attribute 'trailing-whitespace nil
  ;;                                      :background
  ;;                                      (face-attribute 'font-lock-warning-face
  ;;                                                      :foreground))
  (my|add-toggle show-trailing-whitespace
    :status show-trailing-whitespace
    :on (setq-local show-trailing-whitespace +1)
    :off (setq-local show-trailing-whitespace nil)
    :documentation "Show trailing-whitespace")
  (add-hook 'prog-mode-hook #'my/toggle-show-trailing-whitespace-on)

  (dolist (hook '(special-mode-hook
                  Info-mode-hook
                  eww-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  twittering-mode-hook
                  minibuffer-setup-hook
                  calendar-mode-hook
                  eshell-mode-hook
                  shell-mode-hook
                  term-mode-hook))
    (add-hook hook #'my/toggle-show-trailing-whitespace-off))

  (my|add-toggle whitespace-mode
    :mode whitespace-mode
    :documentation "Show whitespace")

  (setq whitespace-line-column fill-column)
  (setq whitespace-style
        '(face
          tabs
          tab-mark
          spaces
          space-mark
          trailling
          lines-tail
          indentation::space
          indentation:tab
          newline
          newline-mark))

  (use-package my-whitespace-cleanup-mode
    :init
    (my|add-toggle whitespace-cleanup-mode
      :status whitespace-cleanup-mode
      :on (whitespace-cleanup-mode +1)
      :off (whitespace-cleanup-mode -1)
      :documentation "Cleanup whitesapce")
    :config
    (global-whitespace-cleanup-mode +1))
  ;; (set-face-attribute 'whitespace-space nil
  ;;                                      :background nil
  ;;                                      :foreground (face-attribute 'font-lock-warning-face
  ;;                                                                                              :foreground))
  ;; (set-face-attribute 'whitespace-tab nil
  ;;                                      :background nil)
  ;; (set-face-attribute 'whitespace-indentation nil
  ;;                                      :background nil)
  )

;; auto indent when yanking
(defvar my-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar my-yank-indent-threshold 30000)
(defvar my-yank-indent-modes '(prog-mode text-mode))
;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation from BEG to END, as long as the region isn't too large."
  (if (<= (- end beg) my-yank-indent-threshold)
      (indent-region beg end nil)))

(my|advise-commands "indent" (yank yank-pop) after
                    "If current mode is derived of `my-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                    (if (and (not (ad-get-arg 0))
                             (not (member major-mode my-indent-sensitive-modes))
                             (apply #'derived-mode-p my-yank-indent-modes))
                        (let ((transient-mark-mode nil))
                          (yank-advised-indent-function (region-beginning) (region-end)))))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))
(with-region-or-buffer align)
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; hippie
(use-package hippie-exp
  :commands (hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

;; proced
(use-package proced
  :bind ("C-x p" . proced)
  :config
  (setq proced-auto-update-flag t)
  (setq proced-auto-update-interval 3)
  ;; (setq proced-post-display-hook '(fit-window-to-buffer))
  )

;; 设置默认浏览器为firefox
;; (setq browse-url-firefox-new-window-is-tab t)
;; (setq browse-url-firefox-program "firefox")

;;;netstat命令的默认参数
(use-package net-utils
  :commands (ifconfig iwconfig netstat-program-options arp route traceroute ping
                      nslookup-host nslookup dns-lookup-host dig run-dig ftp
                      finger  whois  network-connection-to-service
                      network-connection)
  :config
  (setq netstat-program-options '("-natup")
        ping-program-options '()))
(use-package etags
  :commands (xref-find-definitions
             xref-find-definitions-other-window
             xref-find-definitions-other-frame
             xref-find-apropos
             tags-loop-continue
             tags-search
             pop-tag-mark
             )
  :config
  ;;设置TAGS文件
  (when (file-exists-p "/usr/include/TAGS")
    (add-to-list 'tags-table-list "/usr/include/TAGS"))
  (when (file-exists-p "/usr/local/include/TAGS")
    (add-to-list 'tags-table-list "/usr/local/include/TAGS"))

  (setq tags-revert-without-query t
        tags-case-fold-search nil ;; t=case-insensitive, nil=case-sensitive
        tags-add-tables nil               ;don't ask user
        ))


;; calendar
(use-package calendar
  :commands (calendar)
  :config
  (setq
   calendar-date-style (quote iso)
   calendar-mark-holidays-flag t
   calendar-chinese-all-holidays-flag t))

;; When called with no active region, do not activate mark.
(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(use-package compile
  :commands (compile my/smart-compile my/insert-compile-command)
  :init
  (require 'files-x)

  (defcustom my-samrt-compile-auto-insert-compile-command t
    "Auto insert compile-command to file."
    :type 'boolean
    :group 'my-config)

  (defun my/insert-compile-command ()
    "Insert compile command to file."
    (interactive)
    (save-excursion
      ;; add compile-command to header
      ;; (modify-file-local-variable-prop-line 'compile-command (eval compile-command)
      ;;                                                                                `add-or-replace)
      (modify-file-local-variable 'compile-command
                                  (eval compile-command)
                                  `add-or-replace)))

  (defun my/smart-compile()
    "Smart compile.
1. if directory have `makefile' `Makefile' `GNUmakefile' `GNUMakefile',
compile command will be `make -k'

2. c/c++-mode if installed clang, use clang to compile. or use gcc/g++.
compile command will be `gcc/g++/clang/clang++ -Wall -o x x.cpp -g'

3. go-mode compile command will be `go run xx.go'

4. if `my-samrt-compile-auto-insert-compile-command' is t, and you change
compile-command, will auto insert new-compile-command to code file.

5. make clean won't be insert."

    (interactive)
    ;; do save
    (setq-local compilation-directory default-directory)
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    ;; find compile-command
    (let ((command (eval compile-command))
          (candidate-make-file-name '("makefile" "Makefile" "GNUmakefile" "GNUMakefile"))
          (compiler (file-name-nondirectory (or (executable-find "clang")
                                                (executable-find "gcc")
                                                "gcc"))))
      (if (string-prefix-p "make" command)
          (unless (find t candidate-make-file-name :key
                        '(lambda (f) (file-readable-p f)))
            (cond ((eq major-mode 'c-mode)
                   (setq command
                         (concat compiler " -Wall -o "
                                 (file-name-sans-extension
                                  (file-name-nondirectory buffer-file-name))
                                 " "
                                 (file-name-nondirectory buffer-file-name)
                                 " -g ")))
                  ;; c++-mode
                  ((eq major-mode 'c++-mode)
                   (setq command
                         (concat  (cond ((equal "gcc" compiler) "g++")
                                        ((equal "clang" compiler) "clang++")
                                        (t "g++")) " -Wall -o "
                                        (file-name-sans-extension
                                         (file-name-nondirectory buffer-file-name))
                                        " "
                                        (file-name-nondirectory buffer-file-name)
                                        " -g ")))
                  ((eq major-mode 'go-mode)
                   (setq command
                         (concat "go run "
                                 (buffer-file-name)
                                 " ")))
                  )))
      ;; (setq-local compilation-directory default-directory)

      (let ((new-command (compilation-read-command command)))
        (unless (equal command new-command)
          (unless (or (equal new-command "make -k clean")
                      (equal new-command "make clean"))
            (setq-local compile-command new-command)
            (when my-samrt-compile-auto-insert-compile-command
              (my/insert-compile-command))))
        (setq command new-command))
      (compilation-start command)
      ))

  :config
  (use-package ansi-color)
  (setq compilation-ask-about-save nil  ; Just save before compiling
        compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
        compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
        )

  ;; Compilation from Emacs
  (defun my/colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer))

;; align
(use-package align
  :bind (("C-x \\" . my/align-repeat))
  :commands (align align-regexp)
  :init
  (defun my/align-repeat (start end regexp &optional justify-right after)
    "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
    (interactive "r\nsAlign regexp: ")
    (let* ((ws-regexp (if (string-empty-p regexp)
                          "\\(\\s-+\\)"
                        "\\(\\s-*\\)"))
           (complete-regexp (if after
                                (concat regexp ws-regexp)
                              (concat ws-regexp regexp)))
           (group (if justify-right -1 1)))
      (message "%S" complete-regexp)
      (align-regexp start end complete-regexp group 1 t)))
  (defun my/align-repeat-decimal (start end)
    "Align a table of numbers on decimal points and dollar signs (both optional) from START to END."
    (interactive "r")
    (align-region start end nil
                  '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                         (repeat . t)
                         (group 1 2)
                         (spacing 1 1)
                         (justify nil t)))
                  nil))
  (defmacro my|create-align-repeat-x (name regexp &optional justify-right default-after)
    (let ((new-func (intern (concat "my/align-repeat-" name))))
      `(defun ,new-func (start end switch)
         (interactive "r\nP")
         (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
           (my/align-repeat start end ,regexp ,justify-right after)))))

  (my|create-align-repeat-x "comma" "," nil t)
  (my|create-align-repeat-x "semicolon" ";" nil t)
  (my|create-align-repeat-x "colon" ":" nil t)
  (my|create-align-repeat-x "equal" "=")
  (my|create-align-repeat-x "math-oper" "[+\\-*/]")
  (my|create-align-repeat-x "ampersand" "&")
  (my|create-align-repeat-x "bar" "|")
  (my|create-align-repeat-x "left-paren" "(")
  (my|create-align-repeat-x "right-paren" ")" t)
  (my|create-align-repeat-x "backslash" "\\\\")
  )

;; comment
(use-package newcomment
  :commands (comment-line
             comment-indent-new-line
             comment-dwim
             comment-or-uncomment-region
             comment-box
             comment-region
             uncomment-region
             comment-kill
             comment-set-column
             comment-indent
             comment-indent-default
             )
  :bind (("M-;" . my/comment-dwim-line)
         ("C-M-;" . comment-kill)
         ("C-c M-;" . comment-box))
  :init
  (defun my/comment-dwim-line (&optional arg)
    "Replacement for the \"comment-dwim\" command.
If no region is selected and current line is not blank and we are not
at the end of the line,then comment current line,els use \"(comment-dwim ARG)\"
Replaces default behaviour of comment-dwim, when it inserts comment
at the end of the line."
    (interactive "*P")
    (comment-normalize-vars)
    (if  (and (not (region-active-p)) (not (looking-at "[ \t]*$")))

        (if (comment-beginning)
            (comment-or-uncomment-region (comment-beginning)
                                         (progn (backward-char) (search-forward comment-end)))
          (comment-or-uncomment-region (line-beginning-position)
                                       (line-end-position)))
      (comment-dwim arg))
    (indent-according-to-mode))
  :config
  (setq comment-style 'extra-line)
  ;; (setq comment-style 'multi-line)
  )

(use-package register
  :bind (("C-x r f" . frameset-to-register)
         ("C-x r w" . list-registers)

         ("C-x r ." . point-to-register)
         ("C-x r j" . jump-to-register)
         ("C-x r i" . insert-register)
         ("C-x r a" . append-to-register)
         ("C-x r p" . prepend-to-register))
  :config
  (setq register-preview-delay 0)
  (setq register-alist nil)

  (set-register ?z '(file . "~/git/"))
  ;; (set-register ?f '(window-configuration 10))
  ;; (add-to-list 'desktop-globals-to-save '(window-configuration))
  )

(use-package paren
  :init
  (show-paren-mode +1))

(use-package goto-addr
  :commands (goto-address-prog-mode goto-address-mode)
  :config
  (setq goto-address-url-face 'underline)
  )

;; prog-mode-hook
(use-package prog-mode
  :init
  (setq comment-auto-fill-only-comments t)
  (global-prettify-symbols-mode +1)
  :config
  ;; (defun my-local-comment-auto-fill ()
  ;;   "Turn on comment auto fill."
  ;;   (set (make-local-variable 'comment-auto-fill-only-comments) t))
  (defun my-font-lock-comment-annotations ()
    "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|\\BUG\\):\\)"
            1 font-lock-warning-face t)))

    )
  (defun my-prog-mode-defaults ()
    "Default coding hook, useful with any programming language."
    (setq indent-tabs-mode nil)
    (turn-on-auto-fill)
    (goto-address-prog-mode +1)
    (bug-reference-prog-mode +1)
    ;; (my-local-comment-auto-fill)
    (unless (or (fboundp 'smartparens-mode)
                (fboundp 'pairedit-mode))
      (electric-pair-mode +1))
    (my-font-lock-comment-annotations)
    )
  (add-hook 'prog-mode-hook 'my-prog-mode-defaults))



;; disable feature
;; (put 'set-goal-column 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)

(diminish 'subword-mode)


(defun my/count-words-analysis (start end)
  "Count how many times each word is used in the region.
Punctuation is ignored."
  (interactive "r")
  (let (words
        alist_words_compare
        (formated "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (called-interactively-p 'my/count-words-analysis)
      (if (> (length formated) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formated 0 -2)))
        (message "No words.")))
    words))

(defun my/find-my-init-file ()
  "Edit the `my-init-file', in another window."
  (interactive)
  (find-file-other-window my-init-file))

(defun my/backward-kill-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun my/backward-kill-sexp-or-region (&optional arg)
  "Call `kill-region' when a region is active and
`backward-kill-sexp' otherwise. ARG is passed to
`backward-kill-sexp' if no region is active."
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-sexp arg)))

(defun my/rename-file (filename &optional new-filename)
  "Rename FILENAME to NEW-FILENAME.

When NEW-FILENAME is not specified, asks user for a new name.

Also renames associated buffer (if any exists), invalidates
projectile cache when it's possible and update recentf list."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let* ((buffer (find-buffer-visiting filename))
           (short-name (file-name-nondirectory filename))
           (new-name (if new-filename new-filename
                       (read-file-name
                        (format "Rename %s to: " short-name)))))
      (cond ((get-buffer new-name)
             (error "A buffer named '%s' already exists!" new-name))
            (t
             (let ((dir (file-name-directory new-name)))
               (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                 (make-directory dir t)))
             (rename-file filename new-name 1)
             (when buffer
               (kill-buffer buffer)
               (find-file new-name))
             (when (fboundp 'recentf-add-file)
               (recentf-add-file new-name)
               (recentf-remove-if-non-kept filename))
             (when (projectile-project-p)
               (call-interactively #'projectile-invalidate-cache))
             (message "File '%s' successfully renamed to '%s'" short-name
                      (file-name-nondirectory new-name)))))))


;; from magnars
(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        ;; (error "Buffer '%s' is not visiting a file!" name)
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (projectile-project-p)
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun my/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

FILENAME is file or directory.
When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p (format "Are you sure you want to delete %s? " filename)))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

(defun my/delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `my/delete-file' function.."
  (interactive "f")
  (funcall-interactively #'my/delete-file filename t))

;; from magnars
(defun my/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer)
      (when (yes-or-no-p (format "Are you sure you want to delete %s? " filename))
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "Deleted file %s" filename)))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))


(defun my/narrow-or-widen-dwim (p)
  "Ifconfig the buffer is narrowed, it windens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually Call `org-edit-src-code'.

With prefix P, dont' widen, just narrow even if buffer is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-window))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))
(define-key ctl-x-map (kbd "n") #'my/narrow-or-widen-dwim)

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))


(use-package find-file
  :defer t
  :config
  ;; overwirte to fix bug will create filename directory
  (defun ff-find-the-other-file (&optional in-other-window)
    "Find the header or source file corresponding to the current file.
  Being on a `#include' line pulls in that file, but see the help on
  the `ff-ignore-include' variable.

  If optional IN-OTHER-WINDOW is non-nil, find the file in another window."

    (let (match           ;; matching regexp for this file
          suffixes        ;; set of replacing regexps for the matching regexp
          action          ;; function to generate the names of the other files
          fname           ;; basename of this file
          pos             ;; where we start matching filenames
          stub            ;; name of the file without extension
          alist           ;; working copy of the list of file extensions
          pathname        ;; the pathname of the file or the #include line
          default-name    ;; file we should create if none found
          format          ;; what we have to match
          found           ;; name of the file or buffer found - nil if none
          dirs            ;; local value of ff-search-directories
          no-match)       ;; whether we know about this kind of file
      (run-hooks 'ff-pre-find-hook 'ff-pre-find-hooks)
      (message "Working...")
      (setq dirs
            (if (symbolp ff-search-directories)
                (ff-list-replace-env-vars (symbol-value ff-search-directories))
              (ff-list-replace-env-vars ff-search-directories)))
      (setq fname (ff-treat-as-special))
      (cond
       ((and (not ff-ignore-include) fname)
        (setq default-name fname)
        (setq found (ff-get-file dirs fname nil in-other-window)))
       ;; let's just get the corresponding file
       (t
        (setq alist (if (symbolp ff-other-file-alist)
                        (symbol-value ff-other-file-alist)
                      ff-other-file-alist)
              pathname (if (buffer-file-name)
                           (buffer-file-name)
                         "/none.none"))
        (setq fname (file-name-nondirectory pathname)
              no-match nil
              match (car alist))

        ;; find the table entry corresponding to this file
        (setq pos (ff-string-match (car match) fname))
        (while (and match (if (and pos (>= pos 0)) nil (not pos)))
          (setq alist (cdr alist))
          (setq match (car alist))
          (setq pos (ff-string-match (car match) fname)))

        ;; no point going on if we haven't found anything
        (if (not match)
            (setq no-match t)

          ;; otherwise, suffixes contains what we need
          (setq suffixes (car (cdr match))
                action (car (cdr match))
                found nil)

          ;; if we have a function to generate new names,
          ;; invoke it with the name of the current file
          (if (and (atom action) (fboundp action))
              (progn
                (setq suffixes (funcall action (buffer-file-name))
                      match (cons (car match) (list suffixes))
                      stub nil
                      default-name (car suffixes)))

            ;; otherwise build our filename stub
            (cond
             ;; get around the problem that 0 and nil both mean false!
             ((= pos 0)
              (setq format "")
              (setq stub "")
              )
             (t
              (setq format (concat "\\(.+\\)" (car match)))
              (string-match format fname)
              (setq stub (substring fname (match-beginning 1) (match-end 1)))
              ))
            ;; if we find nothing, we should try to get a file like this one
            (setq default-name
                  (concat stub (car (car (cdr match))))))
          ;; do the real work - find the file
          (setq found
                (ff-get-file dirs
                             stub
                             suffixes
                             in-other-window)))))
      (cond
       (no-match                     ;; could not even determine the other file
        (message ""))
       (t
        (cond
         ((not found)                ;; could not find the other file
          (run-hooks 'ff-not-found-hook 'ff-not-found-hooks)
          (cond
           (ff-always-try-to-create  ;; try to create the file
            (let (name pathname)
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; fix with helm will create file in directory ;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (setq name
                    (expand-file-name default-name
                                      (read-directory-name
                                       (format "Find or create %s in: " default-name)
                                       default-directory)))
              (setq pathname
                    (if (file-directory-p name)
                        (concat (file-name-as-directory name) default-name)
                      (setq found name)))

              (ff-find-file pathname in-other-window t)))

           (t                        ;; don't create the file, just whinge
            (message "No file found for %s" fname))))

         (t                          ;; matching file found
          nil))))
      found))
  )


;; TODO: keymaps
;; ==================== FILE ====================
(defvar my-file-keymap
  (let ((map (make-sparse-keymap nil)))
    (define-key map (kbd "f") 'find-file)
    (define-key map (kbd "C-f") 'find-file-other-window)
    (define-key map (kbd "n") 'find-alternate-file)
    (define-key map (kbd "C-n") 'find-alternate-file-other-window)
    (define-key map (kbd "v") 'view-file)
    (define-key map (kbd "C-v") 'view-file-other-window)
    (define-key map (kbd "m") 'find-file-root)

    (define-key map (kbd "a") 'append-to-file)
    (define-key map (kbd "i") 'insert-file)
    (define-key map (kbd "d") 'delete-file)
    (define-key map (kbd "w") 'write-file)
    (define-key map (kbd "r") 'my/rename-current-buffer-file)
    (define-key map (kbd "C-r") 'my/rename-file)
    (define-key map (kbd "k") 'my/delete-current-buffer-file)
    (define-key map (kbd "C-k") 'delete-file)
    (define-key map (kbd  "R") 'read-only-mode)

    (define-key map (kbd "I") 'crux-find-user-init-file)
    (define-key map (kbd "S") 'crux-find-shell-init-file)
    (define-key map (kbd "U") 'crux-find-user-custom-file)
    (define-key map (kbd "M") 'my/find-my-init-file)

    (define-key map (kbd "c d") 'my/dos2unix)
    (define-key map (kbd "c u") 'my/unix2dos)
    (define-key map (kbd "c r") 'my/dos2unix-remove-M)
    map)
  "My file group keymap.")
(define-key ctl-x-map "f" my-file-keymap)

;; ==================== BUFFER ====================
(global-set-key (kbd "C-c C-r") 'revert-buffer)

;; ==================== HELP ====================
;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; ==================== EDIT ====================
;; 打开标记
;; C-u C-SPC to pop
(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-.") 'mark-sexp)
(global-set-key (kbd "C-,") 'mark-word)
;; pop mark
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
;; equal C-u C-SPC
(global-set-key (kbd "C-x C-,") 'pop-to-mark-command)

(global-unset-key (kbd "C-x SPC"))
(global-set-key (kbd "C-<return>") 'cua-rectangle-mark-mode)

;; 删除光标之前的单词(保存到kill-ring)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-w") 'my/backward-kill-sexp-or-region)
;;删除光标之前的字符(不保存到kill-ring)
(global-set-key (kbd "C-q") 'backward-delete-char)
;;删除选中区域
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-M-<backspace>") 'my/backward-kill-to-indentation)
;;M-j来连接不同行
(global-set-key (kbd "M-j") 'join-line)

;; ====================MACRO====================
(global-set-key (kbd "C-x K") 'kmacro-keymap)
(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro-repeat)

;; ==================== MISC ====================
;; 列出正在运行的进程
(global-set-key (kbd "C-x M-p") 'list-processes)

;; s-q 来插入转义字符
(global-set-key (kbd "s-q") 'quoted-insert)

(global-set-key (kbd "RET") 'newline-and-indent)


(provide 'my-edit)
;;; my-edit.el ends here
