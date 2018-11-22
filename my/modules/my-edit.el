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

;;打开图片显示功能
(auto-image-file-mode t)
;; (when (executable-find "convert")
;;   (setq imagemagick-render-type 1))

;;在鼠标光标处插入
(setq mouse-yank-at-point t)

;; no blink
(blink-cursor-mode -1)

;; 光标靠近鼠标指针时，鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;; (setq ring-bell-function 'ignore
;;       visible-bell nil)
;; 响铃
(defun my--flash-mode-line ()
  "My visible bell."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my--flash-mode-line)


;; 支持emacs和外部程序的拷贝粘贴
(setq-default x-select-enable-clipboard t)

;; smooth scrolling
(setq scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; 递归minibuffer
(setq enable-recursive-minibuffers t)

;; resize mini-window to fit the text displayed in them
(setq resize-mini-windows nil)

;; suggest key bindings
(setq suggest-key-bindings t)

;; default major mode
(setq-default default-major-mode 'text-mode)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)

;; message max
(setq message-log-max 16384)

;;设置删除记录
(setq kill-ring-max 200)

;; 锁定行高
(setq resize-mini-windows nil)

;; 行距
(setq line-spacing 0.0)

;; 显示80行就换行
(setq fill-column 80)
(setq-default adaptive-fill-regexp
	      "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+\\|\\([0-9]+\\.\\)[ \t]*\\)*")

;; Show column number in mode line
(column-number-mode +1)
(line-number-mode +1)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; tab width
(setq-default default-tab-width 4)

;; don't use tab
(setq indent-tabs-mode nil)

;; add final newline
(setq require-final-newline t)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(when (my-system-is-mswindows)
  (setq delete-by-moving-to-trash t))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)
(setq-default sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq set-mark-command-repeat-pop t)
(transient-mark-mode +1)

(delete-selection-mode +1)
;; Keep focus while navigating help buffers
(setq help-window-select 'nil)

;; imenu
(setq-default imenu-auto-rescan t)

;; Don't try to ping things that look like domain names
(setq-default ffap-machine-p-known 'reject)

;; 在标题栏提示当前位置
(setq frame-title-format
      '( "Emacs - " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))
;; initial scarch message
(setq-default initial-scratch-message
              (concat ";; Happy Hacking, "
                      user-login-name
                      (if user-full-name
                          (concat " ("user-full-name ")"))
                      " - Emacs ♥ you!\n\n"))

;; 现实电池状态
(use-package battery
  :if (boundp 'battery-status-function)
  :config
  (display-battery-mode t)
  )

;; linum
;;显示行列号
(use-package linum
  :init
  (global-linum-mode +1)
  :init
  (setq linum-delay t)
  (setq linum-format 'dynamic)

  (my|add-toggle linum-mode
    :status linum-mode
    :on (linum-mode +1)
    :off (linum-mode -1)
    :documentation "Show line number")

  :config
  (defadvice linum-schedule (around my-linum-schedule () activate)
    "Updated line number every second."
    (run-with-idle-timer 1 nil #'linum-update-current))
  (add-hook 'prog-mode-hook 'my/toggle-linum-mode-on)
  )

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

;; 启用cua
(use-package cua-base
  :init
  ;; When called with no active region, do not activate mark.
  (defadvice cua-exchange-point-and-mark (before deactivate-mark activate compile)
    "When called with no active region, do not activate mark."
    (interactive
     (list (not (region-active-p)))))
  :config
  (setq cua-auto-mark-last-change t)
  (cua-selection-mode +1))

;;
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
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
  (setq bookmark-default-file (expand-file-name "bookmarks" my-cache-dir)))

;; abbrev config
(use-package abbrev
  :diminish abbrev-mode
  :if (file-exists-p abbrev-file-name)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" my-cache-dir))
  (abbrev-mode +1))

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
  :config
  (setq global-auto-revert-non-file-buffers t
	auto-revert-verbose nil)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
  (global-auto-revert-mode +1))

;; which func
(use-package which-func
  :config
  (which-function-mode +1))

;; whitespace 设置
(use-package whitespace
  :commands (whitespace-mode)
  :init
  ;; (set-face-attribute 'trailing-whitespace nil
  ;; 					  :background
  ;; 					  (face-attribute 'font-lock-warning-face
  ;; 									  :foreground))
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

  (my|add-toggle whitespace-cleanup-mode
    :status whitespace-cleanup-mode
    :on (whitespace-cleanup-mode +1)
    :off (whitespace-cleanup-mode -1)
    :documentation "Cleanup whitesapce")

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

  (require 'my-whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode +1)

  ;; (set-face-attribute 'whitespace-space nil
  ;; 					  :background nil
  ;; 					  :foreground (face-attribute 'font-lock-warning-face
  ;; 												  :foreground))
  ;; (set-face-attribute 'whitespace-tab nil
  ;; 					  :background nil)
  ;; (set-face-attribute 'whitespace-indentation nil
  ;; 					  :background nil)
  )

;;拷贝来的代码自动格式化
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

(defvar my-yank-indent-threshold 3000)
(defvar my-yank-indent-modes '(LaTex-mode
			       TeX-mode
			       latex-mode
			       nxml-mode
			       html-mode
			       web-mode))
;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation from BEG to END, as long as the region isn't too large."
  (if (<= (- end beg) my-yank-indent-threshold)
      (indent-region beg end nil)))

(my|advise-commands "indent" (yank yank-pop) after
		    "If current mode is one of `my-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                    (if (and (not (ad-get-arg 0))
                             (not (member major-mode my-indent-sensitive-modes))
                             (or (derived-mode-p 'prog-mode)
				 (derived-mode-p 'text-mode)
                                 (member major-mode my-yank-indent-modes)))
                        (let ((transient-mark-mode nil))
                          (yank-advised-indent-function (region-beginning) (region-end)))))

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

;;; grep 默认递归查找
(use-package grep
  :commands (grep-mode
	     grep
	     grep-find
	     find-grep
	     lgrep
	     rgrep
	     zrgrep
	     rzgrep)
  :config
  (setq grep-command "grep --color -nH -r -E "
	grep-highlight-matches t
	grep-scroll-output t
	))

(when (executable-find "ag")
  (use-package ag
    :ensure t
    :bind ("M-?" . ag)
    :config
    (use-package wgrep-ag
      :ensure t)
    (setq ag-highlight-search t)
    )
  )

;; 设置默认浏览器为firefox
;; (setq browse-url-firefox-new-window-is-tab t)
;; (setq browse-url-firefox-program "firefox")


;;;启动时的默认模式
;; (setq-default initial-major-mode
;;               'lisp-interaction-mode)

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
	tags-add-tables nil		  ;don't ask user
	))

;; disable feature
;; (put 'set-goal-column 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)

;; calendar
(use-package calendar
  :commands (calendar)
  :config
  (setq
   calendar-date-style (quote iso)
   calendar-mark-holidays-flag t
   calendar-chinese-all-holidays-flag t))

;; set buffer major mode accroding to auto-mode-alist
(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))


;;When called with no active region, call FUNC on current buffer.
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

;; When called with no active region, do not activate mark.
(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(use-package compile
  :commands (compile my/smart-compile
		     my/insert-compile-command)
  :init
  (require 'files-x)
  (defun my/insert-compile-command ()
    "Insert compile command to file."
    (interactive)
    (save-excursion
      (modify-file-local-variable-prop-line 'compile-command (eval compile-command)
					    `add-or-replace)))

  (defun my/smart-compile()
    "比较智能的C/C++编译命令
如果当前目录有makefile则用make -k编译，否则，如果是
处于c-mode，就用clang -Wall编译，如果是c++-mode就用
clang++ -Wall编译"
    (interactive)
    ;; do save
    (setq-local compilation-directory default-directory)
    (save-some-buffers (not compilation-ask-about-save)
		       compilation-save-buffers-predicate)
    ;; find compile-command
    (let ((command (eval compile-command))
	  (candidate-make-file-name '("makefile" "Makefile" "GNUmakefile" "GNUMakefile")))
      (if (string-prefix-p "make" command)
	  (unless (find t candidate-make-file-name :key
			'(lambda (f) (file-readable-p f)))
	    (cond ((eq major-mode 'c-mode)
		   (setq command
			 (concat "clang -Wall -o "
				 (file-name-sans-extension
				  (file-name-nondirectory buffer-file-name))
				 " "
				 (file-name-nondirectory buffer-file-name)
				 " -g ")))
		  ;; c++-mode
		  ((eq major-mode 'c++-mode)
		   (setq command
			 (concat "clang++ -Wall -o "
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
	    (my/insert-compile-command)))
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

;; highlight
(use-package hi-lock
  :commands (hi-lock-mode global-hi-lock-mode)
  :diminish hi-lock-mode
  :bind (:map hi-lock-map
	      ("C-c o l" . highlight-lines-matching-regexp)
	      ("C-c o i" . hi-lock-find-patterns)
	      ("C-c o r" . highlight-regexp)
	      ("C-c o p" . highlight-phrase)
	      ("C-c o ." . highlight-symbol-at-point)
	      ("C-c o u" . unhighlight-regexp)
	      ("C-c o b" . hi-lock-write-interactive-patterns)
	      ))

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
  (setq comment-fill-column 80)
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


(use-package diminish
  :ensure t)

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
  (setq fci-rule-width 1
	;; fci-rule-color "#D0BF8F"
	)
  (push '(fci-mode "") minor-mode-alist)
  )

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :commands (aggressive-indent-mode)
  :init
  (my|add-toggle aggressive-indent-mode
    :mode aggressive-indent-mode
    :documentation "Always keep code indent.")
  (aggressive-indent-mode +1))

;; expand-region
(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind (("C-=" . er/expand-region))
  :config
  (setq expand-region-contract-fast-key ",")
  (setq expand-region-smart-cursor nil)
  )

;; page-break-lines "s-q C-l"
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode +1)
  (add-hook 'prog-mode-hook 'page-break-lines-mode))

;; smarter kill-ring navigation
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings)
  )

;; projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
	      ("A a" . projectile-add-known-project)
	      ("A d" . projectile-remove-known-project))
  :ensure t
  :config
  (setq projectile-sort-order 'recentf
        projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-prefix " Project")
  (projectile-mode +1)
  )

;; diff-hl
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode global-diff-hl-mode)
  :init
  (my|add-toggle diff-hl-mode
    :mode diff-hl-mode
    :documentation "Highlight diff")
  (my|add-toggle global-diff-hl-mode
    :mode global-diff-hl-mode
    :documentation "Global highlight diff")
  :config
  (global-diff-hl-mode +1)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)
  :config
  ;; fix undo-tree maybe cause menu-bar can't work in scrach buffer
  ;; (add-to-list 'undo-tree-incompatible-major-modes 'lisp-interaction-mode)
  (global-undo-tree-mode +1))

;; easy-kill
(use-package easy-kill
  :ensure t
  :commands (easy-kill easy-mark)
  :bind (([remap kill-ring-save] . easy-kill))
  ;; :config
  ;; (global-set-key [remap kill-ring-save] 'easy-kill)
  ;; (global-set-key [remap mark-sexp] 'easy-mark)
  )

;; use settings from .editorconfig file when present
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode +1)
  )

;;; indent-guide
(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :commands (indent-guide-mode)
  :init
  (my|add-toggle indent-guide-mode
    :mode indent-guide-mode
    :documentation "Show indent"
    )
  (add-hook 'python-mode-hook 'indent-guide-mode)
  :config
  (setq indent-guide-delay 0.3)
  )

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)
	 ("C-c M <" . mc/mark-previous-like-this)
	 ("C-c M >" . mc/mark-next-like-this)
	 ("C-c M C-<" . mc/mark-all-like-this)
	 ("C-c M r" . set-rectangular-region-anchor)
	 ("C-c M c" . mc/edit-ines)
	 ("C-c M e" . mc/edit-ends-of-lines)
	 ("C-c M a" . mc/edit-beginnings-of-lines)
	 ))

;; discover-my-major
(use-package discover-my-major
  :bind (("C-h RET" . discover-my-major))
  :ensure t)

;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :diminish symbol-overlay-mode
  :bind (:map symbol-overlay-mode-map
	      ("M-n" . symbol-overlay-jump-next)
	      ("M-p" . symbol-overlay-jump-prev))
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  :config
  (set-face-attribute 'symbol-overlay-default-face nil
  		      :background
  		      (face-attribute 'isearch
  				      :background)
  		      :foreground
  		      (face-attribute 'isearch
  				      :foreground)))

(use-package google-translate
  :ensure t
  :defer t
  :config
  (defun my/set-google-translate-languages (source target)
    "Set source language for google translate.
For instance pass En as source for English."
    (interactive
     "sEnter source language (ie. en): \nsEnter target language (ie. zh-CN): "
     source target)
    (message
     (format "Set google translate source language to %s and target to %s"
	     source target))
    (setq google-translate-default-source-language (downcase source))
    (setq google-translate-default-target-language (downcase target)))
  (setq google-translate-enable-ido-completion t)
  (setq google-translate-show-phonetic t)
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "zh-CN"))

;; GTAGS
;; (use-package ggtags
;;   :ensure t
;;   :commands (ggtags-mode ggtags-find-project ggtags-find-tag-dwim)
;;   :bind
;;   (:map ggtags-mode-map
;; 		("C-c g s" . ggtags-find-other-symbol)
;; 		("C-c g h" . ggtags-view-tag-history)
;; 		("C-c g r" . ggtags-find-reference)
;; 		("C-c g f" . ggtags-find-file)
;; 		("C-c g c" . ggtags-create-tags)
;; 		("C-c g u" . ggtags-update-tags)
;; 		("C-c g a" . helm-gtags-tags-in-this-function)
;; 		("C-c g ." . ggtags-find-tag-dwim)
;; 		("C-c g g" . ggtags-find-definition)
;; 		;; ("M-." . ggtags-find-tag-dwim)
;; 		;; ("M-," . pop-tag-mark)
;; 		("C-c <" . ggtags-prev-mark)
;; 		("C-c >" . ggtags-next-mark)
;; 		)
;;   :init
;;   (add-hook 'c-mode-common-hook
;; 			(lambda ()
;; 			  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;; 				(ggtags-mode +1))))
;;   )

(use-package goto-addr
  :commands (goto-address-prog-mode goto-address-mode)
  :config
  (setq goto-address-url-face 'underline)
  )

;; prog-mode-hook
(use-package prog-mode
  :config
  (defun my-local-comment-auto-fill ()
    "Turn on comment auto fill."
    (set (make-local-variable 'comment-auto-fill-only-comments) t))
  (defun my-font-lock-comment-annotations ()
    "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
    (font-lock-add-keywords
     nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|\\BUG\\):\\)"
	    1 font-lock-warning-face t))))
  (defun my-prog-mode-defaults ()
    "Default coding hook, useful with any programming language."
    (goto-address-prog-mode +1)
    (bug-reference-prog-mode +1)
    (my-local-comment-auto-fill)
    (my-font-lock-comment-annotations))
  (add-hook 'prog-mode-hook 'my-prog-mode-defaults))




;; 添加百度搜索
(defun my-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro my|install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "my/%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any." search-engine-name)
     (interactive)
     (my-search ,search-engine-url ,search-engine-prompt)))
(my|install-search-engine "baidu" "https://www.baidu.com/s?ie=UTF-8&w=" "Baidu: ")
;; (my|install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
;; (my|install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
;; (my|install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
;; (my|install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")



;; large file
(defvar my-large-file-size large-file-warning-threshold
  "Maximum size of file above which a confirmation is requested.
When nil, never request confirmation.")
(defcustom my-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "Major modes which `spacemacs/check-large-file' will not be automatically applied to."
  :group 'my
  :type '(list symbol))
;; check when opening large files - literal file open
(defun my-check-large-file ()
  "Check when opening large files - literal file open."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode my-large-file-modes-list))
           size (> size (* 1024 1024 my-large-file-size))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
;; Prompt to open file literally if large file.
(add-hook 'find-file-hook 'my-check-large-file)

(defun my/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1)
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (setq p1 (point))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

(defun my/insert-current-time-string ()
  "Insert the current time."
  (interactive "*")
  (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
;; (insert (format-time-string "%H:%M:%S" (current-time))))

(defun my/dos2unix-remove-M()
  "Remove ^M in files."
  (interactive)
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match "")))

(defun my/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

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
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formated 0 -2)))
        (message "No words.")))
    words))


(use-package find-file
  :defer t
  :config
  ;;  FIXME: overwirte to fix bug
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


;; ==================== FILE ====================

(defvar my-file-prefix-map (make-sparse-keymap))
(define-key ctl-x-map "f" my-file-prefix-map)
(global-set-key (kbd "C-x f") nil)
(global-set-key (kbd "C-x f a") 'append-to-file)
(global-set-key (kbd "C-x f i") 'insert-file)
(global-set-key (kbd "C-x f f") 'find-file)
(global-set-key (kbd "C-x f r") 'find-file-read-only)
(global-set-key (kbd "C-x f C-f") 'find-file-other-window)
(global-set-key (kbd "C-x f d") 'delete-file)
(global-set-key (kbd "C-x f n") 'find-alternate-file)
(global-set-key (kbd "C-x f C-n") 'find-alternate-file-other-window)
(global-set-key (kbd "C-x f v") 'view-file)
(global-set-key (kbd "C-x f C-v") 'view-file-other-window)
(global-set-key (kbd "C-x f c d") 'my/dos2unix)
(global-set-key (kbd "C-x f c u") 'my/unix2dos)

;; ==================== HELP ====================
;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; ==================== EDIT ====================
;; 标记段落
(global-set-key (kbd "M-h") 'mark-paragraph)
;; 标记全篇
(global-set-key (kbd "C-x h") 'mark-whole-buffer)
;; 标记一行
(global-set-key (kbd "C-.") 'mark-sexp)
;; 标记word
(global-set-key (kbd "C-,") 'mark-word)
;; 打开标记
;; (global-set-key (kbd "C-SPC") 'cua-set-mark)
;; (global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-SPC") 'set-mark-command) ; C-u C-SPC to pop
;;
;;跳到全局上次标记的地方(C-x C-.)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
;;跳到矩形另一端
(global-set-key (kbd "C-x C-x") 'cua-exchange-point-and-mark)
(global-set-key (kbd "C-x C-,") 'pop-to-mark-command) ;equal C-u C-SPC

;;移动到第0列
(global-set-key (kbd "M-m") 'move-beginning-of-line)
;;移动到行首第一个字符(move-beginning-of-line)
;; (global-set-key (kbd "C-a") 'back-to-indentation)

;;删除光标之前的单词(保存到kill-ring)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-w") 'backward-kill-sexp)
;; (global-set-key (kbd "C-w") 'sp-backward-kill-word)
;;删除光标之前的字符(不保存到kill-ring)
(global-set-key (kbd "C-q") 'backward-delete-char)

;;删除选中区域
(global-set-key (kbd "C-c C-k") 'kill-region)
;;删除到行首为C-0 C-k
;; (global-set-key (kbd "C-c d") 'my/kill-back-to-indentation)

;; 在当前行上，打开新的一行， M-o在当前行下打开新行
;; (global-set-key (kbd "C-o") 'my/open-line-with-reindent)

;; 删除并格式化
(global-set-key (kbd "C-M-<backspace>") 'my/kill-back-to-indentation)

;; count words
(global-set-key (kbd "M-=") 'my/count-words-analysis)

;; revert buffer
(global-set-key (kbd "C-c C-r") 'revert-buffer)

;; 切换auto fill
;; (global-set-key (kbd "C-c C-a") 'auto-fill-mode)

;;取消C-c C-c的注释功能
(global-set-key (kbd "C-c C-c") nil)
;;M-j来连接不同行
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "M-J") 'crux-top-join-line)

;; ====================MACRO====================
;; 宏 C-x C-k
;;调用宏 C-x ( 开始录制 ctrl-x ) 结束录制
(global-set-key (kbd "C-x K") 'kmacro-keymap)
(global-set-key (kbd "C-x (") 'kmacro-start-macro)
(global-set-key (kbd "C-x )") 'kmacro-end-macro)
;; (global-set-key (kbd "C-x K e") 'kmacro-end-and-call-macro)
;; (global-set-key (kbd "C-x K s") 'kmacro-start-macro)
;; (global-set-key (kbd "C-x K k") 'kmacro-end-macro)
;; (global-set-key (kbd "C-x K c") 'kmacro-call-macro)
(global-set-key (kbd "<f3>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f4>") 'kmacro-end-or-call-macro-repeat)

;; ==================== MISC ====================
;; woman
(global-set-key (kbd "C-c h m") 'man)

;; 列出正在运行的进程
(global-set-key (kbd "C-x M-p") 'list-processes)

;; 百度搜索
;; (global-set-key (kbd "C-c C-s") 'my-baidu)

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

;; s-q 来插入转义字符
(global-set-key (kbd "s-q") 'quoted-insert)

(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-x C-m") 'execute-extended-command)
;; (global-set-key (kbd "C-x m") 'execute-extended-command)
;; (global-set-key (kbd "C-x f") 'ido-find-file)
;;重新绑定发送邮件为C-x M
;; (global-set-key (kbd "C-x M") 'compose-mail)

;;插入buffer
;; (global-set-key (kbd "C-x I") 'ido-insert-buffer)
;; (global-set-key (kbd "C-x C-SPC") 'ido-switch-buffer-other-window)
;; (global-set-key (kbd "C-x SPC") 'ido-switch-buffer-other-window)
;; (global-set-key (kbd "C-x C-d") 'ido-dired)
;; (global-set-key (kbd "C-x C-k") 'ido-kill-buffer)

;;(global-set-key (kbd "M-\\") 'delete-horizontal-space)


;; (global-set-key (kbd "C-M-/") 'completion-at-point)

;; (global-set-key (kbd "C-x [") 'switch-to-prev-buffer)
;; (global-set-key (kbd "C-x ]") 'switch-to-next-buffer)

(provide 'my-edit)
;;; my-edit.el ends here
