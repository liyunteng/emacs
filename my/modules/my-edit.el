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

;; disable ad redefinition warning
(setq ad-redefinition-action 'accept)

;;打开图片显示功能
(auto-image-file-mode t)

;;不要在鼠标光标处插入
(setq mouse-yank-at-point t)

;; 支持emacs和外部程序的拷贝粘贴
(setq-default x-select-enable-clipboard t)

;; smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; 光标靠近鼠标指针时，鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;; Show a marker in the left fringe for lines not in the buffuer
(setq indicate-empty-lines t)

;; message max
(setq message-log-max 16384)

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; buffer menu
(setq buffers-menu-max-size 10)

;;设置删除记录
(setq kill-ring-max 2000)

;; 递归minibuffer
(setq enable-recursive-minibuffers t)

;; M-x command 显示绑定的键
(setq suggest-key-bindings t)

;; 锁定行高
(setq resize-mini-windows t)

;; 设定行距
(setq line-spacing 0.0)

;; 显示80行就换行
(setq fill-column 80)

;; Show column number in mode line
(setq column-number-mode t)
(setq line-number-mode t)

;; highlight current line
(global-hl-line-mode +1)

;; imenu
(set-default 'imenu-auto-rescan t)

;; no blink
(blink-cursor-mode -1)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; tab width
(setq tab-width 4)

;; don't use tab
(setq indent-tabs-mode nil)

;; add final newline
(setq require-final-newline t)

(delete-selection-mode t)

(setq set-mark-command-repeat-pop t)

;; Scroll compilation to first error or end
(setq-default compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq-default ffap-machine-p-known 'reject)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(when (my-system-is-mswindows)
  (setq delete-by-moving-to-trash t))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(transient-mark-mode +1)

;; 现实电池状态
(require 'battery)
(if (fboundp 'battery-status-function)
    (display-battery-mode t) nil)

;; 在标题栏提示当前位置
(setq frame-title-format
      '( "Emacs - " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))
;; initial scarch message
(setq-default initial-scratch-message
              (concat ";; Happy hacking, "
		      user-login-name
		      (if user-full-name
			  (concat " ("user-full-name ")"))
		      " - Emacs ♥ you!\n\n"))


;; linum
;;显示行列号
(require 'linum)
(setq linum-format 'dynamic)
(global-linum-mode 'linum-mode)
(defvar my-linum-mode-inhibit-modes-list
  '(eshell-mode
    shell-mode
    profiler-report-mode
    ffip-diff-mode
    dictionary-mode
    erc-mode
    browse-kill-ring-mode
    etags-select-mode
    dired-mode
    help-mode
    text-mode
    fundamental-mode
    jabber-roster-mode
    jabber-chat-mode
    inferior-js-mode
    inferior-python-mode
    inferior-scheme-mode
    ivy-occur-grep-mode ; for better performance
    twittering-mode
    compilation-mode
    weibo-timeline-mode
    woman-mode
    Info-mode
    calc-mode
    calc-trail-mode
    comint-mode
    gnus-group-mode
    inf-ruby-mode
    gud-mode
    org-mode
    vc-git-log-edit-mode
    log-edit-mode
    term-mode
    w3m-mode
    speedbar-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode))
(defadvice linum-on (around my-linum-on-inhibit-for-modes)
  "Stop the load of `linum-mode' for some major modes."
  (unless (member major-mode my-linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)

(setq-default linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  "Updated line number every second."
  (run-with-idle-timer 1 nil #'linum-update-current))

;; 启用cua
(require 'cua-base)
(setq cua-auto-mark-last-change t)
(cua-selection-mode t)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; activate it for all buffers
(if (< emacs-major-version 25)
    (progn (require 'saveplace)
           (setq-default save-place t))
  (save-place-mode 1))

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; ediff
(setq-default ediff-split-window-function 'split-window-horizontally
          ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; bookmark
(require 'bookmark)
(setq bookmark-save-flag 1)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)


;; Auto revert
(require 'autorevert)
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)


;; (setq ring-bell-function 'ignore
;;       visible-bell nil)
;; 响铃
(defun my--flash-mode-line ()
  "My visible bell."
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))
(setq ring-bell-function 'my--flash-mode-line)

;; which func
(require 'which-func)
(which-function-mode +1)

;; whitespace 设置
(require 'whitespace)
(my-require-package 'whitespace-cleanup-mode)
(require 'whitespace-cleanup-mode)
(setq whitespace-line-column fill-column)
(setq whitespace-style
      '(face trailing lines-tail empty
             space-before-tab::space newline
             indentation::space space-after-tab::space))
(global-whitespace-mode +1)
(global-whitespace-cleanup-mode +1)
(diminish 'whitespace-cleanup-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq show-trailing-whitespace t)
(when show-trailing-whitespace
  (set-face-attribute 'trailing-whitespace nil
                      :background
                      (face-attribute 'font-lock-comment-face
                                      :foreground))
  (set-face-attribute 'whitespace-space nil
                      :background nil
                      :foreground (face-attribute 'font-lock-warning-face
                                                  :foreground))
  (set-face-attribute 'whitespace-tab nil
                      :background nil)
  (set-face-attribute 'whitespace-indentation nil
                      :background nil))
(defun my-no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
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
  (add-hook hook #'my-no-trailing-whitespace))


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
(defvar my-yank-indent-modes '(LaTex-mode TeX-mode))
(dolist (mode '(latex-mode
                plain-tex-mode
                nxml-mode
                html-mode
                web-mode))
  (add-to-list 'my-yank-indent-modes mode))
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
                 (member major-mode my-yank-indent-modes)))
            (let ((transient-mark-mode nil))
              (yank-advised-indent-function (region-beginning) (region-end)))))

;; hippie
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
        try-complete-lisp-symbol))

;; proced
(setq-default proced-auto-update-flag t)
(setq-default proced-auto-update-interval 3)
(setq-default proced-post-display-hook (quote (fit-window-to-buffer)))

;;; grep 默认递归查找
;; (setq-default grep-command "grep --color -nH -r -E ")
(setq-default grep-highlight-matches t)
(setq-default grep-scroll-output t)
(when (executable-find "ag")
  (my-require-package 'ag)
  (my-require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag)
  )

;; 设置默认浏览器为firefox
;; (setq browse-url-firefox-new-window-is-tab t)
;; (setq browse-url-firefox-program "firefox")

;;;设置git diff的样式
;; (setq magit-diff-options
;;       (quote
;;        ("--minimal" "--patience" "--histogram"
;;         "--ignore-space-change")))

;;; 设置github-clone使用的默认协议
;; (setq github-clone-url-slot :clone-url)


;;;启动时的默认模式
;; (setq-default initial-major-mode
;;               'lisp-interaction-mode)

;;;netstat命令的默认参数
(setq-default netstat-program-options '("-nap"))

;; disable feature
;; (put 'set-goal-column 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)


;; 禁用flyspell
;;(setq-default my-flyspell nil)

;; calendar
(setq-default calendar-date-style (quote iso))
(setq-default calendar-chinese-all-holidays-flag t)

;; etags
(setq-default tags-revert-without-query t)
(setq-default tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive


;; set buffer major mode accroding to auto-mode-alist
(defadvice my-set-buffer-major-mode (after set-major-mode activate compile)
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
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)


;; When called with no active region, do not activate mark.
(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

;; Compilation from Emacs
(defun my-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)


;; highlight
(after-load 'hi-lock
  (define-key hi-lock-map (kbd "C-c o l") 'highlight-lines-matching-regexp)
  (define-key hi-lock-map (kbd "C-c o i") 'hi-lock-find-patterns)
  (define-key hi-lock-map (kbd "C-c o r") 'highlight-regexp)
  (define-key hi-lock-map (kbd "C-c o p") 'highlight-phrase)
  (define-key hi-lock-map (kbd "C-c o .") 'highlight-symbol-at-point)
  (define-key hi-lock-map (kbd "C-c o u") 'unhighlight-regexp)
  (define-key hi-lock-map (kbd "C-c o b") 'hi-lock-write-interactive-patterns))


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
  :group 'spacemacs
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


;; align
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
  (require 'align)
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


(my-require-package 'diminish)
(require 'diminish)

;; expand-region
(my-require-package 'expand-region)
(require 'expand-region)
(setq expand-region-contract-fast-key ",")
(setq expand-region-smart-cursor nil)

;; page-break-lines
(my-require-package 'page-break-lines)
(require 'page-break-lines)
(global-page-break-lines-mode t)

;; smarter kill-ring navigation
(my-require-package 'browse-kill-ring)
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "M-y") 'browse-kill-ring)

;; projectile
(my-require-package 'projectile)
(require 'projectile)
(projectile-mode +1)

;; diff-hl
(my-require-package 'diff-hl)
(require 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; undo-tree
(my-require-package 'undo-tree)
(require 'undo-tree)
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t
      undo-tree-auto-save-history t)
(global-undo-tree-mode +1)
(diminish 'undo-tree-mode)

;; easy-kill
(my-require-package 'easy-kill)
(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap mark-sexp] 'easy-mark)


;; use settings from .editorconfig file when present
(my-require-package 'editorconfig)
(require 'editorconfig)
(editorconfig-mode +1)
(diminish 'editorconfig-mode)

;;; indent-guide
(my-require-package 'indent-guide)
(add-hook 'prog-mode-hook 'indent-guide-mode)
(after-load 'indent-guide (diminish 'indent-guide-mode))

;; multiple-cursors
(my-require-package 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; conflict witch text-scale-increase
;; (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;; smartparens
(my-require-package 'smartparens)
(require 'smartparens)
(show-smartparens-global-mode +1)
(add-hook 'nxml-mode-hook 'smartparens-mode)

;; discover-my-major
(my-require-package 'discover-my-major)

;; symbol-overlay
(my-require-package 'symbol-overlay)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'symbol-overlay-mode))
(after-load 'symbol-overlay
  (diminish 'symbol-overlay-mode "so")
  (set-face-attribute 'symbol-overlay-temp-face nil
                      :background
                      (face-attribute 'isearch
                                      :background)
                      :foreground
                      (face-attribute 'isearch
                                      :foreground))
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

;; flycheck
;; enable on-the-fly syntax checking
(my-require-package 'flycheck)
(require 'flycheck)
(if (fboundp 'global-flycheck-mode)
    (progn
      (global-flycheck-mode +1)
      (add-hook 'prog-mode-hook 'flycheck-mode)
      (when (display-graphic-p)
    (progn
      (my-require-package 'flycheck-pos-tip)
      (require 'flycheck-pos-tip)
      (flycheck-pos-tip-mode 1)))))

;; GTAGS
(my-require-package 'ggtags)
(after-load 'ggtags
  (defun my-gtags-ext-produce-tags-if-needed (dir)
    (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
        (let ((default-directory dir))
          (shell-command "gtags")
          (message "tagfile created by GNU Global"))
      ;;  tagfile already exists; update it
      (shell-command "global -u")
      (message "tagfile updated by GNU Global")))

  ;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
  (defun my/gtags-ext-create-or-update ()
    "create or update the gnu global tag file"
    (interactive)
    (my-gtags-ext-produce-tags-if-needed (read-directory-name
                                          "gtags: top of source tree:" default-directory)))

  (defun my/gtags-ext-add-gtagslibpath (libdir &optional del)
    "add external library directory to environment variable GTAGSLIBPATH.\ngtags will can that directory if needed.\nC-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
    (interactive "DDirectory containing GTAGS:\nP")
    (let (sl)
      (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
          ;; create tags
          (let ((default-directory libdir))
            (shell-command "gtags")
            (message "tagfile created by GNU Global")))

      (setq libdir (directory-file-name libdir)) ;remove final slash
      (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
      (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
      (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":"))
      ))

  (defun my/gtags-ext-print-gtagslibpath ()
    "print the GTAGSLIBPATH (for debug purpose)"
    (interactive)
    (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH"))))

(diminish 'hide-ifdef-hiding)
(diminish 'beacon-mode)
(diminish 'helm-mode)
(diminish 'editorconfig-mode)
(diminish 'which-key-mode)
(diminish 'rainbow-mode)
(diminish 'page-break-lines-mode)

;; prog-mode-hook
(setq-default goto-address-url-face 'underline)
(defun my-local-comment-auto-fill ()
  "Turn on comment auto fill."
  (set (make-local-variable 'comment-auto-fill-only-comments) t))
(defun my-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))
(defun my-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (goto-address-prog-mode)
  (bug-reference-prog-mode)
  (smartparens-mode +1)
  (my-local-comment-auto-fill)
  (my-font-lock-comment-annotations))
(add-hook 'prog-mode-hook 'my-prog-mode-defaults)


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
             (message "File '%s' successfully renamed to '%s'" short-name (file-name-nondirectory new-name)))))))

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
;;(define-key my-mode-map (kbd "C-c r") 'my/rename-current-buffer-file)

(defun my/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
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
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))
;;(define-key my-mode-map (kbd "C-c D") 'my/delete-current-buffer-file)

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

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))
;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments

(provide 'my-edit)
;;; my-edit.el ends here
