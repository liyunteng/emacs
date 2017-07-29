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
;; 锁定行高
(setq resize-mini-windows t)
;; 递归minibuffer
(setq enable-recursive-minibuffers t)
;; M-x command 显示绑定的键
(setq suggest-key-bindings t)
;; 现实电池状态
(require 'battery)
(if (fboundp 'battery-status-function)
    (display-battery-mode t) nil)

;; 在标题栏提示当前位置
(setq frame-title-format
      '( "Emacs - " (:eval (if (buffer-file-name)
                               (abbreviate-file-name (buffer-file-name))
                             "%b"))))

(require 'page-break-lines)
(global-page-break-lines-mode t)

(require 'expand-region)
(setq expand-region-contract-fast-key ",")
(setq expand-region-smart-cursor nil)

(require 'bookmark)
(setq bookmark-save-flag 1)

(require 'projectile)
(projectile-global-mode +1)

;;显示行列号
(require 'help-mode)
(setq-default linum-format 'dynamic)
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
  "Stop the load of linum-mode for some major modes."
  (unless (member major-mode my-linum-mode-inhibit-modes-list)
    ad-do-it))
(ad-activate 'linum-on)

;; updated line number every second
(setq-default linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 1 nil #'linum-update-current))

;;设定行距
(setq line-spacing 0.0)
;;显示80行就换行
(setq-default fill-column 80)
;;设置删除记录
(setq kill-ring-max 2000)

;; COPY FROM SPACEMACS BASE
;; Auto revert
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq-default global-auto-revert-non-file-buffers t
              auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Show column number in mode line
(setq column-number-mode t)
(setq line-number-mode t)

;; highlight current line
(global-hl-line-mode +1)

(set-default 'imenu-auto-rescan t)

;; no blink
(blink-cursor-mode -1)
;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)
;; draw underline lower
(setq x-underline-at-descent-line t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

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
(setq goto-address-url-face 'underline)
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(when (my-system-is-mswindows)
  (setq delete-by-moving-to-trash t))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; end there


;;自动换行
(defun my-auto-fill-mode-hook ()
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'my-auto-fill-mode-hook)
(add-hook 'text-mode-hook 'my-auto-fill-mode-hook)

;;拷贝来的代码自动格式化
;; (dolist (mode '(latex-mode
;;                 plain-tex-mode
;;                 nxml-mode
;;                 html-mode
;;                 web-mode))
;;   (add-to-list 'my-yank-indent-modes mode))

(setq-default proced-auto-update-flag t)
(setq-default proced-auto-update-interval 3)
(setq-default proced-post-display-hook (quote (fit-window-to-buffer)))

;;; grep 默认递归查找
;; (setq-default grep-command "grep --color -nH -r -E ")
(setq-default grep-highlight-matches t)
(setq-default grep-scroll-output t)

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

;; whitespace 设置
(require 'whitespace-cleanup-mode)  
(global-whitespace-mode +1)
(global-whitespace-cleanup-mode +1)
(setq-default whitespace-line-column fill-column)
(setq-default whitespace-style
              '(face trailing lines-tail empty
                     space-before-tab::space newline
                     indentation::space space-after-tab::space))
(setq-default show-trailing-whitespace t)
;; (setq-default my-whitespace t)
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

;;(setq-default my-clean-whitespace-on-save t)
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


;; flycheck
(my-require-package 'flycheck-pos-tip)
(when (display-graphic-p)
  (after-load 'flycheck
    (require 'flycheck-pos-tip)
    (flycheck-pos-tip-mode 1)
    ))

;; 
(require 'easymenu)
(require 'imenu-anywhere)


(setq require-final-newline t)
(delete-selection-mode t)

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

;; calendar
(setq-default calendar-date-style (quote iso))
(setq-default calendar-chinese-all-holidays-flag t)

;; misc
(setq indent-tabs-mode nil)
(setq set-mark-command-repeat-pop t)
(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)
(require 'ediff)
;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

;; 响铃
;; (defun my/flash-mode-line ()
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.05 nil 'invert-face 'mode-line))
;; (setq-default
;;  ring-bell-function 'my/flash-mode-line)

;; 启用cua
(cua-selection-mode t)
(setq-default cua-auto-mark-last-change t)

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
;;(my-install-search-engine "baidu" "https://www.baidu.com/s?ie=UTF-8&w=" "Baidu: ")

;; M-. to find lisp function
(define-key messages-buffer-mode-map (kbd "M-.")
  'elisp-slime-nav-find-elisp-thing-at-point)
(define-key help-mode-map (kbd "M-.")
  'elisp-slime-nav-find-elisp-thing-at-point)
(define-key completion-list-mode-map (kbd "M-.")
  'elisp-slime-nav-find-elisp-thing-at-point)
(after-load 'debug
  (define-key debugger-mode-map (kbd "M-.")
    'elisp-slime-nav-find-elisp-thing-at-point))

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


;; large file
(defvar my-large-file-size large-file-warning-threshold
  "Maximum size of file above which a confirmation is requested.
When nil, never request confirmation.")

(defcustom my-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode)
  "Major modes which `spacemacs/check-large-file' will not be
automatically applied to."
  :group 'spacemacs
  :type '(list symbol))
;; check when opening large files - literal file open
(defun my-check-large-file ()
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
(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode my-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-region (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun my/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun my/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))



;; align
;; modified function from http://emacswiki.org/emacs/AlignCommands
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

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun my/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
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
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun my/unix2dos ()
  "Converts the current buffer to DOS file format."
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

(defun my/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1)
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (setq p1 (point))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1))
  )


;; undo-tree
(require 'undo-tree)
(setq-default undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)

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

;;
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

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
;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)



(defvar load-user-customized-major-mode-hook t)
(defvar cached-normal-file-full-path nil)
(defun my/is-buffer-file-temp ()
  (interactive)
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
  (let ((f (buffer-file-name))
        org
        (rlt t))
    (cond
     ((not load-user-customized-major-mode-hook) t)
     ((not f)
      ;; file does not exist at all
      (setq rlt t))
     ((string= f cached-normal-file-full-path)
      (setq rlt nil))
     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))
     ((and (string-match "\.html$" f)
           (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
      ;; file is a html file exported from org-mode
      (setq rlt t))
     (t
      (setq cached-normal-file-full-path f)
      (setq rlt nil)))
    rlt))


;; etags
(setq-default tags-revert-without-query t)
(setq-default tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive

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

(provide 'my-edit)
;;; my-edit.el ends here
