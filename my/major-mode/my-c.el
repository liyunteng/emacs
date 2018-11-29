;;; my-c.el --- c-mode                               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  liyunteng

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

(use-package xcscope
  :ensure t
  :commands (cscope-minor-mode)
  :config
  (defvar my-cscope-origin-buffer nil)
  (defadvice cscope-call (before my-save-cscope-origin-buffer activate)
    (setq my-cscope-origin-buffer (current-buffer)))
  (defun my/cscope-quit ()
    "My cscope quit."
    (interactive)
    (cscope-quit)
    (switch-to-buffer my-cscope-origin-buffer)
    (delete-other-windows))

  (setq cscope-truncate-lines nil)
  (setq cscope-use-relative-paths nil)
  (setq cscope-index-recursively t)
  (setq cscope-name-line-width -45)
  (setq cscope-display-cscope-buffer t)
  (setq cscope-close-window-after-select nil)
  (cscope-setup)
  :bind
  (:map cscope-minor-mode-keymap
	("C-c s G" . cscope-find-global-definition)
	:map cscope-list-entry-keymap
	("C-p" . cscope-history-backward-file)
	("C-n" . cscope-history-forward-file)
	("C-M-p" . cscope-history-backward-result)
	("C-M-n" . cscope-history-forward-result)
	("C-k" . cscope-history-kill-file)
	("C-M-k" . cscope-history-kill-result)
	("d" . cscope-find-global-definition-no-prompting)
	("G" . cscope-find-global-definition)
	("q" . my/cscope-quit)
	))

;; (use-package helm-cscope
;; :if (fboundp 'helm-mode)
;; :config
;;   (define-key helm-cscope-mode-map (kbd "C-c s s")
;;     'helm-cscope-find-this-symbol)
;;   (define-key helm-cscope-mode-map (kbd "C-c s =")
;;     'helm-cscope-find-assignments-to-this-symbol)
;;   (define-key helm-cscope-mode-map (kbd "C-c s d")
;;     'helm-cscope-find-global-definition)
;;   (define-key helm-cscope-mode-map (kbd "C-c s c")
;;     'helm-cscope-find-calling-this-function)
;;   (define-key helm-cscope-mode-map (kbd "C-c s C")
;;     'helm-cscope-find-called-function)
;;   (define-key helm-cscope-mode-map (kbd "C-c s r")
;;     'helm-cscope-find-called-function)
;;   (define-key helm-cscope-mode-map (kbd "C-c s t")
;;     'helm-cscope-find-this-text-string)
;;   (define-key helm-cscope-mode-map (kbd "C-c s e")
;;     'helm-cscope-find-egrep-pattern)
;;   (define-key helm-cscope-mode-map (kbd "C-c s f")
;;     'helm-cscope-find-this-file)
;;   (define-key helm-cscope-mode-map (kbd "C-c s i")
;;     'helm-cscope-find-files-including-file)
;;   (define-key helm-cscope-mode-map (kbd "C-c s u")
;;     'helm-cscope-pop-mark))


(defconst my-kernel-include-path
  (list
   "/usr/src/linux/include"
   "/usr/src/linux/arch/x86/include")
  "My kernel include path.")

(defconst my-src-path (list
		       "./"
                       "src/"
                       "../src"
                       "../../src")
  "My local src path.")

;;;###autoload
(defconst my-include-path (list
                           "include/"
                           "inc/"
                           "export/"
			   "./"
                           "../"
                           "../include"
                           "../inc"
                           "../export"
                           "../.."
                           "../../include"
                           "../../inc"
                           "../../export"
                           "../../.."
                           "../../../include"
                           "../../../inc"
                           "../../../export")
  "My local include path.")

;;自动给头文件添加ifndef
;; (defun get-include-guard ()
;;   "Return a string suitable for use in a C/C++ include guard"
;;   (let* ((fname (buffer-file-name (current-buffer)))
;;          (fbasename (replace-regexp-in-string ".*/" "" fname))
;;          (inc-guard-base (replace-regexp-in-string "[.-]"
;;                                                    "__"
;;                                                    fbasename)))
;;     (concat (upcase inc-guard-base) "__")))

;; (add-hook 'find-file-not-found-hooks
;;           '(lambda ()
;;              (let ((file-name (buffer-file-name (current-buffer))))
;;                (when (string= ".h" (substring file-name -2))
;;                  (let ((include-guard (get-include-guard)))
;;                    (insert "#ifndef " include-guard)
;;                    (newline)
;;                    (insert "#define " include-guard)
;;                    (newline 4)
;;                    (insert "#endif // " include-guard)
;;                    (newline)
;;                    (previous-line 3)
;;                    (set-buffer-modified-p nil))))))


;; to replace /usr/share/emacs/24.5/lisp/progmodes/hideif.el.gz
;; (defun hif-mathify (val)
;;   "Treat VAL as a number: if it's t or nil, use 1 or 0."
;;   (cond ((eq val t) 1)
;;         ((null val) 0)
;;         ((and (stringp val)
;;               (string-match "0[xX]\\([0-9a-fA-F]+\\.?[0-9a-fA-F]*\\)"
;;                             val))
;;          (string-to-number (match-string 1 val) 16))
;;         ((and (stringp val)
;;               (string-match "\\([0-9]+\\.?[0-9]*\\)"
;;                             val))
;;          (string-to-number (match-string 1 val) 10))
;;         (t val)
;;         ))

;; (defun hif-divide (a b)
;;   (if (not (equal 0 (hif-mathify b)))
;;       (/ (hif-mathify a)
;;          (Hif-mathify b))
;;     0))

;;Add kernel style
;; (defun linux-c-mode ()
;;   "C mode with adjusted defaults for use with the Linux kernel."
;;   (interactive)
;;   (c-mode)
;;   (c-set-style "K&R")
;;   (setq tab-width 8)
;;   (setq indent-tabs-mode nil)
;;   (setq c-basic-offset 8))
;; (setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
;;                             auto-mode-alist))

(use-package cmacexp
  :defines (c-macro-shrink-window-flag
	    c-macro-promp-flag)
  :defer t
  :config
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-prompt-flag t)
  )

(use-package find-file
  :defines (cc-search-directories
	    cc-other-file-alist)
  :config
  (dolist (var my-include-path)
    (add-to-list 'cc-search-directories var))
  (dolist (var my-src-path)
    (add-to-list 'cc-search-directories var))
  (dolist (var '(("\\c\\'" (".h" ".hpp" ".hxx"))
                 ("\\.h\\'" (".c" ".cpp" ".cxx"))
                 ("\\.hpp\\'" (".cpp" ".cxx" ".c"))
                 ("\\.hxx\\'" (".cpp" ".cxx" ".c"))))
    (add-to-list 'cc-other-file-alist var)))

(use-package hideif
  :diminish hide-ifdef-mode ;;hide-ifdef-hiding
  :commands (hide-ifdef-mode
	     hide-ifdefs)
  :defines (hide-ifdef-mode)
  :defer t
  :init
  (my|add-toggle hide-ifdef-mode
    :status hide-ifdef-mode
    :on (progn (hide-ifdef-mode +1)
	       (hide-ifdefs t))
    :off (hide-ifdef-mode -1)
    :documentation "Hide/Show ifdef"
    )
  ;; linux header may be cause failed
  ;; (add-hook 'c-mode-common-hook 'my/toggle-hide-ifdef-mode-on)
  :config
  ;; fix can't use = with string
  ;; (defun hif-mathify (val)
  ;;   "Treat VAL as a number: if it's t or nil, use 1 or 0."
  ;;   (cond ((stringp val) (string-to-number val))
  ;; 	  ((eq val t) 1)
  ;; 	  ((null val) 0)
  ;; 	  (t val)))
  (setq hide-ifdef-shadow t
	hide-ifdef-initially nil)
  )

(use-package disaster
  :ensure t
  :defer t
  :init
  (defadvice disaster (after make-disaster-view-mode activate)
    (when (get-buffer disaster-buffer-assembly)
      (with-current-buffer disaster-buffer-assembly
	(view-buffer-other-window disaster-buffer-assembly nil 'kill-buffer))))
  )
;; (c-add-style "ffmpeg"
;; 	     '("k&r"
;; 	       (c-basic-offset . 4)
;; 	       (indent-tabs-mode . nil)
;; 	       (show-trailing-whitespace . t)
;; 	       (c-offsets-alist
;; 		(statement-cont . (c-lineup-assignments +)))))

(use-package semantic
  :defer t
  :commands (semantic-mode)
  :init
  (setq semanticdb-default-save-directory (expand-file-name "semanticdb" my-cache-dir))
  (defconst my-project-roots '("~/git/ihi/client/c9service"
                               "/usr/src/linux")
    "My project roots to setq semanticdb-project-roots.")
  ;;global-semantic-decoration-mode
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
  :config
  ;; for debug
  ;; (setq semantic-dump-parse t)
  ;; (setq semantic-edits-verbose-flag t)
  ;; (setq semantic-idle-scheduler-verbose-flag t)
  ;; (setq semantic-lex-debug-analyzers t)
  ;; (setq semantic-update-mode-line t)
  (use-package semantic/idle
    :defines (semantic-idle-scheduler-idle-time
	      semantic-idle-scheduler-max-buffer-size
	      semantic-idle-scheduler-work-idle-time
	      semantic-idle-work-update-headers-flag
	      )
    :init
    (setq semantic-idle-scheduler-idle-time 1)
    (setq semantic-idle-scheduler-max-buffer-size 10240000)
    (setq semantic-idle-scheduler-work-idle-time 30)
    (setq semantic-idle-work-update-headers-flag t)
    (setq semantic-idle-work-parse-neighboring-files-flag t)

    ;; (add-hook 'semantic-init-hooks 'semantic-idle-completions-mode)
    )

  (require 'semantic/dep)
  (semantic-add-system-include "/usr/local/include")
  ;; (require 'semantic/decorate/include)
  (require 'semantic/bovine/c)
  ;;   ;; (require 'semantic/bovine/make)
  ;;   ;; (require 'semantic/bovine/c-by)
  (require 'semantic/wisent)

  (after-load 'cc-mode
    (defcustom-mode-local-semantic-dependency-system-include-path
      c-mode my-c-system-include (semantic-gcc-get-include-paths "c"))
    (defcustom-mode-local-semantic-dependency-system-include-path
      c++-mode my-c++-system-include (semantic-gcc-get-include-paths "c++"))

    (setq-mode-local c-mode semantic-dependency-include-path my-include-path)
    (setq-mode-local c++-mode semantic-dependency-include-path my-include-path)
    )

  (use-package semantic/ia
    :init
    (defun my/semantic-find-definition (arg)
      (interactive "P")
      (when (fboundp 'xref-push-marker-stack)
	(xref-push-marker-stack (push-mark (point))))
      (semantic-ia-fast-jump (point))
      (recenter-top-bottom)
      )

    ;; fix cursor not on word
    (defadvice semantic-ia-show-doc (around fix-not-on-word  (apoint) activate)
      (catch 'a
	(if (semantic-analyze-current-context apoint)
    	    ad-do-it
    	  (message "Cursor not on symbol")
	  (throw 'a nil)
    	  )))
    )

  ;; (require 'semantic/lex-spp)
  ;; (setq semantic-lex-maximum-depth 200)
  ;; 设置头文件路径

  ;; (use-package semantic/senator
  ;; 	:init
  ;; 	(setq senator-highlight-found t))

  ;; ;;;semantic Database
  (use-package semantic/db
    :init
    (setq semanticdb-search-system-databases t)
    (setq semanticdb-project-roots my-project-roots)
    )

  (use-package semantic/db-find
    :init
    (setq semanticdb-find-default-throttle
	  '(local project unloaded system recursive)
	  )
    )

  (use-package semantic/db-global
    :init
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    ))

(defun my-c-mode-hooks ()
  "My c common mode hooks."
  (unless semantic-mode
    (semantic-mode +1))

  ;; (hide-ifdef-mode +1)
  (auto-fill-mode +1)
  (cscope-minor-mode t)

  ;; (setq hide-ifdef-hiding t)
  ;; (setq hide-ifdef-shadow t)
  ;; (helm-cscope-mode t)

  ;;更改注释的样式
  ;; (setq comment-start '"/* ")
  ;; (setq comment-end '" */")
  ;; (c-set-style "qt-gnu")
  ;; 空格代替tab
  (setq-local indent-tabs-mode nil)
  (c-set-style "cc-mode")
  (setq-local tab-width 8)

  ;; 设置头文件路径
  (use-package flycheck
    :config
    (use-package semantic/dep)
    (mapc (lambda (arg) (dolist (incs (append semantic-dependency-system-include-path my-include-path))
			  (add-to-list arg incs)))
	  '(flycheck-gcc-include-path flycheck-clang-include-path
				      flycheck-cppcheck-include-path))
    ;; (if  (executable-find "clang")
    ;; 	(progn (setq-local flycheck-checker 'c/c++-clang)
    ;; 		   (dolist (item (append semantic-dependency-system-include-path my-include-path))
    ;; 			 (add-to-list 'flycheck-clang-include-path item)))
    ;;   (progn (setq-local flycheck-checker 'c/c++-gcc)
    ;; 		 (dolist (item (append semantic-dependency-system-include-path my-include-path))
    ;; 		   (add-to-list 'flycheck-gcc-include-path item))))
    ;; (setq-default flycheck-clang-args '("-std=c++11"))
    )

  ;; (setq-default company-clang-arguments '("-std=c++11"))
  )

(defun my-c-mode-keys ()
  "My c mode local key."
  ;; 快速跳转
  ;; (local-set-key (kbd "C-c j") 'my/jump-to-definition)
  ;; (local-set-key (kbd "C-c C-j") 'my/jump-to-definition-other-window)

  ;; 快速跳转定义与实现
  ;; (local-set-key (kbd "C-c g") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c C-g") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c b") 'pop-tag-mark)
  (local-set-key (kbd "C-c C-b") 'my/jump-back-to-origin)

  ;; hide-ifdef
  (local-set-key (kbd "C-c C-c p") 'previous-ifdef)
  (local-set-key (kbd "C-c C-c n") 'next-ifdef)
  (local-set-key (kbd "C-c C-c f") 'forward-ifdef)
  (local-set-key (kbd "C-c C-c b") 'backward-ifdef)
  (local-set-key (kbd "C-c C-c =") 'show-ifdef-block)
  (local-set-key (kbd "C-c C-c -") 'hide-ifdef-block)
  (local-set-key (kbd "C-c C-c h") 'hide-ifdefs)
  (local-set-key (kbd "C-c C-c s") 'show-ifdefs)
  (local-set-key (kbd "C-c C-c m") 'hide-ifdef-toggle-shadowing)
  ;; 添加define
  (local-set-key (kbd "C-c C-c D") 'hide-ifdef-define)
  ;; 去掉define
  (local-set-key (kbd "C-c C-c U") 'hide-ifdef-undef)


  ;; 搜索函数被调用
  (local-set-key (kbd "C-c C-c r") 'semantic-symref-symbol)
  (local-set-key (kbd "C-c C-c R") 'semantic-symref-regexp)

  ;; disaster
  (local-set-key (kbd "C-c C-c d") 'disaster)

  ;; 现实文档
  (local-set-key (kbd "C-c C-d") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c C-l") 'semantic-ia-show-summary)

  ;; 分析头文件
  (local-set-key (kbd "C-c M-i") 'semantic-decoration-unparsed-include-parse-include)
  ;; 分析全部头文件
  (local-set-key (kbd "C-c M-I") 'semantic-decoration-unparsed-include-parse-all-includes)
  (local-set-key (kbd "C-c `") 'semantic-show-unmatched-syntax-next)

  ;; 标记函数 C-M-h
  (local-set-key (kbd "C-c C-c H") 'senator-mark-defun)

  ;; 改变显示样式
  (local-set-key (kbd "C-c C-c t") 'semantic-toggle-decoration-style)
  ;; 添加头文件
  (local-set-key (kbd "C-c C-c A") 'semantic-add-system-include)
                                        ;
  ;; debug
  (local-set-key (kbd "C-c C-c P") 'semantic-c-describe-environment)
  (local-set-key (kbd "C-c C-c B") 'semantic-describe-buffer)
  (local-set-key (kbd "C-c C-c T") 'semantic-describe-tag)
  (local-set-key (kbd "C-c C-c S") 'semantic-c-add-preprocessor-symbol)

  ;;头文件切换
  ;; (local-set-key (kbd "C-c C-c a") 'ff-get-other-file)
  (local-set-key (kbd "C-c C-a") 'ff-find-related-file)
  ;; (local-set-key (kbd "C-c C-h") 'ff-find-related-file)
  ;; (local-set-key (kbd "C-c C-a") 'auto-fill-mode)

  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c/" 'semantic-ia-complete-tip)
  ;; (define-key semantic-mode-map (kbd "C-c h") 'semantic-decoration-include-visit)
  ;; (define-key semantic-mode-map "." 'semantic-complete-self-insert)
  ;; (define-key semantic-mode-map "->" 'semantic-complete-self-insert)

  (local-set-key (kbd "C-c \\") 'c-backslash-region)
  (local-set-key (kbd "C-c C-\\") 'c-backslash-region)

  ;; cpp预处理的结果
  (local-set-key (kbd "C-c e") 'c-macro-expand)
  (local-set-key (kbd "C-c C-e") 'c-macro-expand)

  ;; (local-set-key (kbd "C-c g") 'cscope-find-this-symbol)
  ;; (local-set-key (kbd "C-c C-g") 'cscope-find-global-definition)

  ;;补全
  (local-set-key (kbd "M-/") 'hippie-expand)

  ;;去掉ctl-c ctl-c的注释功能
  ;; (local-set-key (kbd "C-c C-c") nil)

  (local-set-key (kbd "M-j") 'join-line)
  ;; (local-set-key (kbd "M-.") 'my/tags-search)
  ;; (local-set-key (kbd "M-,") 'tags-loop-continue)
  ;; C-M-j c-indent-new-comment-line


  (local-set-key [(f9)] 'my/smart-compile)
  (local-set-key (kbd "C-c C-m") 'my/smart-compile)

  (local-set-key (kbd "C-c C-k") 'kill-region) ;replace c-toggle-comment-style
  )


;; 添加Kernel的Include
;; (let ((include-dirs my-kernel-include-path))
;;   (mapc (lambda (dir)
;;           (semantic-add-system-include dir 'c-mode)
;;           (semantic-add-system-include dir 'c++-mode))
;;         include-dirs))

;; (let ((include-dirs my-include-path))
;;   (mapc (lambda (dir)
;;           (add-to-list 'cc-search-directories dir)
;;           (semantic-add-system-include dir 'c-mode)
;;           (semantic-add-system-include dir 'c++-mode))
;;         include-dirs))
;; (setq semantic-default-c-path
;; semantic-c-dependency-system-include-path)

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'my-c-mode-hooks)
  (add-hook hook 'my-c-mode-keys))

(provide 'my-c)
;;; my-c.el ends here