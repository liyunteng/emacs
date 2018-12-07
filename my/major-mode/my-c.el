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
;; (c-add-style "ffmpeg"
;; 	     '("k&r"
;; 	       (c-basic-offset . 4)
;; 	       (indent-tabs-mode . nil)
;; 	       (show-trailing-whitespace . t)
;; 	       (c-offsets-alist
;; 		(statement-cont . (c-lineup-assignments +)))))

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
  :config
  (defadvice disaster (after make-disaster-view-mode activate)
    (when (get-buffer disaster-buffer-assembly)
      (with-current-buffer disaster-buffer-assembly
	    (view-buffer-other-window disaster-buffer-assembly nil 'kill-buffer)))))

(use-package semantic
  :defer t
  :commands (semantic-mode)
  :init
  (defconst my-project-roots '("~/git/ihi/client/c9service"
                               "/usr/src/linux")
    "My project roots to setq semanticdb-project-roots.")
  ;;global-semantic-decoration-mode
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

  ;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
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
    (setq semantic-idle-scheduler-max-buffer-size 0)
    (setq semantic-idle-scheduler-work-idle-time 60)
    (setq semantic-idle-work-update-headers-flag t)
    (setq semantic-idle-work-parse-neighboring-files-flag t)

    ;; (add-hook 'semantic-init-hooks 'semantic-idle-completions-mode)
    )

  (require 'semantic/dep)
  (semantic-add-system-include "/usr/local/include")
  ;; (require 'semantic/decorate/include)
  (require 'semantic/bovine/gcc)
  (after-load 'cc-mode
    (semantic-gcc-setup)
    ;; (defcustom-mode-local-semantic-dependency-system-include-path
    ;;   c-mode my-c-system-include (semantic-gcc-get-include-paths "c"))
    ;; (defcustom-mode-local-semantic-dependency-system-include-path
    ;;   c++-mode my-c++-system-include (semantic-gcc-get-include-paths "c++"))

    (setq-mode-local c-mode semantic-dependency-include-path my-include-path)
    (setq-mode-local c++-mode semantic-dependency-include-path my-include-path)
    )
  (require 'semantic/bovine/c)
  ;; (dolist (x (list "/usr/lib/gcc/x86_64-pc-linux-gnu/8.2.1/include/stddef.h"))
  ;;   (add-to-list 'semantic-lex-c-preprocessor-symbol-file x))
  ;; (after-load 'c++-mode
  ;;   (semantic-c-add-preprocessor-symbol "__cplusplus" "201103L")
  ;;   (semantic-c-reset-preprocessor-symbol-map))
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
    	  ))))


  (use-package semantic/db-file
    :config
    (setq semanticdb-default-save-directory (expand-file-name "semanticdb" my-cache-dir)))
  ;; ;;;semantic Database
  (use-package semantic/db
    :config
    (setq semanticdb-search-system-databases t)
    (setq semanticdb-project-roots my-project-roots)
    )

  (use-package semantic/db-find
    :config
    (setq semanticdb-find-default-throttle
	      '(local project unloaded system recursive)))

  (use-package semantic/db-global
    :config
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    )

  (define-key semantic-mode-map (kbd "C-c , R") 'semantic-symref-regexp)
  (define-key semantic-mode-map (kbd "C-c , h") 'semantic-decoration-include-visit)
  (define-key semantic-mode-map (kbd "C-c , M-d") 'semantic-decoration-include-describe)
  (define-key semantic-mode-map (kbd "C-c , M-D") 'semantic-decoration-all-include-summary)
  (define-key semantic-mode-map (kbd "C-c , M-i") 'semantic-decoration-unparsed-include-parse-include)
  (define-key semantic-mode-map (kbd "C-c , M-I") 'semantic-decoration-unparsed-include-parse-all-includes)
  (define-key semantic-mode-map (kbd "C-c , M-t") 'semantic-toggle-decoration-style)

  (define-key semantic-mode-map (kbd "C-c , a") 'semantic-add-system-include)
  (define-key semantic-mode-map (kbd "C-c , s") 'semantic-c-add-preprocessor-symbol)

  ;; debug
  (define-key semantic-mode-map (kbd "C-c , d e") 'semantic-c-describe-environment)
  (define-key semantic-mode-map (kbd "C-c , d b") 'semantic-describe-buffer)
  (define-key semantic-mode-map (kbd "C-c , d t") 'semantic-describe-tag)
  (define-key semantic-mode-map (kbd "C-c , d w") 'semantic-dump-parser-warnings)
  (define-key semantic-mode-map (kbd "C-c , d s") 'semantic-lex-spp-describe)
  )


(use-package cc-mode
  :config
  (defun my-cc-mode-hook ()
    "My c common mode hooks."
    (unless semantic-mode
      (semantic-mode +1))

    ;; (hide-ifdef-mode +1)
    (auto-fill-mode +1)
    (cscope-minor-mode +1)
    (c-subword-mode +1)

    ;; (setq hide-ifdef-hiding t)
    ;; (setq hide-ifdef-shadow t)

    ;; (setq comment-start '"/* ")
    ;; (setq comment-end '" */")
    ;; (c-set-style "qt-gnu")
    (setq-local indent-tabs-mode nil)
    (c-set-style "cc-mode")

    ;; set flycheck include paths
    ;; (when (boundp 'flycheck-mode)
    ;;   (after-load 'semantic/dep
    ;;     (mapc (lambda (arg) (dolist (incs (append semantic-dependency-system-include-path my-include-path))
	;; 		             (add-to-list arg incs)))
	;;           '(flycheck-gcc-include-path
    ;;             flycheck-clang-include-path
	;; 			flycheck-cppcheck-include-path))))
    ;; (setq-default company-clang-arguments '("-std=c++11"))
    )

  (defun my--cc-tab ()
    (interactive)
    (if (looking-at "#ifn?def")
        (if (or (get-char-property (line-end-position) 'invisible)
                (equal (get-char-property (line-end-position)  'face) 'hide-ifdef-shadow))
            (show-ifdef-block)
          (hide-ifdef-block))
      (if (boundp 'company-mode)
          (call-interactively 'company-indent-or-complete-common)
        (call-interactively 'indent-for-tab-command))))

  (define-key c-mode-base-map (kbd "C-c g") 'semantic-analyze-proto-impl-toggle)
  (define-key c-mode-base-map (kbd "C-c D") 'disaster)

  (define-key c-mode-base-map (kbd "C-d") 'c-hungry-delete-forward)
  (define-key c-mode-base-map (kbd "C-c C-d") 'semantic-ia-show-doc)     ;c-hungry-delete-forward
  (define-key c-mode-base-map (kbd "C-c C-l") 'semantic-ia-show-summary) ;c-toggle-elecric-state

  (define-key c-mode-base-map (kbd "C-c C-a") 'ff-find-related-file)     ;c-toggle-auto-newline

  (define-key c-mode-base-map (kbd "C-c C-m") 'my/smart-compile)
  (define-key c-mode-base-map (kbd "C-c C-s") nil)                       ;c-show-syntactic-information
  (define-key c-mode-base-map (kbd "C-c C-c") nil)                       ;comment-region
  (define-key c-mode-base-map (kbd "C-c C-b") nil)                       ;c-submit-bug-report
  (define-key c-mode-base-map (kbd "C-c C-w") nil)                       ;c-subword-mode
  (define-key c-mode-base-map (kbd "C-c C-k") 'kill-region)              ;c-toggle-comment-style
  (define-key c-mode-base-map (kbd "TAB") 'my--cc-tab)

  (add-hook 'c-mode-common-hook 'my-cc-mode-hook)
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



(provide 'my-c)
;;; my-c.el ends here
