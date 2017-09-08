;;; my-semantic.el --- cedet configuration

;; Copyright (C) 2014  liyunteng

;; Author: liyunteng(require 'cedet) <li_yunteng@163.com>
;; Keywords: c

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


;;; (global-ede-mode nil)


;; (require-package 'ecb)
;; ;; (require 'ecb)
;; (eval-after-load "ecb"
;;   (progn
;;     (setq ecb-tip-of-the-day nil)
;;     (add-hook 'ecb-activate-hook
;;               '(lambda ()
;;                  ))
;;     (add-hook 'ecb-deactivate-hook
;;               '(lambda ()
;; ))
;;     )
;;   )

(defconst my-project-roots '("~/git/ihi/client/c9service"
                             "/usr/src/linux")
  "My project roots to setq semanticdb-project-roots.")

(use-package speedbar
  :commands (speedbar)
  :bind (("<f2>" . speedbar))
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-tag-hierarchy-method
		'(speedbar-prefix-group-tag-hierarchy))

  (speedbar-add-supported-extension ".go")
  (add-hook 'speedbar-mode-hook
			(lambda ()
			  (auto-raise-mode t)
			  (setq dframe-update-speed 1)
			  ;; (add-to-list 'speedbar-frame-parameters '(top . 0))
			  ;; (add-to-list 'speedbar-frame-parameters '(left . 0))
			  )))

(use-package semantic
  :defer t
  :commands (semantic-mode)
  :init
  (defun my/semantic-find-definition (arg)
	"Jump to the definition of the symbol, type or function at point.
  With prefix arg, find in other window."
	(interactive "P")
	(let* ((tag (or (semantic-idle-summary-current-symbol-info-context)
					(semantic-idle-summary-current-symbol-info-brutish)
					(error "No known tag at point")))
		   (pos (or (semantic-tag-start tag)
					(error "Tag definition not found")))
		   (file (semantic-tag-file-name tag)))

	  (when (fboundp 'xref-push-marker-stack)
		(xref-push-marker-stack (push-mark (point))))
	  (if file
		  (if arg (find-file-other-window file) (find-file file))
		(if arg (switch-to-buffer-other-window (current-buffer))))


	  ;; (push-mark)
	  (goto-char pos)
	  (recenter-top-bottom)
	  ;; (end-of-line)
	  ))

  ;;global-semantic-decoration-mode
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
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
	(setq semantic-idle-scheduler-work-idle-time 60)
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

  ;; (use-package semantic/ia
  ;; 	:init
  ;; 	;;修复向回跳转的问题
  ;; 	(defadvice semantic-ia-fast-jump (around my-semantic-ia-fast-jump-push-mark activate)
  ;; 	  ;; (semantic-mrub-push semantic-mru-bookmark-ring
  ;; 	  ;; 					  (point)
  ;; 	  ;; 					  'mark)
  ;; 	  (when (fboundp 'xref-push-marker-stack)
  ;; 		(xref-push-marker-stack (push-mark (point))))
  ;; 	  ad-do-it
  ;; 	  )
  ;; 	)

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
	)




  ;; (require 'ede/cpp-root)

  ;; (require 'ede/cpp-root)
  ;; (require 'ede/autoconf-edit)
  ;; (require 'ede/custom)
  ;; (require 'ede/files)
  ;; (require 'ede/generic)
  ;; (require 'ede/linux)
  ;; (require 'ede/locate)
  ;; (require 'ede/make)
  ;; (require 'ede/makefile-edit)
  ;; (require 'ede/pconf)
  ;; (require 'ede/pmake)

  ;; (global-ede-mode t)
  ;; (ede-cpp-root-project "c9service"
  ;;                       :file "~/git/ihi/client/c9service/Makefile"
  ;;                       :name "c9service"
  ;;                       :include-path '("/"
  ;;                                       "/ihi_streamer"
  ;;                                       "/libs/include"
  ;;                                       "/jsoncpp/include"
  ;;                                       "/c9av/include"
  ;;                                       "/c9av/ti_include"
  ;;                                       "/c9av/ti_include/c9sdk"
  ;;                                       "/c9av/ti_include/mcfw"
  ;;                                       "/c9av/ti_include/mcfw/interfaces"))


  ;; (setq semanticdb-project-roots my-project-roots)
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file
  ;; "/opt/hisi-linux/x86-arm/arm-hisiv200-linux/arm-hisiv200-linux-gnueabi/include/c++/4.4.1/arm-hisiv200-linux-gnueabi/armv7a_vfp_v3d16/bits")


  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("__cplusplus" . "201103L"))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file ')

  ;;  优先调用senator的分析结果
  ;; (autoload 'senator-try-expand-semantic "senator")

  )

(provide 'my-semantic)
;;; my-cedet.el ends here
