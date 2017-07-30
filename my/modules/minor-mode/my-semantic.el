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

(after-load 'semantic/mru-bookmark
  ;;修复向回跳转的问题
  (defadvice semantic-ia--fast-jump-helper (around my-semantic-ia-fast-jump-push-mark activate)
    (semantic-mrub-push semantic-mru-bookmark-ring
                        (point)
                        'mark)
    (when (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack (set-mark (point))))
    ad-do-it
    )
  ;; (defadvice push-mark (around semantic-mru-bookmark activate)
  ;;     "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
  ;; If `semantic-mru-bookmark-mode' is active, also push a tag onto
  ;; the mru bookmark stack."
  ;;     (semantic-mrub-push semantic-mru-bookmark-ring
  ;;                         (point)
  ;;                         'mark)
  ;;     (xref-push-marker-stack)
  ;;     ad-do-it)
  )

(after-load 'speedbar
  (require 'semantic/sb)
  (add-hook 'speedbar-mode-hook
            (lambda ()
              (auto-raise-mode t)
              (setq dframe-update-speed 1)
              ;; (add-to-list 'speedbar-frame-parameters '(top . 0))
              ;; (add-to-list 'speedbar-frame-parameters '(left . 0))
              ))

  (speedbar-add-supported-extension ".go")
  (setq speedbar-show-unknown-files t)
  (setq speedbar-tag-hierarchy-method
        '(speedbar-prefix-group-tag-hierarchy))
  )

(after-load 'semantic
  ;;global-semantic-decoration-mode
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)

  ;; for debug
  ;; (setq semantic-dump-parse t)
  ;; (setq semantic-edits-verbose-flag t)
  ;; (setq semantic-idle-scheduler-verbose-flag t)
  ;; (setq semantic-lex-debug-analyzers t)
  ;; (setq semantic-update-mode-line t)


  (setq global-semantic-mru-bookmark-mode t)

  (require 'semantic/idle)
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-idle-scheduler-max-buffer-size 10240000)
  (setq semantic-idle-scheduler-work-idle-time 60)
  (setq semantic-idle-work-update-headers-flag t)
  ;; (setq semantic-idle-work-parse-neighboring-files-flag t)

  ;;smart complitions
  (require 'semantic/ia)

  ;;   ;;Include settings
  (require 'semantic/analyze)
  ;;   ;; (require 'semantic/decorate/include)
  (require 'semantic/bovine)
  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  ;;   ;; (require 'semantic/bovine/make)
  ;;   ;; (require 'semantic/bovine/c-by)
  (require 'semantic/wisent)


  (require 'semantic/dep)
  ;; (require 'semantic/lex-spp)
  ;; (setq semantic-lex-maximum-depth 200)
  ;; 设置头文件路径
  (setq-mode-local c-mode semantic-dependency-system-include-path
                   (semantic-gcc-get-include-paths "c"))
  (setq-mode-local c++-mode semantic-dependency-system-include-path
                   (semantic-gcc-get-include-paths "c++"))


  (require 'semantic/senator)
  (setq senator-highlight-found t)
  ;;   ;; (add-hook 'semantic-init-hooks 'semantic-idle-completions-mode)

  ;; ;;;semantic Database
  (require 'semantic/db)
  (require 'semantic/db-global)
  (require 'semantic/db-file)

  ;;   ;; ;;;使用gnu global的TAGS
  (setq semanticdb-search-system-databases t)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (setq-mode-local c-mode semanticdb-find-default-throttle
                   '(project local unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
                   '(project local unloaded system recursive))

  (setq semanticdb-project-roots my-project-roots)
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


  ;; (require 'semantic/mru-bookmark)
  ;; (require 'semantic/sb)

  ;;  优先调用senator的分析结果
  ;; (autoload 'senator-try-expand-semantic "senator")

  )

(provide 'my-semantic)
;;; my-cedet.el ends here
