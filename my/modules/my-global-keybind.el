;;; my-global-keybind.el --- my global key binds     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  lyt

;; Author: lyt <lyt@lyt-arch>
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

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-x p") 'proced)

;; occur绑定为M-s o ;; (global-set-key (kbd "C-x M-o") 'occur)
;; (global-set-key (kbd "C-h o") 'occur)

;; org
(global-set-key (kbd "C-x c") 'org-capture)
(global-set-key (kbd "C-x l") 'org-store-link)
(global-set-key (kbd "C-x a") 'org-agenda)

;;使用sudo 编辑文件
(global-set-key (kbd "C-x M-f") 'find-file-root)

;; append to file
(defvar my-file-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'append-to-file)
    (define-key map "r" 'find-file-read-only)
    ))
(define-key ctl-x-map "f" my-file-prefix-map)
(global-set-key (kbd "C-x f") nil)
(global-set-key (kbd "C-x f a") 'append-to-file)
(global-set-key (kbd "C-x f i") 'insert-file)
(global-set-key (kbd "C-x f f") 'find-file)
(global-set-key (kbd "C-x f d") 'delete-file)
(global-set-key (kbd "C-x f v") 'find-alternate-file)

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
(global-set-key (kbd "C-SPC") 'set-mark-command)
;;跳到全局上次标记的地方(C-x C-.)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
;; (global-set-key (kbd "C-x C-x") 'cua-exchange-point-and-mark)
;; (global-set-key (kbd "C-x C-x") 'pop-to-mark-command)

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

;; align
(global-set-key (kbd "C-x \\") 'my/align-repeat)

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

;; 使用自己的注释方法
(global-set-key (kbd "M-;") 'my/comment-dwim-line)
;; 删除注释
(global-set-key (kbd "C-M-;") 'comment-kill)

;; ==================== WINDOW ====================
;; switch window
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'my/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") 'my/split-window-vertically-then-switch)
(global-set-key (kbd "C-x 3") 'my/split-window-horizontally-then-switch)
(global-set-key (kbd "C-x |") 'my/split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'my/split-window-vertically-instead)
(global-set-key (kbd "C-x <down>") 'my/toggle-current-window-dedication)
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;; 调整window大小
(global-set-key (kbd "C-M-=") 'balance-windows)
(global-set-key (kbd "C-M-+") 'enlarge-window)
(global-set-key (kbd "C-M-_") 'shrink-window)
(global-set-key (kbd "C-M-<") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M->") 'shrink-window-horizontally)
;; 调整window透明度
(global-set-key (kbd "M-C-8") (lambda () (interactive) (my/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (my/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))
;; 调整window文字大小
(global-set-key (kbd "C-x C-=") 'text-scale-adjust)
(global-set-key (kbd "C-x C--") 'text-scale-adjust)
(global-set-key (kbd "C-x C-0") 'text-scale-adjust)


(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
;; ==================== MACRO ====================
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

;; ==================== REGISTER ====================
;; 保存frame的布局到register
(global-set-key (kbd "C-x r f") 'frameset-to-register)
(global-set-key (kbd "C-x r w") 'list-registers)
;; 将当前的位置保存到regiseter
(global-set-key (kbd "C-x r .") 'point-to-register)
(global-set-key (kbd "C-x r j") 'jump-to-register)
;; (global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r v") 'helm-register)
(global-set-key (kbd "C-x r i") 'insert-register)
(global-set-key (kbd "C-x r a") 'append-to-register)
(global-set-key (kbd "C-x r p") 'prepend-to-register)
;; bookmark
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-h o") 'helm-occur)


;;multi-term
(global-set-key (kbd "C-x x") 'my/multi-term-dedicated-toggle-and-select)
(global-set-key (kbd "C-x t m") 'my/multi-term-dedicated-toggle-and-select)
(global-set-key (kbd "C-x t t") 'multi-term)
(global-set-key (kbd "C-x t n") 'multi-term-next)
(global-set-key (kbd "C-x t p") 'multi-term-prev)

;; ==================== MISC ====================
;;speedbar
(global-set-key (kbd "<f2>") 'speedbar-get-focus)
;; (global-set-key [(f4)] 'speedbar)

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

;;最大化
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<f11>") 'toggle-frame-maximized)
;;菜单栏
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; (global-set-key (kbd "C-M-/") 'completion-at-point)

;; (global-set-key (kbd "C-x [") 'switch-to-prev-buffer)
;; (global-set-key (kbd "C-x ]") 'switch-to-next-buffer)


(global-set-key (kbd "C-=") 'er/expand-region)


;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
;; A quick major mode help with discover-my-major
(define-key 'help-command (kbd "C-m") 'discover-my-major)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

(provide 'my-global-keybind)
;;; my-global-keybind.el ends `'here
