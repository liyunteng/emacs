;;; my-gnus.el --- gnus                              -*- lexical-binding: t; -*-

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
(require 'gnus)

;; (defvar my-gnus-home "~/Gnus")
;; (setq-default gnus-startup-file (expand-file-name "newsrc" my-gnus-home)
;; 			  gnus-default-directory  my-gnus-home
;; 			  gnus-home-directory  my-gnus-home
;; 			  gnus-dribble-directory my-gnus-home

;; 			  gnus-directory (expand-file-name "News/" my-gnus-home)
;; 			  gnus-article-save-directory  gnus-directory
;; 			  gnus-kill-files-directory (expand-file-name "trash" gnus-directory)
;; 			  gnus-agent-directory (expand-file-name "agent" gnus-directory)
;; 			  gnus-cache-directory (expand-file-name "cache" gnus-directory)
;; 			  gnus-cache-active-file (expand-file-name "active" gnus-cache-directory)


;; 			  message-directory (expand-file-name "Mail" my-gnus-home)
;; 			  message-auto-save-directory (expand-file-name "drafts" message-directory)
;; 			  mail-source-directory (expand-file-name "incoming" message-directory)

;; 			  nnml-newsgroups-file (expand-file-name "newsgroup" message-directory)
;; 			  nnmail-message-id-cache-file (expand-file-name "nnmail-cache" my-gnus-home)
;;               mml-default-directory (expand-file-name "Attachment" my-gnus-home)
;; 			  )

;;
;; 关闭默认的archive 这个方法不好控制
;;
(setq gnus-message-archive-group nil)

;; 设置存档目录
;; (setq gnus-outgoing-message-group
;; 	  '(nnml "archive"
;; 			 (nnml-directory   (expand-file-name "archive" gnus-directory))
;; 			 (nnml-active-file (expand-file-name "active" nnml-directory))
;; 			 (nnml-get-new-mail nil)
;; 			 (nnml-inhibit-expiry t)))

;;;; 一个老外的例子，可以参考
;;;;A function that selects a reasonable group for Gcc'ing this mail.

;;(defun MySendedMail ()
;;  (cond ((and gnus-newsgroup-name
;;              (not (message-news-p))
;;              (stringp gnus-newsgroup-name))
;;         gnus-newsgroup-name)
;;        (t ted-default-gcc-group)))
;;;; Use it.
;;(setq gnus-outgoing-message-group "nnml:SendMails")

;; (defun MySended ()
;;   (if (message-news-p)
;; 	  "nnml:SendNews"
;; 	"nnml:SendMails"))
;; (setq gnus-outgoing-message-group 'MySended)

;; 新闻组
;;cn99
;; (setq nnspool-spool-directory gnus-directory)
;; (setq nnspool-lib-dir gnus-directory)
;; (setq nnspool-active-file (expand-file-name "active" gnus-directory))
;; (setq nnspool-active-file  nil )

(setq gnus-select-method '(nndoc "gnus-help"))
;; (setq gnus-select-method '(nntp "localhost"))

;;告诉gnus如何存放接收来的邮件，gnus把这个叫做backend，最常用的方式
;;是nnfolder，另外还有nnmbox, nnml等其它几种方式，我们选择其中一种就可以了：
;;雅科\新帆
;; (add-to-list 'gnus-secondary-select-methods '(nntp "news.yaako.com"))
(add-to-list 'gnus-secondary-select-methods
			 '(nnimap "163"
					  (nnimap-address "imap.163.com")
					  (nnimap-server-port "imaps")
					  (nnimap-stream ssl)
					  (nnimap-user "li_yunteng")
					  ))
(add-to-list 'gnus-secondary-select-methods
			 '(nnimap "streamocean"
					  (nnimap-address "imap.qiye.163.com")
					  (nnimap-server-port "imaps")
					  (nnimap-stream ssl)
					  (nnimap-user "liyunteng@streamocean.com")))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.newsfan.net"))
;; (add-to-list 'gnus-secondary-select-methods '(nnml "mails"))

;;首先我们设置POP3服务器：
;; Gmail

;;告诉gnus如何存放接收来的邮件，gnus把这个叫做backend，
;;最常用的方式是nnfolder，另外还有nnmbox, nnml等其它几种方式，我们
;;选择其中一种就可以了：
;; 采用stunnel作为服务器处理ssl链接
(setq mail-sources
	  '(
		;; (pop :server "pop.163.com"
		;; 	 :user "li_yunteng"
		;; 	 :password "yun1988"
		;; 	 )

		;; (pop :server "pop.163.qiye.com"
		;; 	 :user "liyunteng@streamocean.com"
		;; 	 :password "Lyt532482644")
		))

;;然后我们设置SMTP服务器，采用smtp方式发送邮件需要一个小程序
;;smtpmail.el, 这个程序现在已被纳入了官方的Emacs，如果你用的是最新的
;;CVS Emacs，比如 Emacs22, Emacs23等，就已经包含了这个程序。你可以检
;;查一下emacs的安装目录中 lisp/mail/ 目录下有没有这个文件，如果没有
;;的话，就只好自己下载、安装了。现在我们看看如何设置：

;;发送邮件，参考 http://www.emacswiki.org/emacs/GnusGmail
;;;; 采用stunnel处理ssl连接
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
	  send-mail-function 'smtpmail-send-it
	  ;; smtpmail-smtp-server "smtp.163.com"
	  ;; smtpmail-stream-type 'ssl
	  ;; smtpmail-smtp-service 994		;ssl 994/465
	  ;; smtpmail-smtp-user "li_yunteng"

	  user-mail-address "liyunteng@streamocean.com"
	  smtpmail-smtp-server "smtp.qiye.163.com"
	  smtpmail-stream-type 'starttls
	  smtpmail-smtp-service 25		;ssl 994/465
	  smtpmail-smtp-user "liyunteng@streamocean.com"

	  ;; smtpmail-local-domain "localhost"
	  ;; smtpmail-sendto-domain "smtp.qiye.163.com"
	  ;; smtpmail-debug-info t
	  )

;;
;; 邮件分类,使用 nnmail-split-fancy方法更为灵活
;;
(setq nnmail-treat-duplicates 'delete ; 如果由重复，删除！
	  nnmail-crosspost nil ; 同一个邮件不要到多个组
	  nnmail-split-fancy-match-partial-words t ; 单词部分匹配也算成功匹配
	  nnmail-split-methods 'nnmail-split-fancy ; 这个分类方法比较灵活
	  nnmail-split-fancy
	  '(|	; 根据 mailing list 分类
	  	(any ".*@streamocean.com" "streamocean")
	  	(any ".*@163.com" "163")
	  	(any ".*@gmail.com" "gmail")
	  	(to "liyunteng@streamocean.com\\|li_yunteng@163.com" "To-me") ;;这个似乎没起作用？！
	  	"other")
	  )
;; 这里或许是 junk 了

;;
;;总是显示mail组，如何显示所有组呢？
;;
(setq gnus-permanently-visible-groups '"nn*")


;;--------------------------------------------------------------------------------------
;; 一些常规设置
;;
;;(gnus-agentize)                                   ;旧格式，已废弃！
(setq gnus-agent t)                                 ;开启代理功能, 以支持离线浏览
(setq gnus-inhibit-startup-message t)               ;关闭启动时的画面
(setq gnus-novice-user nil)                         ;关闭新手设置, 不进行确认
(setq gnus-expert-user t)                           ;不询问用户
(setq gnus-show-threads t)                          ;显示邮件线索
(setq gnus-interactive-exit nil)                    ;退出时不进行交互式询问
(setq gnus-use-dribble-file t)                      ;不创建恢复文件
(setq gnus-always-read-dribble-file t)              ;不读取恢复文件
(setq gnus-asynchronous t)                          ;异步操作
(setq gnus-large-newsgroup 100)                     ;设置大容量的新闻组默认显示的大小
(setq gnus-large-ephemeral-newsgroup nil)           ;和上面的变量一样, 只不过对于短暂的新闻组
(setq gnus-suppress-duplicates t)
(setq gnus-summary-ignore-duplicates t)             ;忽略具有相同ID的消息
(setq gnus-treat-fill-long-lines t)                 ;如果有很长的行, 不提示
(setq message-confirm-send t)                       ;防止误发邮件, 发邮件前需要确认
(setq message-kill-buffer-on-exit t)                ;设置发送邮件后删除buffer
(setq message-from-style 'angles)                   ;`From' 头的显示风格
(setq message-syntax-checks '((sender . disabled))) ;语法检查
(setq nnmail-expiry-wait 7)                         ;邮件自动删除的期限 (单位: 天)
(setq nnmairix-allowfast-default t)                 ;加快进入搜索结果的组
(setq gnus-summary-display-while-building 100)       ;

(setq gnus-summary-line-format "%4P %U%R%z%O %{%5k%} %{%14&user-date;%}   %{%-20,20n%} %{%ua%} %B %(%I%-60,60s%)\n")

;;
;; 显示设置
;;
;; (setq mm-text-html-renderer 'w3m)                     ;用W3M显示HTML格式的邮件
(after-load 'mm-decode
  (setq mm-text-html-renderer 'gnus-w3m)
  (setq mm-inline-large-images t)                       ;显示内置图片
  ;; Inline images?
  (setq mm-attachment-override-types '("image/.*"))
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  )
;; 概要显示设置
(setq gnus-summary-gather-subject-limit 'fuzzy) ;聚集题目用模糊算法


(defun gnus-user-format-function-a (header) ;用户的格式函数 `%ua'
  (let ((myself (concat "<" "my-mail" ">"))
		(references (mail-header-references header))
		(message-id (mail-header-id header)))
	(if (or (and (stringp references)
				 (string-match myself references))
			(and (stringp message-id)
				 (string-match myself message-id)))
		"X" "│")))

(setq gnus-user-date-format-alist             ;用户的格式列表 `user-date'
	  '(((gnus-seconds-today) . "TD %H:%M")   ;当天
		(604800 . "W%w %H:%M")                ;七天之内
		((gnus-seconds-month) . "%d %H:%M")   ;当月
		((gnus-seconds-year) . "%m-%d %H:%M") ;今年
		(t . "%y-%m-%d %H:%M")))

;;其他
;; 线程的可视化外观, `%B'
(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-indent "    ")
(setq gnus-sum-thread-tree-single-indent "◎ ")
(setq gnus-sum-thread-tree-root "● ")
(setq gnus-sum-thread-tree-false-root "☆")
(setq gnus-sum-thread-tree-vertical "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf "╰─► ")


;; 用 Supercite 显示多种多样的引文形式
;; (setq sc-attrib-selection-list nil
;; 	  sc-auto-fill-region-p nil
;; 	  sc-blank-lines-after-headers 1
;; 	  sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
;; 	  sc-cite-blank-lines-p nil
;; 	  sc-confirm-always-p nil
;; 	  sc-electric-references-p nil
;; 	  sc-fixup-whitespace-p t
;; 	  sc-nested-citation-p nil
;; 	  sc-preferred-header-style 4
;; 	  sc-use-only-preference-p nil)

;; 线程设置
(setq
 gnus-use-trees t                                                       ;联系老的标题
 gnus-tree-minimize-window nil                                          ;用最小窗口显示
 gnus-fetch-old-headers 'some                                           ;抓取老的标题以联系线程
 gnus-generate-tree-function 'gnus-generate-horizontal-tree             ;生成水平树
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject ;聚集函数根据标题聚集
 )

;;
;; 时间显示
;;
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local) ;将邮件的发出时间转换为本地时间
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)   ;跟踪组的时间轴

;; 新闻组分组
;; 排序
(setq gnus-thread-sort-functions
	  '((not gnus-thread-sort-by-date)     ;;时间的逆序
		(not gnus-thread-sort-by-number))) ;;跟踪的数量的逆序

;; 自动跳到第一个没有阅读的组
(add-hook 'gnus-switch-on-after-hook 'gnus-group-first-unread-group) ;gnus切换时
(add-hook 'gnus-summary-exit-hook 'gnus-group-first-unread-group) ;退出Summary时

;; 自动更新新消息，功能不错，但在我的机器上会很慢...
;; (add-hook 'gnus-summary-exit-hook 'gnus-notify+)        ;退出summary模式后
;; (add-hook 'gnus-group-catchup-group-hook 'gnus-notify+) ;当清理当前组后
;; (add-hook 'mail-notify-pre-hook 'gnus-notify+)          ;更新邮件时

;; 斑纹化
(setq gnus-summary-stripe-regexp        ;设置斑纹化匹配的正则表达式
	  (concat "^[^"
			  gnus-sum-thread-tree-vertical
			  "]*"))

;;------------------------------------------------------------------------------------
;; 其他的一些设置
;;

;;
;;不喜欢 Summary buffer 和 Article buffer 的版面，如何改变？或者三个
;;窗口的显示？
;;可以通过调用函数 gnus-add-configuration 来控制窗口的配置。语法有点
;;复杂，不过在手册 "Windows Layout" 中解释得很清楚，一些比较流行的例
;;子：
;;用 35% 的 Summary 比 65% 的 Article 替换原来的 25% 比 75%（其中的
;;1.0 意思是“占满剩余空间”）：
;;
;;(gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article 1.0))))
;;
;;三个窗口显示。左边是 Group buffer，右上是 Summary buffer，右下是
;;Article buffer：
;;

(gnus-add-configuration
 '(article
   (horizontal 1.0
			   (vertical 35
						 (group 1.0))
			   (vertical 1.0
						 (summary 0.35 point)
						 (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
			   (vertical 35
						 (group 1.0))
			   (vertical 1.0
						 (summary 1.0 point)))))

;;
;;不喜欢 Summary buffer 的样子，如何调整？
;;那么你需要和变量 gnus-summary-line-format 玩一玩，它得值是一个符号
;;串，比如作者，日期，主题等。手册 "Summary Buffer Lines" 中有可用的
;;符号列表和常被忘记的节点 "Formatting Variables" 和它的子节点。其中
;;有很多有用的东西，像指定光标和制表符的位置等。
;;
;;从 5.10.0 起，Gnus 新提供了一些很不错的标志符，例如，%B 可以形成一
;;个线索树，%&user-date 根据帖子给出时间细节。例子如下：
;;

;; (setq gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n")

;;Article Buffer设置
;;设定要显示的头消息格式
(setq gnus-visible-headers
	  "^\\(^To:\\|^CC:\\|^From:\\|^Subject:\\|^Date:\\|^Followup-To:
\\|^X-Newsreader:\\|^User-Agent:\\|^X-Mailer:
\\|Line:\\|Lines:\\|Content-Type:\\|NNTP-Posting-Host\\)")

;;
;;写消息时如何打开自动折行 (word-wrap) ？
;;
;; (add-hook 'message-mode-hook (lambda ()
;; 							   (setq fill-column 80)
;; 							   (turn-on-auto-fill)))

;;引用设置：不要原来的签名，引用全文
(setq message-cite-function 'message-cite-original-without-signature)
(add-hook 'mail-citation-hook 'sc-cite-original)


;;
;;如果开启了主题视图，只看未读邮件是令人讨厌的，在 ~/.gnus 里面加如这行：
;;
(setq gnus-fetch-old-headers 'some)
;;
;; topic mode 参考这里：(info "(gnus)Group Topics")
;;
(cond (window-system
	   (setq custom-background-mode 'light)
	   (defface my-group-face-1
		 '((t (:foreground "Red" :bold t))) "First group face")
	   (defface my-group-face-2
		 '((t (:foreground "DarkSeaGreen4" :bold t)))
		 "Second group face")
	   (defface my-group-face-3
		 '((t (:foreground "Green4" :bold t))) "Third group face")
	   (defface my-group-face-4
		 '((t (:foreground "SteelBlue" :bold t))) "Fourth group face")
	   (defface my-group-face-5
		 '((t (:foreground "Blue" :bold t))) "Fifth group face")))

(setq gnus-group-highlight
	  '(((> unread 200) . my-group-face-1)
		((and (< level 3) (zerop unread)) . my-group-face-2)
		((< level 3) . my-group-face-3)
		((zerop unread) . my-group-face-4)
		(t . my-group-face-5)))

(setq gnus-group-line-format
	  "%M\%S\%p\%P\%5y: %B%(%-40,40g%) %d\n")
;; (setq-default gnus-topic-topology'(("Mail" visible)
;; 								   ("Emacs" visible)
;; 								   (("Misc" visible))))
;; (setq-default gnus-topic-alist '(("nnimap+163:已发送"
;; 								  "nnimap+163:INBOX"
;; 								  "nnimap+streamocean:INBOX"
;; 								  "nnimap+streamocean:已发送"
;; 								  "nndraft:drafts" "Mail")))
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'mail-citation-hook 'sc-cite-original)

;;
;;压缩保存的邮件
;;
(setq nnml-use-compressed-files t)

;;
;;开启记分
;;
(setq gnus-use-adaptive-scoring t)
(setq gnus-save-score t)
(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(defvar gnus-default-adaptive-score-alist
  '((gnus-kill-file-mark (from -10))
	(gnus-unread-mark)
	(gnus-read-mark (from 10) (subject 30))
	(gnus-catchup-mark (subject -10))
	(gnus-killed-mark (from -1) (subject -30))
	(gnus-del-mark (from -2) (subject -15))
	(gnus-ticked-mark (from 10))
	(gnus-dormant-mark (from 5))))

(setq  gnus-score-find-score-files-function
	   '(gnus-score-find-hierarchical gnus-score-find-bnews bbdb/gnus-score)
	   gnus-use-adaptive-scoring t)

;;;
(setq gnus-confirm-mail-reply-to-news t
	  message-kill-buffer-on-exit t
	  message-elide-ellipsis "[...]\n"
	  )

;;
;;    如何存档有趣的消息？我将这个函数绑定在了F6键上
;;
;;    例如在 gnu.emacs.gnus 中，你偶然发现一个有趣的消息，想要存档，有好
;;    几种方法。第一种，也是最简单的，另存为文件`O f'。但是，从 Gnus 访问
;;    这样的存档文件并不方便。把 Frank Haun &lt;pille3003@fhaun.de&gt; 的这个代
;;    码片断放入 ~/.gnus：
;;
;; (defun my/archive-article (&optional n)
;;   "Copies one or more article(s) to a corresponding `nnml:' group, e.g.
;;       `gnus.ding' goes to `nnml:1.gnus.ding'. And `nnml:List-gnus.ding' goes
;;       to `nnml:1.List-gnus-ding'.

;;       Use process marks or mark a region in the summary buffer to archive
;;       more then one article."
;;   (interactive "P")
;;   (let ((archive-name
;; 		 (format "nnml:archives.%s"
;; 				 (if (featurep 'xemacs)
;; 					 (replace-in-string gnus-newsgroup-name "^.*:" "")
;; 				   (replace-regexp-in-string "^.*:" "" gnus-newsgroup-name)))))
;; 	(gnus-summary-copy-article n archive-name)))

;;
;;    此时，可以在 summary buffer 中用 `M-x my-archive-article' 把光标处
;;    的文章存档到一个 nnml 组（当然，可以改为你想要的其他后端）。
;;
;;    当然，也可以使用缓冲：
;;
(setq gnus-use-cache t)

;;
;;    这样，你只需设置 tick 或者 dormant 标记来保存，在缓冲中设置已读标记
;;    可以删除（文章）。
;;
;;另一种保存帖子的方法：
;;看到有价值的帖子，只要按下`*'键，这篇帖子就会被拷贝到本地的
;;cache中保存起来，即使服务器那边删除了帖子，也没关系了。如
;;果不想要了，用`Meta-*'就可以把帖子从缓存中删掉。
;;
;; (setq gnus-use-cache 'passive)

;;
;;中文！中文！永远都是头痛的事儿...Emacs23终于解决的这个问题 :-)
;;
;; 设置编码，这个是改变了整个emacs的编码！太恐怖了
;; (set-language-environment 'Chinese-GBK)
(setq gnus-default-charset 'utf-8)
(add-to-list 'gnus-group-charset-alist
			 '("\\(^\\|:\\)cn\\>\\|\\<chinese\\>" gbk))
(setq gnus-summary-show-article-charset-alist
	  '((1 . utf-8)
		(2 . big5)
		(3 . gb18030)
		(4 . gbk)
		(5 . gn2312)
		(6 . utf-7)))

(setq gnus-group-name-charset-group-alist
	  '(("\\.com\\.cn:" . gbk)
		("news\\.newsfan\\.net" . gbk)))

(setq gnus-group-name-charset-method-alist
	  '(((nntp "news.cn99.net") . gbk)))

(setq gnus-group-name-charset-method-alist
	  '(((nntp "news.newsfan.net") . gbk)))

(setq gnus-newsgroup-ignored-charsets
	  '(unknown-8bit x-unknown x-gbk gb18030))

;; 显示编码格式
(add-hook 'gnus-startup-hook
		  '(lambda ()
			 (setq gnus-visible-headers
				   (concat "^User-Agent:\\|^Content-Type:\\|"
						   "Content-Transfer-Encoding:\\|"
						   "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|"
						   gnus-visible-headers))))

;;设置发送风格
;; (setq gnus-posting-styles
;; 	  '(
;; 		;; all
;; 		(".*"
;; 		 (name "liyunteng")
;; 		 (address "li_yunteng@163.com")
;; 		 ;; (face (gnus-convert-png-to-face (concat emacsHome "/Gnus/xface.png")))
;; 		 (organization "StreamOcean")
;; 		 (signature "
;; oooOOOOoo...
;; >  Life is too short ! ...")
;; 		 (eval (setq mm-coding-system-priorities
;; 					 '(iso-8859-1 gb2312 gbk gb18030 utf-8)))
;; 		 ;;(body "")
;; 		 )
;; 		;;cn.bbs.com
;; 		("^cn\\.bbs\\.comp"
;; 		 (name "liyunteng")
;; 		 (address "li_yunteng@163.com")
;; 		 ;; (face (gnus-convert-png-to-face (concat emacsHome "/Gnus/xface.png")))
;; 		 (organization "lyt")
;; 		 (signature "
;; oooOOOOoo...
;; >  Life is too short ! ...")
;; 		 (eval (setq mm-coding-system-priorities
;; 					 '(iso-8859-1 gb2312 gbk gb18030 utf-8)))
;; 		 ;;(body "")
;; 		 )
;; 		;;tw
;; 		("^tw\\.comp"
;; 		 (name "abc")
;; 		 ;; (address "yourname@gmail.com")
;; 		 ;; (face (gnus-convert-png-to-face (concat emacsHome "/Gnus/xface.png")))
;; 		 (organization "abc")
;; 		 (signature "
;; oooOOOOoo...
;; >  Life is too short ! ...")
;; 		 (eval (setq mm-coding-system-priorities
;; 					 '(iso-8859-1 big5 utf-8)))
;; 		 ;;(body "")
;; 		 )
;; 		))


;;
;; 多窗口处理
;;
;;
;; From: Katsumi Yamaoka <yamaoka@jpl.org>
;; Subject: Re: multiple message frames
;; To: ding@gnus.org
;; Newsgroups: gnus.ding
;; Date: Thu, 11 Sep 2003 16:06:37 +0900
;;
;; Hi,
;;
;; This is the revised version of the "multiple message frames" suit.
;; It makes it possible to open multiple message frames and delete
;; each frame automatically after sending or killing it.  You can use
;; it by simply putting it in your .gnus.el file.  Enjoy!
;;

(let* ((default
		 ;; Winodw layout for normal message frames.
		 '(vertical
		   ((user-position . t)
			;;(left . -1) (top . 1)
			(width . 80) (height . 40))
		   (message 1.0 point)))
	   (bug
		;; Window layout for message frames reporting bugs.
		;; Note that multiple gnus-bug frames are not supported.
		'(vertical
		  ((user-position . t)
		   ;;(left . -1) (top . 1)
		   (width . 80) (height . 40))
		  (if gnus-bug-create-help-buffer '("*Gnus Help Bug*" 0.5))
		  ("*Gnus Bug*" 1.0 point)))
	   (config
		`(frame
		  1.0
		  (if (buffer-live-p gnus-summary-buffer)
			  (if (get-buffer gnus-article-buffer)
				  (car (cdr (assq 'article gnus-buffer-configuration)))
				(car (cdr (assq 'summary gnus-buffer-configuration))))
			(car (cdr (assq 'group gnus-buffer-configuration))))
		  ,default))
	   (settings '(compose-bounce forward mail-bounce message post
								  reply reply-yank)))
  (while settings
	(gnus-add-configuration (list (car settings) config))
	(setq settings (cdr settings)))
  (setcdr (nthcdr 2 (setq config (copy-sequence config))) (list bug))
  (gnus-add-configuration (list 'bug config)))

(add-hook
 'gnus-configure-windows-hook
 (lambda nil
   (if (eq major-mode 'message-mode)
	   (let* ((message-frame (selected-frame))
			  (delete-frame-function
			   `(lambda nil
				  (if (and
					   ;; Uncomment the following line if other windows
					   ;; in message frames are supposed to be important.
					   ;;(eq (selected-window) (next-window))
					   (eq (selected-frame) ,message-frame))
					  (delete-frame ,message-frame)))))
		 (setq gnus-frame-list (delq message-frame gnus-frame-list)
			   message-exit-actions `((funcall ,delete-frame-function))
			   message-postpone-actions message-exit-actions)
		 (if (or (featurep 'xemacs)
				 (< emacs-major-version 21))
			 (make-local-hook 'kill-buffer-hook))
		 (add-hook 'kill-buffer-hook `,delete-frame-function t t)))))

;; Don't popup a message frame when sending a queued message.
(add-hook
 'gnus-message-setup-hook
 (lambda nil
   (if (or (memq this-command '(gnus-draft-send-message
								gnus-draft-send-all-messages
								gnus-group-send-queue))
		   (and (featurep 'gnus-delay)
				(save-excursion
				  (save-restriction
					(widen)
					(message-narrow-to-headers)
					(re-search-forward
					 (concat "^" (regexp-quote gnus-delay-header)
							 ":\\s-+")
					 nil t)))))
	   (let ((config (copy-sequence gnus-buffer-configuration)))
		 (set (make-local-variable 'gnus-buffer-configuration)
			  (cons '(forward (vertical 1.0 (message 1.0 point)))
					(delq (assq 'forward config) config)))
		 (set (make-local-variable 'gnus-configure-windows-hook)
			  nil)))))

;;(gnus-compile)                          ;编译一些选项, 加快速度


;; 最后设置
;;

;; (gnus-compile)                          ;编译一些选项, 加快速度


(provide 'my-gnus)
;;; my-gnus.el ends here
