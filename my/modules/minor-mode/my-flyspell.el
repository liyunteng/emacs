;;; my-flyspell.el --- flyspell                      -*- lexical-binding: t; -*-

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

(use-package flyspell
  :defer t
  :ensure t
  :config  ;; flyspell setup for web-mode
  (defun web-mode-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face))
	   rlt)
      (cond
       ;; Check the words with these font faces, possibly.
       ;; This *blacklist* will be tweaked in next condition
       ((not (memq f '(web-mode-html-attr-value-face
		       web-mode-html-tag-face
		       web-mode-html-attr-name-face
		       web-mode-constant-face
		       web-mode-doctype-face
		       web-mode-keyword-face
		       web-mode-comment-face ;; focus on get html label right
		       web-mode-function-name-face
		       web-mode-variable-name-face
		       web-mode-css-property-name-face
		       web-mode-css-selector-face
		       web-mode-css-color-face
		       web-mode-type-face
		       web-mode-block-control-face)))
	(setq rlt t))
       ;; check attribute value under certain conditions
       ((memq f '(web-mode-html-attr-value-face))
	(save-excursion
	  (search-backward-regexp "=['\"]" (line-beginning-position) t)
	  (backward-char)
	  (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
				  (thing-at-point 'symbol)))))
       ;; finalize the blacklist
       (t
	(setq rlt nil)))
      rlt))
  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
  ;;

  ;; flyspell setup for js2-mode
  (defun js-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face)))
      ;; *whitelist*
      ;; only words with following font face will be checked
      (memq f '(js2-function-call
		js2-function-param
		js2-object-property
		font-lock-variable-name-face
		font-lock-string-face
		font-lock-function-name-face
		font-lock-builtin-face
		rjsx-tag
		rjsx-attr))))
  (put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
  (put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)
  ;;


  (use-package ispell
    :config
    ;; if (aspell installed) { use aspell}
    ;; else if (hunspell installed) { use hunspell }
    ;; whatever spell checker I use, I always use English dictionary
    ;; I prefer use aspell because:
    ;; 1. aspell is older
    ;; 2. looks Kevin Atkinson still get some road map for aspell:
    ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
    (defun flyspell-detect-ispell-args (&optional run-together)
      "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER will make aspell less capable. So it should only be used in prog-mode-hook."
      (let (args)
	(when ispell-program-name
	  (cond
	   ((string-match "aspell$" ispell-program-name)
	    ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
	    (setq args (list "--sug-mode=ultra" "--lang=en_US"))
	    (if run-together
		(setq args (append args '("--run-together" "--run-together-limit=16" "--run-together-min=2")))))
	   ((string-match "hunspell$" ispell-program-name)
	    (setq args nil))))
	args))

    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell"))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
	    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
     (t (setq ispell-program-name nil)
	(message "You need install either aspell or hunspell for ispell")))

    ;; Aspell Setup (recommended):
    ;; Skipped because it's easy.
    ;;
    ;; Hunspell Setup:
    ;; 1. Install hunspell from http://hunspell.sourceforge.net/
    ;; 2. Download openoffice dictionary extension from
    ;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
    ;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
    ;; the contents to a temporary folder.
    ;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
    ;; save dictionary files; I saved it to `~/usr_local/share/hunspell/'
    ;; 5. Add that path to shell env variable `DICPATH':
    ;; setenv DICPATH $MYLOCAL/share/hunspell
    ;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
    ;; variable is effective.
    ;;
    ;; hunspell will search for a dictionary called `en_US' in the path specified by
    ;; `$DICPATH'

    ;; `ispell-cmd-args' contains *extra* arguments appending to CLI process
    ;; when (ispell-send-string). Useless!
    ;; `ispell-extra-args' is *always* used when start CLI aspell process
    (setq ispell-extra-args (flyspell-detect-ispell-args t))
    ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
    (defadvice ispell-word (around my-ispell-word activate)
      (let ((old-ispell-extra-args ispell-extra-args))
	(ispell-kill-ispell t)
	;; use emacs original arguments
	(setq ispell-extra-args (flyspell-detect-ispell-args))
	ad-do-it
	;; restore our own ispell arguments
	(setq ispell-extra-args old-ispell-extra-args)
	(ispell-kill-ispell t)
	))
    )

  (use-package flyspell-correct
    ;; :bind
    ;; (:map flyspell-mode-map
    ;; 	  ("C-M-i" . flyspell-correct-at-point)
    ;; 	  ("C-;" . flyspell-correct-previous-word-generic))
    :ensure t
    :init
    (flyspell-correct-auto-mode t))
  (use-package flyspell-correct-helm
    :ensure t)

  ;; (defun my/clean-aspell-dict ()
  ;; 	"Clean ~/.aspell.pws (dictionary used by aspell)."
  ;; 	(interactive)
  ;; 	(let* ((dict (file-truename "~/.aspell.en.pws"))
  ;; 		   (lines (read-lines dict))
  ;; 		   ;; sort words
  ;; 		   (aspell-words (sort (cdr lines) 'string<)))
  ;; 	  (with-temp-file dict
  ;; 		(insert (format "%s %d\n%s"
  ;; 						"personal_ws-1.1 en"
  ;; 						(length aspell-words)
  ;; 						(mapconcat 'identity aspell-words "\n"))))))


  ;; ;; better performance
  (setq flyspell-issue-message-flag nil)
  )


(provide 'my-flyspell)
;;; my-flyspell.el ends here
