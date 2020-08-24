;;; my-ibuffer.el --- ibuffer                        -*- lexical-binding: t; -*-

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

(use-package ibuffer
  :bind (("C-X C-b" . ibuffer))
  :init
  (use-package ibuffer-vc
    :ensure t
    :init
    (defun ibuffer-set-up-preferred-filters ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'filename/process)
	    (ibuffer-do-sort-by-filename/process)))

    (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters))

  (use-package ibuffer-projectile
    :ensure t
    :config
    (defun my/ibuffer-group-by-projects ()
      (interactive)
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
	    (ibuffer-do-sort-by-alphabetic))
      )
    (define-key ibuffer-mode-map (kbd "/ p") 'my/ibuffer-group-by-projects))

  :config
  (use-package ibuf-ext
    :init
    (setq ibuffer-never-show-predicates nil)
    ;; (add-to-list 'ibuffer-never-show-predicates "^\\*")
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-filter-group-name-face 'font-lock-doc-face)

    :config
    ;; 使用/ r来进行切换
    (setq ibuffer-saved-filters
          (append '(("c" (or
	                      (mode . c-mode)
	                      (mode . c++-mode)
	                      (mode . makefile-gmake-mode)
	                      (mode . asm-mode)))

                    ("lisp" (or
		                     (mode . emacs-lisp-mode)
		                     (mode . lisp-mode)
		                     (mode . lisp-interaction-mode)
		                     (mode . inferior-emacs-lisp-mode)))

                    ("go" (mode . go-mode))

                    ("file" (filename . ".*")))
                  ibuffer-saved-filters))


    (defun my--ibuffer-get-major-modes-ibuffer-rules-list (mm-list result-list)
      (if mm-list
          (let* ((cur-mm (car mm-list))
                 (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
            (my--ibuffer-get-major-modes-ibuffer-rules-list
             (cdr mm-list) (cons next-res-list-el result-list)))
        result-list))

    (defun my--ibuffer-get-major-modes-list ()
      (mapcar
       (function (lambda (buffer)
                   (buffer-local-value 'major-mode (get-buffer buffer))))
       (buffer-list (selected-frame))))

    (defun my/ibuffer-create-buffs-group ()
      (interactive)
      (let* ((ignore-modes '(Buffer-menu-mode
                             compilation-mode
                             minibuffer-inactive-mode
                             ibuffer-mode
                             magit-process-mode
                             messages-buffer-mode
                             fundamental-mode
                             completion-list-mode
                             help-modenn
                             Info-mode
                             helm-major-mode
                             ))
             (cur-bufs
              (list (cons "Mode"
                          (my--ibuffer-get-major-modes-ibuffer-rules-list
                           (cl-set-difference
                            (cl-remove-duplicates
                             (my--ibuffer-get-major-modes-list)) ignore-modes)
                           '()))

                    (cons "File" '(("dir" (mode . dired-mode))
                        		   ("file" (filename . ".*"))
                                   ("special" (derived-mode . special-mode))))
                    )))
        (setq ibuffer-saved-filter-groups cur-bufs)
        ;; (ibuffer-switch-to-saved-filter-groups "Home")
        ))
    (add-hook 'ibuffer-mode-hook 'my/ibuffer-create-buffs-group)


    ;; 使用/ g来进行切换
    ;; (setq ibuffer-saved-filter-groups
    ;;       (list
    ;;        (cons "vc"  '(("Git:~/git/doc" (vc-root Git  . "~/git/doc/"))
    ;;     		 ("Git:~/.emacs.d" (vc-root Git . "~/.emacs.d/"))
    ;;     		 ("Git:~/git/app" (vc-root Git  . "~/git/app/"))
    ;;     		 ("Git:~/git/test" (vc-root Git  . "~/git/test/"))
    ;;     		 ("Git:~/git/linux-stable" (vc-root Git . "~/git/linux-stable/"))))

    ;;        (cons "i-dir"  '(("/etc" (filename . "/etc"))
    ;;     		    ;; ("~/.emacs.d/lisp" (filename . ".*/.emacs.d/lisp"))
    ;;     		    ;; ("~/.emacs.d/elpa" (filename . ".*/.emacs.d/elpa"))
    ;;     		    ("~/.emacs.d" (filename . ".*/.emacs.d"))
    ;;     		    ;; ("~/git/doc" (filename . ".*/git/doc"))
    ;;     		    ("~/work" (filename . ".*/work/"))
    ;;     		    ("~/git/" (filename . ".*/git"))
    ;;     		    ("home" (filename . "/home"))
    ;;     		    ("/usr/share" (filename . "/usr/share"))
    ;;     		    ("/usr/src" (filename . "/usr/src"))
    ;;     		    ("/usr/include" (filename . "/usr/include"))
    ;;     		    ("/usr/local" (filename ."/usr/local"))
    ;;     		    ("/usr" (filename . "/usr"))
    ;;     		    ("/var/log" (filename . "/var/log"))
    ;;     		    ("/var" (filename . "/var"))
    ;;     		    ("/sys" (filename . "/sys"))
    ;;     		    ("/proc" (filename . "/proc"))
    ;;     		    ("/mnt" (filename . "/mnt"))
    ;;     		    ("/media" (filename . "/media"))))

    ;;        (cons "ext" '(("*buffer*" (name . "\\*.*\\*"))
    ;;     		 ("TAGS" (name . "^TAGS\\(<[0-9]+>\\)?$"))
    ;;     		 ("dired" (mode . dired-mode))))))
    )
  (use-package ibuf-macs
    :config
    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))
    ;; Modify the default ibuffer-formats (toggle with `)
    (setq ibuffer-formats
	      '((mark modified read-only vc-status-mini " "
		          (name 30 30 :left :elide)
		          " "
		          (size-h 9 -1 :right)
		          " "
		          (mode 16 16 :left :elide)
		          " "
		          filename-and-process)
	        (mark modified read-only vc-status-mini " "
		          (name 30 30 :left :elide)
		          " "
		          (size-h 9 -1 :right)
		          " "
		          (mode 16 16 :left :elide)
		          " "
		          (vc-status 16 16 :left)
		          " "
		          filename-and-process))))

  (define-key ibuffer-mode-map (kbd "/ g") 'ibuffer-switch-to-saved-filter-groups)
  ;; 禁用filter-groups decompose 和 pop
  (define-key ibuffer-mode-map (kbd "/ D") nil)
  ;; (define-key ibuffer-mode-map (kbd "/ P") nil)
  (define-key ibuffer-mode-map (kbd "/ d") nil)

  (define-key ibuffer-mode-map (kbd "/ S") nil)
  (define-key ibuffer-mode-map (kbd "/ X") nil)
  (define-key ibuffer-mode-map (kbd "/ s") nil)
  (define-key ibuffer-mode-map (kbd "/ x") nil)

  (define-key ibuffer-mode-map (kbd "s c") 'ibuffer-do-sort-by-vc-status)
  (define-key ibuffer-mode-map (kbd "s r") 'ibuffer-invert-sorting)
  (define-key ibuffer-mode-map (kbd "s f") 'ibuffer-do-sort-by-filename/process)
  (define-key ibuffer-mode-map (kbd "s a") 'ibuffer-do-sort-by-alphabetic)
  (define-key ibuffer-mode-map (kbd "s m") 'ibuffer-do-sort-by-major-mode)
  (define-key ibuffer-mode-map (kbd "s n") 'ibuffer-do-sort-by-mode-name)
  (define-key ibuffer-mode-map (kbd "s s") 'ibuffer-do-sort-by-size)
  (define-key ibuffer-mode-map (kbd "s v") 'ibuffer-do-sort-by-recency)
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-do-sort-by-filename/process)))

  (defun my-ibuffer-mode-hook ()
    ;; (ibuffer-switch-to-saved-filter-groups "a")
    (ibuffer-auto-mode t))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook))

(provide 'my-ibuffer)
;;; my-ibuffer.el ends here
