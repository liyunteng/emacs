;;; my-header.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  StreamOcean

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

(defconst header-history-label "Change Log:")
(defconst header-free-software
  "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>."
  )
(defconst header-copyright-notice "Copyright: (C) 2017 StreamOcean\n")
(defconst header-date-format "%Y/%m/%d %H:%M:%S")
(defconst make-box-comment-region-replace-prefix-flag nil)

(defvar c-style)
(defvar explicit-shell-file-name)
(defvar header-max 5000)
(defvar header-auto-update-enabled t)
(make-variable-buffer-local 'header-auto-update-enabled)
(when (boundp 'safe-local-variable-values)
  (add-to-list 'safe-local-variable-values '(header-auto-update-enabled)))

(defvar return-to nil)
(defvar header-multiline "")
(defvar file-header-update-alist ())
(defvar header-prefix-string "")

(defvar make-header-hook '(
                           ;; header-mode-line
                           ;; header-blank
                           header-file-name
                           header-description
                           header-author
                           ;; header-maintainer
                           header-copyright
                           header-creation-date
                           ;; header-version
                           header-modification-date
                           header-modification-author
                           header-update-count
                           header-blank
                           header-end-line
                           header-commentary
                           header-blank
                           header-blank
                           header-blank
                           header-end-line
                           header-history
                           header-blank
                           header-blank
                           header-end-line
                           ;; header-free-software
                           header-code
                           ;; header-eof
                           )
  "*Functions that insert header elements.
Each function is started on a new line and is expected to end in a new line.
Each function may insert any number of lines, but each line, including the
first, must be started with the value of `header-prefix-string'.
\(This variable holds the same value as would be returned by calling
`header-prefix-string' but is faster to access.)  Each function may set the
following global variables:

  `header-prefix-string' -- mode-specific comment sequence
  `return-to' -- character position to which point will be moved after header
                 functions are processed.  Any header function may set this,
                 but only the last setting will take effect.

It is reasonable to locally set these hooks according to certain modes.
For example, a table of contents might only apply to code development modes
and `header-shell' might only apply to shell scripts.  See instructions in
file `header2.el' to do this."
  )

(defsubst nonempty-comment-start ()
  "Return `comment-start', or nil if it is an empty ing."
  (and (not (equal "" comment-start))  comment-start))

(defsubst nonempty-comment-end ()
  "Return `comment-end', or nil if it is an empty string."
  (and (not (equal "" comment-end))  comment-end))

(defsubst header-blank ()
  "Insert an empty comment to file header (after `header-prefix-string')."
  (insert header-prefix-string  "\n"))

(defsubst section-comment-start ()
  "Comment start of major section headings."
  (if (= (length comment-start) 1)      ; e.g. Lisp: ";; \n;;;"
      (concat header-prefix-string "\n" comment-start header-prefix-string)
    (concat "\n" comment-start)))       ; e.g. C: "\n/*"


(defsubst header-title ()
  "Insert buffer's file name and leave room for a description.
In `emacs-lisp-mode', this should produce the title line for library
packages."
  (insert (concat comment-start (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " --- "))
  (insert (read-string "Short Description: ") "\n")
  (setq return-to  (1- (point))))

(defsubst header-file-name ()
  "Insert \"Filename: \" line, using buffer's file name."
  (insert header-prefix-string "Filename: "
          (if (buffer-file-name)
              (file-name-nondirectory (buffer-file-name))
            (buffer-name))
          "\n"))

(defsubst header-description ()
  "Insert \"Description: \" line."
  (insert header-prefix-string "Description: ")
  (insert (read-string "Description: ") "\n"))

(defsubst header-author ()
  "Insert current user's name (`user-full-name') as this file's author."
  (insert header-prefix-string "Author: " (user-full-name) "\n"))

(defsubst header-maintainer ()
  "Insert \"Maintainer: \" line."
  (insert header-prefix-string "Maintainer: \n"))

(defun header-copyright ()
  "Insert `header-copyright-notice', unless nil."
  (when header-copyright-notice
    (let ((start  (point)))
      (insert header-copyright-notice)
      (save-restriction
        (narrow-to-region start (point))
        (goto-char (point-min))
        ;; Must now insert header prefix.  Cannot just replace string,
        ;; because that would cause too many undo boundries.
        (insert header-prefix-string)
        (while (progn (skip-chars-forward "^\n") (looking-at "\n"))
          (forward-char 1) (unless (eolp) (insert header-prefix-string)))
        (goto-char (point-max))))))

(defsubst header-creation-date ()
  "Insert today's time, date, and time zone as file creation date."
  (insert header-prefix-string "Created:" (header-date-string) "\n"))

(defun header-date-string ()
  "Current date and time."
  (concat " " (format-time-string
               (cond ((stringp header-date-format) header-date-format)
                     (header-date-format "%a %b %e %T %Y (%z)")
                     (t                  "%Y-%m-%dT%T%z")) ; An alternative: "%a %b %e %T %Y (UTC)"
               (current-time)
               (not header-date-format))))

(defsubst header-version ()
  "Insert lines to record version information."
  (insert header-prefix-string "Version: \n"))

(defsubst header-commentary ()
  "Insert \"Commentary: \" line."
  (insert (concat (section-comment-start) "Commentary: \n")))

(defsubst header-history ()
  "Insert `header-history-label' into header for use by `make-revision'.
Without this, `make-revision' inserts `header-history-label' after the header."
  (insert (concat (section-comment-start) header-history-label "\n")))

(defun header-free-software ()
  "Insert text saying that this is free software."
  (let ((header-multiline  header-free-software))
    (header-multiline)))

(defun header-multiline ()
  "Insert multiline comment.  The comment text is in `header-multiline'."
  (let ((lineno  1)
        beg end nb-lines)
    (beginning-of-line)
    (if (nonempty-comment-end)
        (insert "\n" comment-start)
      (header-blank)
      (insert header-prefix-string))
    (setq beg  (point))
    (insert header-multiline)
    (setq end       (point-marker)
          nb-lines  (count-lines beg end))
    (goto-char beg)
    (forward-line 1)
    (while (< lineno nb-lines)
      (insert header-prefix-string)
      (forward-line 1)
      (setq lineno  (1+ lineno)))
    (goto-char end)
    (when (nonempty-comment-end) (insert "\n"))
    (insert comment-end)
    (insert "\n")
    (unless (nonempty-comment-end)
      (header-blank)
      (header-end-line))))

(defsubst header-code ()
  "Insert \"Code: \" line."
  (insert (concat (section-comment-start) "Code:" (nonempty-comment-end) "\n\n\n")))


(defsubst header-eof ()
  "Insert comment indicating end of file."
  (goto-char (point-max))
  (insert "\n")
  (unless (nonempty-comment-end) (header-end-line))
  (insert comment-start
          (concat (and (= 1 (length comment-start)) header-prefix-string)
                  (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))
                  " ends here"
                  (or (nonempty-comment-end) "\n"))))

(defsubst header-modification-date ()
  "Insert todays date as the time of last modification.
This is normally overwritten with each file save."
  (insert header-prefix-string "Last-Updated:\n"))

(defsubst header-modification-author ()
  "Insert current user's name as the last person who modified the file.
This is normally overwritten with each file save."
  (insert header-prefix-string "          By:\n"))

(defsubst header-update-count ()
  "Insert a count of the number of times the file has been saved."
  (insert header-prefix-string "    Update #: 0\n"))

(defsubst header-shell ()
  "Insert a kernal shell specifier line.
Uses the same shell named in `explicit-shell-file-name', the ESHELL
environment variable, the SHELL environment variable, or
'/bin/sh'.  (This is the same shell that the shell command uses.)"
  (insert "#!" (or (and (boundp 'explicit-shell-file-name)
                        explicit-shell-file-name)
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh")
          "\n"))

(defun header-mode-line ()
  "Insert a \" -*- Mode: \" line."
  (let* ((mode-declaration  (concat " -*- Mode: " (true-mode-name)
                                    (if (assoc 'c-style (buffer-local-variables))
                                        (concat "; C-Style: " (symbol-name c-style))
                                      "")
                                    " -*-"))
         (md-length         (length mode-declaration)))
    (insert (cond ((and comment-start (= 1 (length comment-start)))
                   ;; Assume comment start char is also fill char.
                   (concat comment-start comment-start
                           (make-string (/ (- 77 md-length) 2)
                                        (aref comment-start 0))
                           mode-declaration
                           (make-string (/ (- 78 md-length) 2)
                                        (aref comment-start 0))))
                  ((nonempty-comment-start) ; Assume spaces fill the gaps.
                   (concat comment-start
                           (make-string (/ (- 79 md-length
                                              (length comment-start)) 2)
                                        ?\ )
                           mode-declaration))
                  (t                    ; No comment-start.  Assume Lisp.
                   (concat ";;" (make-string (/ (- 77 md-length) 2) ?\;)
                           mode-declaration
                           (make-string (/ (- 78 md-length) 2) ?\;))))
            "\n")))

(defsubst header-end-line ()
  "Insert a divider line."
  (insert (cond ((nonempty-comment-end))
                ((and comment-start (= 1 (length comment-start)))
                 (make-string 70 (aref comment-start 0)))
                ((nonempty-comment-start))
                (t (make-string 70 ?\;)))
          "\n"))

(defun register-file-header-action (regexp function-to-call)
  "Record FUNCTION-TO-CALL as the action to take if REGEXP is found
in the file header when a file is written.  The function will be called
with the cursor located just after the matched REGEXP.  Calling this twice
with the same args overwrites the previous FUNCTION-TO-CALL."
  (let ((ml  (assoc regexp file-header-update-alist)))
    (if ml
        (setcdr ml function-to-call);; overwrite old defn
      ;; This entry is new to us.  Add to the master alist
      (setq file-header-update-alist  (cons (cons regexp function-to-call)
                                            file-header-update-alist)))))

(register-file-header-action "Last-Updated[ \t]*:" 'update-last-modified-date)
(register-file-header-action "          By[ \t]*:" 'update-last-modifier)
(register-file-header-action "    Update #[ \t]*:" 'update-write-count)

(defun true-mode-name ()
  "Return name of mode in a form such that mode may be re-established
by calling the function named by appending \"-name\" to this string.
This differs from variable `mode-name' in that this is guaranteed to
work even when the value has embedded spaces or other junk."
  (let ((major-mode-name  (symbol-name major-mode)))
    (capitalize (substring major-mode-name 0
                           (or   (string-match "-mode" major-mode-name)
                                 (length major-mode-name))))))

(defun header-prefix-string ()
  "Return a mode-specific prefix string for use in headers.
It is sensitive to language-dependent comment conventions."
  (cond
   ;; E.g. Lisp.
   ((and comment-start (= 1 (length comment-start)))
    (concat comment-start comment-start " "))

   ;; E.g. C++ and ADA.
   ;; Special case, three letter comment-start where the first and
   ;; second letters are the same.
   ((and comment-start (= 3 (length comment-start))
         (equal (aref comment-start 1) (aref comment-start 0)))
    comment-start)

   ;; E.g. C.
   ;; Other three-letter comment-start -> grab the middle character
   ((and comment-start (= 3 (length comment-start)))
    (concat " " (list (aref comment-start 1)) " "))

   ((and comment-start  (not (nonempty-comment-end)))

    ;; Note: no comment end implies that the full comment-start must be
    ;; used on each line.
    comment-start)
   (t ";; ")))

(defun auto-make-header ()
  "Call `make-header' if current buffer is empty and is a file buffer."
  (and (zerop (buffer-size)) (not buffer-read-only) (buffer-file-name)
       (make-header)))

(defun make-header ()
  "Insert (mode-dependent) header comment at beginning of file.
A header is composed of a mode line, a body, and an end line.  The body is
constructed by calling the functions in `make-header-hook'.  The mode line
and end lines start and terminate block comments.  The body lines continue
the comment."
  (interactive)
  (beginning-of-buffer)                 ; Leave mark at old location.
  (insert "\n")
  (beginning-of-buffer)                 ; Leave mark at old location.
  (let* ((return-to             nil)    ; To be set by `make-header-hook'.
         (header-prefix-string  (header-prefix-string))) ; Cache result.
    (mapcar #'funcall make-header-hook)
    (when return-to (goto-char return-to))))

(defun make-revision ()
  "Prepare for a new history revision.  Insert history line if inexistant."
  (interactive)
  (setq comment-start  (or comment-start "\;")) ; Use Lisp comment as default.
  (let ((header-prefix-string   (header-prefix-string))
        (logical-comment-start  (if (= 1 (length comment-start))
                                    (concat comment-start comment-start " ")
                                  comment-start)))
    ;; Look for the history line
    (beginning-of-buffer)               ; Leave a mark behind.
    (if (re-search-forward (concat "^\\(" (and comment-start
                                               (regexp-quote comment-start))
                                   (regexp-quote (header-prefix-string)) "\\|"
                                   (if (and comment-start
                                            (not (string= "" comment-start)))
                                       (concat "\\|"
                                               (regexp-quote comment-start))
                                     "")
                                   "\\)" " *\\(" header-history-label
                                   "\\|HISTORY\\)") ; Backward compatibility.
                           header-max t)
        (end-of-line)
      ;; We did not find a history line, add one
      (goto-char (point-min))
      ;; find the first line that is not part of the header
      (while (and (< (point) header-max)
                  (looking-at (concat "[ \t]*\\("
                                      (regexp-quote (header-prefix-string))
                                      (if (and comment-start
                                               (not (string= "" comment-start)))
                                          (concat "\\|" (regexp-quote comment-start))
                                        "")
                                      (if (and comment-end (not (string= "" comment-end)))
                                          (concat "\\|" (regexp-quote comment-end))
                                        "")
                                      "\\)")))
        (forward-line 1))
      (insert "\n" logical-comment-start header-history-label)
      (save-excursion (insert "\n" comment-end)))
    ;; We are now on the line with the header-history-label label
    (insert "\n" header-prefix-string
            (let ((str  (current-time-string)))
              (concat (if (equal ?\  (aref str 8))
                          (substring str 9 10)
                        (substring str 8 10))
                      "-" (substring str 4 7) "-" (substring str 20 24)))
            "    " (user-full-name)
            ;;"  |>Ident<|\n"
            "\n" header-prefix-string "   ")
    ;; Add details about the history of the file before its modification
    (when (save-excursion (re-search-backward "Last-Updated[ \t]*:[ \t]*\\(.+\\)$" nil t))
      (insert "Last-Updated: " (buffer-substring (match-beginning 1) (match-end 1)))
      (when (save-excursion (re-search-backward "    Update #[ \t]*:[ \t]*\\([0-9]+\\)$" nil t))
        (insert " #" (buffer-substring (match-beginning 1) (match-end 1))))
      (when (save-excursion (re-search-backward "          By[ \t]*:[ \t]*\\(.+\\)$" nil t))
        (insert " (" (buffer-substring (match-beginning 1) (match-end 1)) ")"))
      (insert "\n" header-prefix-string "   "))))

(defun make-divider (&optional end-col)
  "Insert a comment divider line: the comment start, filler, and end.
The width is `fill-column', by default.  With a numeric prefix arg,
use that as the width, except use at least 4 columns."
  (interactive "P")
  (setq end-col  (if end-col (prefix-numeric-value end-col) fill-column))
  (insert comment-start)
  (when (= 1 (length comment-start)) (insert comment-start))
  (insert (make-string (max 2 (- end-col (length comment-end) (current-column)))
                       (aref comment-start (if (= 1 (length comment-start)) 0 1)))
          comment-end
          "\n"))

(defun make-box-comment (&optional end-col)
  "Insert an empty (mode dependent) box comment.
The maxium width is `fill-column', by default.  With a numeric prefix
arg, use that as the maximum width, except use at least 2 + the length
returned by function `header-prefix-string'."
  (interactive "P")
  (setq end-col  (if end-col (prefix-numeric-value end-col) fill-column))
  (unless (= 0 (current-column)) (forward-line 1))
  (insert comment-start)
  (when (= 1 (length comment-start)) (insert comment-start))
  (unless (char-equal (preceding-char) ?\  ) (insert ?\  ))
  (insert (make-string (max 2 (- end-col (length comment-end) (current-column)))
                       (aref comment-start (if (= 1 (length comment-start)) 0 1)))
          "\n"
          (header-prefix-string))
  (save-excursion
    (insert "\n"
            (header-prefix-string)
            (make-string (max 2 (- end-col (length comment-end) (current-column)))
                         (aref comment-start (if (= 1 (length comment-start)) 0 1)))
            comment-end
            "\n")))

(defun make-box-comment-region (&optional end-col start end)
  "Wrap active region in a box comment, or make an empty box comment.
The maxium width is `fill-column', by default.  With a numeric prefix
arg, use that as the maximum width, except use at least 2 + the length
returned by function `header-prefix-string'.
Respects `make-box-comment-region-remove-comments'."
  (interactive "P\nr")
  (setq end-col  (if end-col (prefix-numeric-value end-col) fill-column))
  (if (not (and mark-active  (mark)  (> (region-end) (region-beginning))))
      (make-box-comment end-col)
    (let ((selection    (buffer-substring start end)))
      (kill-region start end)
      (make-box-comment end-col)
      (insert
       (replace-regexp-in-string "\n"
                                 (concat "\n" (header-prefix-string))
                                 (if make-box-comment-region-replace-prefix-flag
                                     (replace-regexp-in-string
                                      (concat "^[ \t]*[" (nonempty-comment-start) "]*")
                                      ""
                                      selection)
                                   selection))))))

(defun update-file-header ()
  "Update file header.
Search the first `header-max' chars in buffer using regexps in
`file-header-update-alist'.  When a match is found, apply the
corresponding function with point located just after the match.
The functions can use `match-beginning' and `match-end' to find
the strings that cause them to be invoked."
  (interactive)
  (save-excursion
    (save-restriction                   ; Only search `header-max' chars.
      (narrow-to-region 1 (min header-max (1- (buffer-size))))
      (let ((patterns  file-header-update-alist))
        ;; Do not record this call as a command in command history.
        (setq last-command  nil)
        (while patterns
          (goto-char (point-min))
          (when (re-search-forward (car (car patterns)) nil t)
            ;; Position cursor at end of match.
            (goto-char (match-end 0))
            ;;(message "do %s" (car patterns)) (sit-for 1)
            (funcall (cdr (car patterns))))
          (setq patterns  (cdr patterns)))))))

(defun auto-update-file-header ()
  "Update file header if file is modified.
Call `update-file-header' if:
 `header-auto-update-enabled' is non-nil,
 the file is modified,
 it is longer than 100 chars,
 and the buffer is not read-only.
Return nil, for use on a hook."
  (and header-auto-update-enabled
       (> (buffer-size) 100)
       (buffer-modified-p)
       (not buffer-read-only)
       (update-file-header)
       nil))

(defsubst delete-and-forget-line ()
  "Delete current line and return it.  Do not add it to the `kill-ring'."
  (let* ((start  (point))
         (stop   (line-end-position))
         (str    (buffer-substring start stop)))
    (delete-region start stop)
    str))

(defun update-write-count ()
  (let* ((str  (delete-and-forget-line))
         (rem  (read-from-string str))
         (num  (car rem)))
    (if (numberp num)
        (insert (format " %s" (1+ num)) (substring str (cdr rem)))
      (insert str)
      (error "Invalid number for update count `%s'" str))))

(defsubst update-last-modifier ()
  "Update the line that indicates who last modified the file."
  (delete-and-forget-line)
  (insert (format " %s" (let ((ufn  (user-full-name)))
                          (if (and ufn (not (string= "" ufn))) ufn (user-login-name))))))

(defsubst update-last-modified-date ()
  "Update the line that indicates the last-modified date."
  (delete-and-forget-line)
  (insert (header-date-string)))

(defun update-file-name ()
  "Update the line that indicates the file name."
  (beginning-of-line)
  ;; Verify looking at a file name for this mode.
  (when (looking-at (concat (regexp-quote (header-prefix-string)) " *\\(.*\\) *\\-\\-"))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (file-name-nondirectory (buffer-file-name)) " ---")))


(provide 'my-header)
;;; header.el ends here
