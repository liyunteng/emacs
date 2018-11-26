
(read-directory-name "abc: " "~/.emacs.d/" "abc.h" nil)
(read-file-name "abc: " "~/" "abc.h")

(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-other-file . ido))
(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-related-file . ido))

(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-related-file . helm-read-file-name-handler-1))



(defun foo/test ()
  (interactive)
  (message "%S" (completing-read "test: " '(a b c d e))))

(defun helm-foo/test-completing-read-handler (prompt collection
                                              predicate require-match
                                              initial-input hist def
                                              inherit-input-method
                                              name buffer)
  (helm-comp-read prompt collection :marked-candidates t
                  :name name
                  :buffer buffer))

(add-to-list 'helm-completing-read-handlers-alist
             '(foo/test . helm-foo/test-completing-read-handler))




;; string-match
(setq st "The quick fox jumped quickly.")

(string-match "\\(qu\\)\\(ick\\)" st)
(match-string 1 st)

;; `,,@
(setq b '(ba bb bc))
`b
`(a b c)
`(a ,b c)
`(a ,@b c)
