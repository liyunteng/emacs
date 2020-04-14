
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

(defun helm-foo/test-completing-read-handler (prompt
                                              collection
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
;; vector

(read-directory-name "abc: " "~/.emacs.d/" "abc.h" nil)
(read-file-name "abc: " "~/" "abc.h")

(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-other-file . ido))
(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-related-file . ido))

(add-to-list 'helm-completing-read-handlers-alist
             '(ff-find-related-file . helm-read-file-name-handler-1))

;; vector
(setq v (vector 1 2 3 4 5 "5"))
(wlength v)
(elt v 4)
(aref v 1)
(reverse v)
v
(nreverse v)
v

(setq v (vector  4 2 3 1))
(sort v '>)

;; struct
(cl-defstruct person name age sex)
(setq me (make-person :name "liyunteng" :age 30 :sex "man"))
(person-name me)
(person-p me)
(cl-incf (person-age me))
(person-p (copy-person me))

(cl-defstruct person (name nil :read-only t)
              age
              (sex 'unknown))
(make-person :name "liyunteng")


(cl-defstruct (person (:constructor create-person)
                      (:type list)
                      :named)
  name age sex)
(create-person :name "liyunteng" :age 30 :sex "man")

(cl-defstruct
    (person (:constructor nil)
            (:constructor new-person
                          (name sex &optional (age 0)))
            (:constructor new-hound (&key (name "Rover")
                                          (dog-years 0)
                                          &aux (age (* 7 dog-years))
                                          (sex "woman"))))
  name age sex
  )
(new-person "liyunteng" "man")
(new-hound :dog-years 3)



(setq a '(1 2 3 4 5))
(mapc '1+ a)
(mapcar '1+ a)

(mapcar 'list a)
(mapcan 'list a)

(mapconcat 'symbol-name '(The cat in the hat) " ")
(mapconcat (lambda (x) (format "%d" x)) a " ")


;; ==
(defalias 'a (function
              (lambda ()
                "ABCD"
                (message "abcd"))))
(defun a ()
  "ABCD"
  (message "abcd"))
(regexp-opt '("abc" "abcd" "abcdefg" "abcdefgh"))


;;  notify
(defun my-callback (event)
  (message "%s" event))
(file-notify-add-watch "/home/lyt/test" '(change) 'my-callback)
(file-notify-rm-watch '(2 . 9))


;; thread
(defun my/log (val)
  (concat (format-time-string "[%Y-%m-%d %H:%M:%S] "
                              (current-time))
          val))
(defun threadfun ()
  (message (my/log "begin running ..."))
  (sleep-for 1)
  (messagg (my/log "running at point 1"))
  (sleep-for 5)
  (message (my/log "running at point 6"))
  (sleep-for 10)
  (message (my/log "running at point 16"))
  (sleep-for 5)
  (message (my/log "finished"))
  )


;; async
(let ((async-shell-command-buffer 'new-buffer))
  (async-start-process "abc" (executable-find "pip")

                       (lambda (result)
                         (message "Result: %s" result)
                         (kill-buffer "*abc*")
                         )

                       "install" "requests" "--user"
                       ))
