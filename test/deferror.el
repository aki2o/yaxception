(require 'yaxception)
(require 'el-expectations)


(expectations
  (desc "deferror created yaxception object")
  (expect t
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-p y)))
  (desc "deferror regist yaxception name")
  (expect "yaxception-test-error"
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-name y)))
  (desc "deferror not regist yaxception parent")
  (expect 'error
    (yaxception:deferror 'yaxception-test-error nil "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-parent y)))
  (desc "deferror regist yaxception parent")
  (expect 'file-error
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-parent y)))
  (desc "deferror regist yaxception msgtmpl")
  (expect "This is YAX test."
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-msgtmpl y)))
  (desc "deferror not regist yaxception tmplkeys")
  (expect nil
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-tmplkeys y)))
  (desc "deferror regist yaxception tmplkeys")
  (expect '(yax-a yax-b)
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test." 'yax-a 'yax-b)
    (let* ((y (gethash "yaxception-test-error" yaxception-custom-err-hash)))
      (yaxception-tmplkeys y)))
  (desc "deferror set error symbol conditions")
  (expect '(yaxception-test-error file-error)
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX test.")
    (get 'yaxception-test-error 'error-conditions))
  (desc "deferror set error symbol message")
  (expect "This is YAX '%s' test."
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (get 'yaxception-test-error 'error-message))
  )

