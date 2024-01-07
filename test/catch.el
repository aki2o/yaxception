(require 'yaxception)

(ert-deftest catch/the-error ()
  "catch the error"
  (let ((i 0))
    (yaxception:$
      (yaxception:try
        (delete-file 'hoge)
        (setq i 10))
      (yaxception:catch 'wrong-type-argument e
        (cl-incf i 5))
      (yaxception:catch 'error e
        (cl-incf i 2)))
    (should (equal i 5))))

(ert-deftest catch/as-ancestor ()
  "catch as ancestor"
  (unwind-protect
      (progn
        (define-error 'yaxception-test-error "This is YAX test." 'file-error)
        (let ((i 0))
          (yaxception:$
            (yaxception:try
              (yaxception:throw 'yaxception-test-error)
              (setq i 10))
            (yaxception:catch 'file-error e
              (cl-incf i 5))
            (yaxception:catch 'error e
              (cl-incf i 2)))
          (should (equal i 5))))
    (setplist 'yaxception-test-error nil)))
