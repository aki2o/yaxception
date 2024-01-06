(require 'yaxception)

(ert-deftest catch/the-error ()
  (let ((i 0))
    (yaxception:$
      (yaxception:try
        (delete-file "not found")
        (setq i 10))
      (yaxception:catch 'file-error e
        (incf i 5))
      (yaxception:catch 'error e
        (incf i 2)))
    (should (equal i 5))))

(ert-deftest catch/as-ancestor ()
  (unwind-protect
      (progn
        (define-error 'yaxception-test-error "This is YAX test." 'file-error)
        (let ((i 0))
          (yaxception:$
            (yaxception:try
              (yaxception:throw 'yaxception-test-error)
              (setq i 10))
            (yaxception:catch 'file-error e
              (incf i 5))
            (yaxception:catch 'error e
              (incf i 2)))
          (should (equal i 5))))
    (setplist 'yaxception-test-error nil)))
