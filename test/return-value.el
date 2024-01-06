(require 'yaxception)

(ert-deftest return-value/non-error ()
  (let ((v (yaxception:$
             (yaxception:try
               "hoge"
               "fuga")
             (yaxception:catch 'error e
               "foo")
             (yaxception:finally
               "bar"))))
    (should (equal v "fuga"))))

(ert-deftest return-value/error ()
  (let ((v (yaxception:$
             (yaxception:try
               "hoge"
               (error "It's test error")
               "fuga")
             (yaxception:catch 'error e
               "foo")
             (yaxception:finally
               "bar"))))
    (should (equal v "foo"))))
