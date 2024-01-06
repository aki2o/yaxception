(require 'yaxception)

(ert-deftest get-prop ()
  "get-prop"
  (unwind-protect
      (let (err)
        (define-error 'yaxception-test-error "This is YAX test" 'void-function)
        (yaxception:$
          (yaxception:try
            (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))
          (yaxception:catch 'error e
            (setq err e)))
        (should (equal (yaxception:get-prop err :yax-a) "hoge"))
        (should (equal (yaxception:get-prop err :yax-b) "fuga"))
        (should (equal (yaxception:get-prop err 'yax-a) "hoge"))
        (should (equal (yaxception:get-prop err :yax-c) nil)))
    (setplist 'yaxception-test-error nil)))
