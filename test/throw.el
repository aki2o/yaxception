(require 'yaxception)

(ert-deftest throw/on-outside-of-catch ()
  "throw on outside of catch"
  (should-error
   (yaxception:throw 'wrong-type-argument)
   :type 'wrong-type-argument))

(ert-deftest throw/original-in-catch ()
  "throw original error in catch"
  (should-error
   (yaxception:$
     (yaxception:try
       (delete-file 'hoge))
     (yaxception:catch 'error e
       (yaxception:throw e)))
   :type 'wrong-type-argument))

(ert-deftest throw/custom-in-catch ()
  "throw custom error in catch"
  (unwind-protect
      (progn
        (define-error 'yaxception-test-error "This is YAX test" 'void-function)
        (should-error
         (yaxception:$
           (yaxception:try
             (delete-file 'hoge))
           (yaxception:catch 'error e
             (yaxception:throw 'yaxception-test-error :yax-a "hoge")))
       :type 'yaxception-test-error))
    (setplist 'yaxception-test-error nil)))
