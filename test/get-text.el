(require 'yaxception)

(ert-deftest get-text/builtin ()
  "get-text with builtin error"
  (let ((ret ""))
    (yaxception:$
      (yaxception:try
        (replace-regexp-in-string " " "" 'yaxception))
      (yaxception:catch 'error e
        (setq ret (yaxception:get-text e))))
    (should (equal ret "Wrong type argument: sequencep, yaxception"))))

(ert-deftest get-text/original ()
  "get-text with original error"
  (unwind-protect
      (let ((ret ""))
        (define-error 'yaxception-test-error "This is YAX test" 'void-function)
        (yaxception:$
          (yaxception:try
            (yaxception:throw 'yaxception-test-error))
          (yaxception:catch 'error e
            (setq ret (yaxception:get-text e))))
        (should (equal ret "This is YAX test"))
        (yaxception:$
          (yaxception:try
            (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga"))
          (yaxception:catch 'error e
            (setq ret (yaxception:get-text e))))
        (should (equal ret "This is YAX test: :yax-a, \"hoge\", :yax-b, \"fuga\"")))
    (setplist 'yaxception-test-error nil)))

(ert-deftest get-text/wrong-argument ()
  "get-text with wrong argument"
  (let* ((ret "hoge")
         (err (should-error
               (yaxception:$
                 (yaxception:try
                   (replace-regexp-in-string " " "" 'yaxception))
                 (yaxception:catch 'error e
                   (setq ret (yaxception:get-text ret))))
               :type 'error)))
    (should (equal ret "hoge"))
    (should (equal (error-message-string err) "[yaxception:get-raw] Invalid argument : hoge"))))
