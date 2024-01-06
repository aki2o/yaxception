(require 'yaxception)
(require 's)

(ert-deftest get-stack-trace-string/simple ()
  "get-stack-trace-string from simple call"
  (let* ((err nil)
         (expected "Wrong type argument: sequencep, yaxception-unknown
  at replace-regexp-in-string(\" \" \"\" yaxception-unknown)"))
    (yaxception:$
      (yaxception:try
        (replace-regexp-in-string " " "" 'yaxception-unknown))
      (yaxception:catch 'wrong-type-argument e
        (setq err e)))
    (should (s-starts-with? expected (yaxception:get-stack-trace-string err)))
    (should (>
             (length (yaxception:get-stack-trace-string err))
             (length expected)))
    (should (equal
             (yaxception:get-stack-trace-string err :limit 1)
             expected))
    (should (equal
             (yaxception:get-stack-trace-string err :filter (lambda (x) (s-starts-with? "replace-" x)))
             expected))))

(ert-deftest get-stack-trace-string/complex ()
  "get-stack-trace-string from complex call"
  (unwind-protect
      (let* ((err nil)
             (expected "Wrong type argument: sequencep, yaxception-unknown
  at replace-regexp-in-string(\" \" \"\" yaxception-unknown)
  at yaxception-ccc()
  at yaxception-bbb()
  at yaxception-aaa()"))
        (defun yaxception-aaa () (yaxception-bbb))
        (defun yaxception-bbb () (yaxception-ccc))
        (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-unknown))
        (yaxception:$
          (yaxception:try
            (yaxception-aaa))
          (yaxception:catch 'wrong-type-argument e
            (setq err e)))
        (should (equal
                 (yaxception:get-stack-trace-string err :limit 4)
                 expected)))
    (fmakunbound 'yaxception-aaa)
    (fmakunbound 'yaxception-bbb)
    (fmakunbound 'yaxception-ccc)))

(ert-deftest get-stack-trace-string/nest ()
  "get-stack-trace-string from nested block"
  (unwind-protect
      (let* ((err nil)
             (expected "Wrong type argument: sequencep, yaxception-unknown
  at replace-regexp-in-string(\" \" \"\" yaxception-unknown)
  at yaxception-ccc()
  at yaxception-bbb()
  at yaxception-aaa()"))
        (defun yaxception-aaa () (yaxception:$ (yaxception:try (yaxception-bbb))))
        (defun yaxception-bbb () (yaxception-ccc))
        (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-unknown))
        (yaxception:$
          (yaxception:try
            (yaxception-aaa))
          (yaxception:catch 'wrong-type-argument e
            (setq err e)))
        (should (equal
                 (yaxception:get-stack-trace-string err :limit 4)
                 expected)))
    (fmakunbound 'yaxception-aaa)
    (fmakunbound 'yaxception-bbb)
    (fmakunbound 'yaxception-ccc)))

(ert-deftest get-stack-trace-string/throw-in-catch ()
  "get-stack-trace-string from throw in catch"
  (unwind-protect
      (let* ((err nil)
             (expected "Wrong type argument: sequencep, yaxception-unknown
  at signal(wrong-type-argument (sequencep yaxception-unknown))"))
        (defun yaxception-aaa () (yaxception-bbb))
        (defun yaxception-bbb () (yaxception-ccc))
        (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-unknown))
        (yaxception:$
          (yaxception:try
            (yaxception:$
              (yaxception:try
                (yaxception-aaa))
              (yaxception:catch 'wrong-type-argument e
                (yaxception:throw e))))
          (yaxception:catch 'error e
            (setq err e)))
        (should (equal
                 (yaxception:get-stack-trace-string err :limit 1)
                 expected))
        (should (equal
                 (yaxception:get-stack-trace-string err :filter (lambda (x) (s-starts-with? "yaxception-" x)))
                 "Wrong type argument: sequencep, yaxception-unknown\n")))
    (fmakunbound 'yaxception-aaa)
    (fmakunbound 'yaxception-bbb)
    (fmakunbound 'yaxception-ccc)))

(ert-deftest get-stack-trace-string/throw-in-finally ()
  "get-stack-trace-string from throw in finally"
  (unwind-protect
      (let* ((err nil)
             (expected "Wrong type argument: sequencep, yaxception-unknown
  at replace-regexp-in-string(\" \" \"\" yaxception-unknown)
  at yaxception-ccc()
  at yaxception-bbb()
  at yaxception-aaa()"))
        (defun yaxception-aaa () (yaxception-bbb))
        (defun yaxception-bbb () (yaxception-ccc))
        (defun yaxception-ccc () (replace-regexp-in-string " " "" 'yaxception-unknown))
        (yaxception:$
          (yaxception:try
            (yaxception:$
              (yaxception:try
                (current-buffer))
              (yaxception:finally
                (yaxception-aaa))))
          (yaxception:catch 'error e
            (setq err e)))
        (should (equal
                 (yaxception:get-stack-trace-string err :limit 4)
                 expected)))
    (fmakunbound 'yaxception-aaa)
    (fmakunbound 'yaxception-bbb)
    (fmakunbound 'yaxception-ccc)))
