(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "throw")
  (expect "File error"
    (let ((msg ""))
      (condition-case err
          (yaxception:throw 'file-error)
        (error (setq msg (error-message-string err))))
      msg))
  (desc "throw re")
  (expect t
    (let ((msg ""))
      (condition-case err
          (yaxception:$
            (yaxception:try
              (delete-file "not found")
              (setq msg "ok"))
            (yaxception:catch 'error e
              (yaxception:throw e)))
        (error (setq msg (error-message-string err))))
      (and (string-match "\\`Removing old name: " msg) t)))
  (desc "throw error")
  (expect "File error"
    (let ((msg ""))
      (condition-case err
          (yaxception:$
            (yaxception:try
              (delete-file "not found")
              (setq msg "ok"))
            (yaxception:catch 'error e
              (yaxception:throw 'file-error)))
        (error (setq msg (error-message-string err))))
      msg))
  (desc "throw custom error")
  (expect "This is YAX 'hoge' test."
    (yaxception:deferror 'yaxception-test-error 'file-error "This is YAX '%s' test." 'yax-a)
    (let ((msg ""))
      (condition-case err
          (yaxception:$
            (yaxception:try
              (delete-file "not found")
              (setq msg "ok"))
            (yaxception:catch 'error e
              (yaxception:throw 'yaxception-test-error :yax-a "hoge" :yax-b "fuga")))
        (error (setq msg (error-message-string err))))
      msg))
  )

