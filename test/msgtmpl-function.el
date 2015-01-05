(require 'yaxception)
(require 'el-expectations)

(expectations
  (desc "msgtmpl function")
  (expect "Function Message!"
    (let ((msg "")
          (f (lambda ()
               "Function Message!")))
      (yaxception:deferror 'yaxception-test-error nil f)
      (condition-case err
          (yaxception:throw 'yaxception-test-error)
        (error (setq msg (error-message-string err))))
      msg)))

