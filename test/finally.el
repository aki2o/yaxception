(require 'yaxception)

(ert-deftest finally/without-error ()
  "finally without error"
  (let ((msg ""))
    (yaxception:$
      (yaxception:try
        (setq msg (concat msg "OK")))
      (yaxception:catch 'error e
        (setq msg (concat msg "Caught")))
      (yaxception:finally
        (setq msg (concat msg "Finished"))))
    (should (equal msg "OKFinished"))))

(ert-deftest finally/with-error ()
  "finally with error"
  (let ((msg ""))
    (yaxception:$
      (yaxception:try
        (delete-file 'hoge)
        (setq msg (concat msg "OK")))
      (yaxception:catch 'error e
        (setq msg (concat msg "Caught")))
      (yaxception:finally
        (setq msg (concat msg "Finished"))))
    (should (equal msg "CaughtFinished"))))

(ert-deftest finally/nest ()
  "finally on nest"
  (let ((msg ""))
    (yaxception:$
      (yaxception:try
        (yaxception:$
          (yaxception:try
            (delete-file 'hoge)
            (setq msg (concat msg "OK")))
          (yaxception:catch 'error e
            (setq msg (concat msg "Throwing"))
            (yaxception:throw e))
          (yaxception:finally
            (setq msg (concat msg "Finished")))))
      (yaxception:catch 'error e
        (setq msg (concat msg "Caught")))
      (yaxception:finally
        (setq msg (concat msg "Complete"))))
    (should (equal msg "ThrowingFinishedCaughtComplete"))))
