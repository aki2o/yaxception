(require 'yaxception)
(require 'el-expectations)


(expectations
  (desc "return-value when not error not has catch")
  (expect "fuga"
    (yaxception:$
      (yaxception:try
        "hoge"
        "fuga")))
  (desc "return-value when not error has catch")
  (expect "fuga"
    (yaxception:$
      (yaxception:try
        "hoge"
        "fuga")
      (yaxception:catch 'error e
        "foo")))
  (desc "return-value when not error has finally")
  (expect "fuga"
    (yaxception:$
      (yaxception:try
        "hoge"
        "fuga")
      (yaxception:finally
        "foo")))
  (desc "return-value when error")
  (expect "foo"
    (yaxception:$
      (yaxception:try
        "hoge"
        (error "It's test error")
        "fuga")
      (yaxception:catch 'error e
        "foo")))
  (desc "return-value when error has finally")
  (expect "bar"
    (yaxception:$
      (yaxception:try
        "hoge"
        (error "It's test error")
        "fuga")
      (yaxception:catch 'error e
        "foo"
        "bar")
      (yaxception:finally
        "baz")))
  )

