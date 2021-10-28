(defsystem "unio"
  :version "0.2.0"
  :author "biofermin2"
  :license "MIT"
  :depends-on ()
  :components ((:file "unio"))))
  :description "Keyword searcher for S-expression."
  :in-order-to ((test-op (test-op "unio/tests"))))

(defsystem "unio/tests"
  :author ""
  :license ""
  :depends-on ("unio"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "unio"))))
  :description "Test system for unio"
  :perform (test-op (op c) (symbol-call :rove :run c)))
