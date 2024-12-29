(defsystem "cloveleaf"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cloveleaf/tests"))))

(defsystem "cloveleaf/tests"
  :author ""
  :license ""
  :depends-on ("cloveleaf"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cloveleaf"
  :perform (test-op (op c) (symbol-call :rove :run c)))
