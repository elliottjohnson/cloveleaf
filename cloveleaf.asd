(defsystem "cloveleaf"
  :version "0.0.1"
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license "MIT"
  :depends-on ("com.inuoe.jzon")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A library to present SMuFL fonts written in Common Lisp."
  :in-order-to ((test-op (test-op "cloveleaf/tests"))))

(defsystem "cloveleaf/ttf"
  :version "0.0.1"
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license "MIT"
  :depends-on ("cloveleaf" "zpb-ttf")
  :components ((:module "backends"
			:components ((:file "ttf"))))
  :description "A TTF backend to cloveleaf.")

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

(defsystem "cloveleaf/smufl"
  :author "Elliott Johnson"
  :license "MIT"
  :depends-on ("com.inuoe.jzon" "cloveleaf")
  :components ((:module "build"
		:components ((:file "generate"))))
  :description
  "A library to generate our distributed files based upon SMuFL releases.")
