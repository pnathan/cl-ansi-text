(asdf:defsystem #:cl-ansi-text-test
  :depends-on ( #:cl-colors #:alexandria #:cl-ansi-text #:fiveam)
  :components ((:module "test"
                :components
                ((:file "cl-ansi-text-test"))))
  :name "cl-ansi-text-test"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "Test system for cl-ansi-text")
