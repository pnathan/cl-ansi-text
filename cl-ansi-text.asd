(asdf:defsystem #:cl-ansi-text
  :depends-on ( #:cl-colors #:alexandria)
  :components ((:file "cl-ansi-text"))
  :name "cl-ansi-text"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "ANSI control string characters, focused on color"
  :long-description "ANSI control string management, specializing in
  colors. Sometimes it is nice to have text output in colors")
