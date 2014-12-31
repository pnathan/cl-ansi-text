#!/usr/local/bin/sbcl

(require "sb-posix")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defparameter *pwd* (concatenate 'string (sb-posix:getcwd) "/"))

(push *pwd* asdf:*central-registry*)


(ql:quickload '(:cl-colors
                :alexandria
                :fiveam

                :cl-ansi-text
                :cl-ansi-text-test))

(let ((result-status (cl-ansi-text-test::ci-run)))
  (sb-posix:exit (if result-status 0 1)))
