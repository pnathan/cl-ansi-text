#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#+sbcl(require "sb-posix")

(defparameter *pwd*
  (concatenate 'string
               (progn #+sbcl(sb-posix:getcwd)
                      #+ccl(ccl::current-directory-name))
               "/"))

(push *pwd* asdf:*central-registry*)


(ql:quickload '(:cl-colors
                :alexandria
                :fiveam

                :cl-ansi-text
                :cl-ansi-text-test))

(let ((result-status (cl-ansi-text-test::ci-run)))
  (let ((posix-status
          (if result-status 0 1)))
    #+sbcl(sb-posix:exit posix-status)
    #+ccl (quit posix-status)))
