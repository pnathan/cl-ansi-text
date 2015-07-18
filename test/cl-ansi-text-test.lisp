;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test suite for cl-ansi-text

(defpackage :cl-ansi-text-test
  (:use :common-lisp
        :cl-user
        :cl-ansi-text
        :fiveam))

(in-package :cl-ansi-text-test)
(use-package :fiveam)
(use-package :cl-ansi-text)

(def-suite test-suite
    :description "test suite.")

(in-suite test-suite)


(test basic-color-strings
  "Test the basic stuff"
  (is (equal '(#\Esc #\[ #\3 #\1 #\m)
             (cl-ansi-text::build-control-string :red :unset :foreground)))
  (is (equal '(#\Esc #\[ #\4 #\1 #\m)
             (cl-ansi-text::build-control-string :red :unset :background)))
  (is (equal '(#\Esc #\[ #\4 #\2 #\; #\1 #\m)
             (cl-ansi-text::build-control-string :green :bright :background))))

(test enabled-connectivity
  "Test *enabled*'s capability"
 (is (equal  '(#\Esc #\[ #\3 #\1 #\m)
             (let ((*enabled* t))
               (concatenate
                'list
                (cl-ansi-text:make-color-string :red)))))
 (is (equal  '()
            (let ((*enabled* nil))
              (concatenate
               'list
               (cl-ansi-text:make-color-string :red)))))
 (is (equal "hi"
            (let ((*enabled* nil))
              (with-output-to-string (s)
                (with-color (:red :stream s) (format s "hi"))))))
 (is (equal '(#\Esc #\[ #\3 #\1 #\m #\T #\e #\s #\t #\! #\Esc #\[ #\0 #\m)
            (concatenate
             'list
             (with-output-to-string (s)
               (with-color (:red :stream s)
                 (format s "Test!")))))))

(test rgb-suite
  "Test RGB colors"
  (is (equal '(#\Esc #\[ #\3 #\8 #\; #\5 #\; #\2 #\1 #\4 #\m)
             (cl-ansi-text::build-control-string #xFFAA00
                                                 :unset :foreground)))
  (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\2 #\1 #\4 #\m)
             (cl-ansi-text::build-control-string #xFFAA00
                                                 :unset :background)))
  (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\1 #\6 #\m)
             (cl-ansi-text::build-control-string #x000000
                                                 :unset :background)))
  (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\2 #\3 #\1 #\m)
             (cl-ansi-text::build-control-string #xFFFFFF
                                                 :unset :background))))

(test color-named-functions
  (let ((str "Test string."))
    (is (equal (black str)
               (with-output-to-string (s)
                 (with-color (:black :stream s)
                   (format s str)))))
    (is (equal (red str)
               (with-output-to-string (s)
                 (with-color (:red :stream s)
                   (format s str)))))

    (is (equal (green str)
               (with-output-to-string (s)
                 (with-color (:green :stream s)
                   (format s str)))))

    (is (equal (yellow str)
               (with-output-to-string (s)
                 (with-color (:yellow :stream s)
                   (format s str)))))
    (is (equal (blue str)
               (with-output-to-string (s)
                 (with-color (:blue :stream s)
                   (format s str)))))
    (is (equal (magenta str)
               (with-output-to-string (s)
                 (with-color (:magenta :stream s)
                   (format s str)))))
    (is (equal (cyan str)
               (with-output-to-string (s)
                 (with-color (:cyan :stream s)
                   (format s str)))))
    (is (equal (white str)
               (with-output-to-string (s)
                 (with-color (:white :stream s)
                   (format s str)))))))

(test color-named-functions-*enabled*
  (let ((str "Other test string.")
        (*enabled* nil))
    (is
     (equal str
            (white (cyan (magenta (blue (yellow (green (red (black str))))))))))))

(defun run-tests ()
  (let  ((results (run 'test-suite)))
    (explain! results)
    (if (position-if #'(lambda (e)
                         (eq (type-of e)
                             'IT.BESE.FIVEAM::TEST-FAILURE
                             ))
                     results)
        nil
        t)))

(defun ci-run ()
  (run-tests))
