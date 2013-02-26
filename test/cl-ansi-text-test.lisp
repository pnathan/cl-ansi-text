;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test suite for cl-ansi-text

(defpackage :cl-ansi-text-test
  (:use :cl
	:cl-ansi-text
	:fiveam))

(in-package :cl-ansi-text-test)
(use-package :fiveam)

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
						 :unset :background)))
  )


(run! 'test-suite)
