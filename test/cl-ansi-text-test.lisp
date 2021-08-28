;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test suite for cl-ansi-text

(defpackage :cl-ansi-text-test
  (:use :common-lisp
        :cl-user
        :cl-ansi-text
        :fiveam))

(in-package :cl-ansi-text-test)

(def-suite :cl-ansi-text :description "test suite.")

(in-suite :cl-ansi-text)

(defun make-color-string-as-list (&rest args)
  (coerce (apply #'cl-ansi-text:make-color-string args) 'list))

(test basic-color-strings
  "Test the basic stuff"
  (let ((*color-mode* :3bit))
    (is (equal '(#\Esc #\[ #\3 #\1 #\m)
               (make-color-string-as-list :red :effect :unset :style :foreground)))
    (is (equal '(#\Esc #\[ #\4 #\1 #\m)
               (make-color-string-as-list :red :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\2 #\; #\1 #\m)
               (make-color-string-as-list :green :effect :bright :style :background)))))

(test enabled-connectivity
  "Test *enabled*'s capability"
  (let ((*color-mode* :3bit))
    (is (equal  '(#\Esc #\[ #\3 #\1 #\m)
                (let ((*enabled* t))
                  (make-color-string-as-list :red))))
    (is (equal  '()
                (let ((*enabled* nil))
                  (make-color-string-as-list :red))))
    (is (equal '(#\Esc #\[ #\3 #\1 #\m)
               (make-color-string-as-list :red :enabled t)))
    (is (equal '()
               (make-color-string-as-list :red :enabled nil)))
    (is (equal "hi"
               (let ((*enabled* nil))
                 (with-output-to-string (s)
                   (with-color (:red :stream s) (format s "hi"))))))
    (is (equal '(#\Esc #\[ #\3 #\1 #\m #\T #\e #\s #\t #\! #\Esc #\[ #\0 #\m)
               (concatenate
                'list
                (with-output-to-string (s)
                  (with-color (:red :stream s)
                    (format s "Test!"))))))))

(test rgb-suite
  "Test RGB colors"
  (let ((*color-mode* :3bit))
    (is (equal '(#\Esc #\[ #\3 #\1 #\m)
               (make-color-string-as-list #xFF0000 :effect :unset :style :foreground)))
    (is (equal '(#\Esc #\[ #\4 #\2 #\m)
               (make-color-string-as-list #x00FF00 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\4 #\m)
               (make-color-string-as-list #x0000FF :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\0 #\m)
               (make-color-string-as-list #x000000 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\7 #\m)
               (make-color-string-as-list #xFFFFFF :effect :unset :style :background))))
  (let ((*color-mode* :8bit))
    (is (equal '(#\Esc #\[ #\3 #\8 #\; #\5 #\; #\2 #\1 #\4 #\m)
               (make-color-string-as-list #xFFAA00 :effect :unset :style :foreground)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\2 #\1 #\4 #\m)
               (make-color-string-as-list #xFFAA00 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\1 #\6 #\m)
               (make-color-string-as-list #x000000 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\5 #\; #\2 #\3 #\1 #\m)
               (make-color-string-as-list #xFFFFFF :effect :unset :style :background))))
  (let ((*color-mode* :24bit))
    (is (equal '(#\Esc #\[ #\3 #\8 #\; #\2 #\; #\2 #\5 #\5 #\; #\1 #\7 #\0 #\; #\0 #\m)
               (make-color-string-as-list #xFFAA00 :effect :unset :style :foreground)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\2 #\; #\2 #\5 #\5 #\; #\1 #\7 #\0 #\; #\0 #\m)
               (make-color-string-as-list #xFFAA00 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\2 #\; #\0 #\; #\0 #\; #\0 #\m)
               (make-color-string-as-list #x000000 :effect :unset :style :background)))
    (is (equal '(#\Esc #\[ #\4 #\8 #\; #\2 #\; #\2 #\5 #\5 #\; #\2 #\5 #\5 #\; #\2 #\5 #\5 #\m)
               (make-color-string-as-list #xFFFFFF :effect :unset :style :background)))))

(test color-rgb
      ;; RGB plain
      (is (equal (coerce (cl-ansi-text:make-color-string "ff0000") 'list)
                 '(#\Esc #\[ #\3 #\1 #\m)))
      ;; Web Style
      (is (equal (coerce (cl-ansi-text:make-color-string "#ff0000") 'list)
                 '(#\Esc #\[ #\3 #\1 #\m)))

      ;; redux, but with 3 octets not 3 16-lets
      (is (equal (coerce (cl-ansi-text:make-color-string "f00") 'list)
                 '(#\Esc #\[ #\3 #\1 #\m)))
      (is (equal (coerce (cl-ansi-text:make-color-string "#f00") 'list)
                 '(#\Esc #\[ #\3 #\1 #\m))))

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
