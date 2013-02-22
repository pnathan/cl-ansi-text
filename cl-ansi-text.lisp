;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Paul Nathan 2013
;;;; cl-ansi-text.lisp
;;;;
;;;; A library to produce ANSI escape sequences. Particularly,
;;;; produces colorized text on terminals

(defpackage :cl-ansi-text
  (:use :common-lisp)
  (:export
   #:with-color
   #:make-color-string
   #:+reset-color-string+
   #:+term-colors+
   #:+term-effects+
   #:*enabled
   ))
(in-package :cl-ansi-text)

;;; !!! NOTE TO CCL USERS !!!
;;;
;;; This seems to be *required* to make this compile in CCL. The
;;; reason is that CCL expects to be able to inline on compile, but
;;; structs don't set up that infrastructure by default.
;;;
;;; At least from the thread "Compiler problem, MCL 3.9" by Arthur
;;; Cater around '96.
#+ccl(common-lisp:eval-when (:compile-toplevel)
       (defmethod make-load-form ((obj cl-colors:rgb )  &optional env)
	 (make-load-form-saving-slots obj)))

(defparameter *enabled* t
  "Turns on/off the colorization of functions" )

(defparameter +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m))
  "This string will reset ANSI colors")

(defvar +cl-bright-colors+
	(vector
         cl-colors:+black+
         cl-colors:+red+
         cl-colors:+green+
         cl-colors:+yellow+
         cl-colors:+blue+
         cl-colors:+magenta+
         cl-colors:+cyan+
         cl-colors:+white+)
  "CL-COLORS bright colors")

(defvar +cl-colors+
  (vector
   cl-colors:+darkgrey+
   cl-colors:+darkred+
   cl-colors:+darkgreen+
   cl-colors:+wheat+ 	;kind of darkyellow
   cl-colors:+darkblue+
   cl-colors:+darkmagenta+
   cl-colors:+darkcyan+
   cl-colors:+grey+)
  "CL-COLORS colors")

(defparameter +term-colors+
  '((:black . 0)
    (:red . 1)
    (:green . 2)
    (:yellow . 3)
    (:blue . 4)
    (:magenta . 5)
    (:cyan . 6)
    (:white . 7)
    (:default . 9))
  "Basic colors")

(defparameter +term-effects+
  '((:normal . nil)                     ;the usual
    (:reset . 0)
    (:bright . 1)
    (:italic . 3)
    (:underline . 4)
    (:blink . 5)
    (:inverse . 7)
    (:hide . 8))
  "ANSI terminal effects")

(defparameter +color-map+)

(defun explode (input)
  "Assumes input is a vector, returns it as a list"
  (concatenate 'list input))

(defun extend (&rest items)
  "Append each item, ensuring that it is surrounded by a list if it is
  not already"
  (apply #'concatenate 'list
	 (mapcar
	  #'alexandria:ensure-list
	  items) ))

(defun keyword-to-cl-colors (color-code &optional bright)
  (aref (if bright
            +bright-colors+
            +colors+) color-code))

(defun eq-colors (a b)
  "Equality for colors"
  ;; eql, equal doesn't quite work for compiled cl-colors on CCL
  (and
   (= (cl-colors:rgb-red a)
      (cl-colors:rgb-red b))
   (= (cl-colors:rgb-green a)
      (cl-colors:rgb-green b))
   (= (cl-colors:rgb-blue a)
      (cl-colors:rgb-blue b))))

(defun cl-colors-to-ansi (color)
  (alexandria:switch (color :test #'eq-colors)
	  ;;bright
	  (cl-colors:+black+ '(30 1))
	  (cl-colors:+red+ '(31 1))
	  (cl-colors:+green+ '(32 1))
	  (cl-colors:+yellow+ '(33 1))
	  (cl-colors:+blue+ '(34 1))
	  (cl-colors:+magenta+ '(35 1))
	  (cl-colors:+cyan+ '(36 1))
	  (cl-colors:+white+ '(37 1))

	  (cl-colors:+darkgrey+ '(30))
	  (cl-colors:+darkred+ '(31))
	  (cl-colors:+darkgreen+ '(32))
	  (cl-colors:+wheat+ '(33))
	  (cl-colors:+darkblue+ '(34))
	  (cl-colors:+darkmagenta+ '(35))
	  (cl-colors:+darkcyan+ '(36))
	  (cl-colors:+grey+ '(37))))

(defun find-color-set (color)
  "Find the list denoting the color"
  (etypecase color
    (cl-colors:rgb (cl-colors-to-ansi color))
    (list color)))

(defun build-control-string (color effect)
  "Build the basic control character list"
  (let ((codes (mapcar #'(lambda (n)
			  (explode (write-to-string n)))
		(find-color-set color))))
    (if (= (length codes) 1)
	(car codes)
	(extend (first codes) #\; (second codes)))))

(defun make-color-string (color-spec &optional (effect nil))
  "Takes either a cl-color or a list denoting the ANSI colors and
returns a string sufficient to change to the given color"
  (concatenate 'string
	       (append
		`( ,(code-char 27) #\[)
		(build-control-string color effect)
		'(#\m))))

(defmacro with-color ((color &key (stream t))
		      &body body)
  "Writes out the string denoting a switch to `color`, executes body,
then writes out the string denoting a `reset`."
  `(progn
    (format ,stream "~a" (make-color-string ,color))
    (unwind-protect
	 (progn
	   ,@body)
      (format ,stream "~a" +reset-color-string+))))

;; Kind of hinky, unsure if good API, ergo, not exporting it
(defmacro format-with-color (color dest control-string &rest args)
  (let ((sym (gensym)))
    `(let ((,sym (format nil "~a~a~a"
			 (make-color-string ,color)
			 ,control-string
			 +reset-color-string+)))
       (format ,dest ,sym ,args))))



;;; http://www.frexx.de/xterm-256-notes/
(defun rgb-to-ansi (red green blue)
  (let ((ansi-domain (mapcar #'(lambda (x)
                                 (floor (* 6 (/ x 256.0))))
                             (list red green blue))))
    (+ 16
     (* 36 (first ansi-domain))
     (* 6 (second ansi-domain))
     (third ansi-domain))))


(defun code-from-rgb (style red green blue)
  (format nil "~d;5;~d" (if (eql style :foreground) 38 48)
          (rgb-to-ansi red green blue)))

(defgeneric ansi-color-code (style color-argument)
  (:documentation
   "Returns the ANSI color code indicated by color-argument, modified by style."))

(defmethod ansi-color-code (style (color symbol))
  (let ((col (cdr  (assoc color +term-colors+)))
        (ground (if (eql style :foreground) 30 40)))
    (if col (+ col ground) 0)))


(defmethod ansi-color-code (style (color integer))
  (code-from-rgb style
                 ;; classic bitmask
                 (ash  (logand color #xff0000) -16)
                 (ash  (logand color #x00ff00) -8)
                 (logand color #x0000ff)))

(defmethod ansi-color-code (style (color list))
  (unless (consp list)
    (error "~a must be a three-integer list"))

  (unless (and (integerp (first color))
               (integerp (second color))
               (integerp (second color)))
    (error "~a must have three integers"))

  (code-from-rgb style
                 (first color)
                 (second color)
                 (third color)))

(deftype )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ~/.../ functions.
;;; Suggested by H4ns
;;; http://www.lispworks.com/documentation/HyperSpec/Body/22_ced.htm
;;; Not ready for general use.

(defun color-red (stream input colon-p at-p &rest params)
  (format stream "~a" (make-color-string cl-colors:+red+)))

(defun color-green (stream input colon-p at-p &rest params)
  (format stream "~a" (make-color-string cl-colors:+green+)))

(defun color-blue (stream input colon-p at-p &rest params)
  (format stream "~a" (make-color-string cl-colors:+blue+)))

(defun color-reset (stream input colon-p at-p &rest params)
  (format stream "~a" +reset-color-string+))
