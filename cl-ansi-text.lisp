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
   #:+resetp-color-string+
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


(defmacro switch (test-function thing &rest forms)
   " When test-function has to get repeatedly applied to thing to

 determine if the result should be executed, SWITCH may prove beneficial
 (switch
     string=
   input
   (value
    thing-to-do-if-value string= input))"

  (let ((cond-form-sym
         (append
          (list 'COND)
          ;; Form up the forms.
          (mapcar
          #'(lambda (form)
              `,(list
                 (list test-function thing (car form))
                 (progn (cadr form))))
          forms)
          ;; have a useful error
          `((t
             (error
              "Unable to match the condition ~a using ~a"
              ,thing '#',test-function ))))))
    cond-form-sym))

(defparameter +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m)))

(defun ansi-to-cl-colors (color-code &optional bright)
  (if (or bright
	  (eql bright 1))
      (case color-code
	(0 cl-colors:+black+)
	(1 cl-colors:+red+)
	(2 cl-colors:+green+)
	(3 cl-colors:+yellow+)
	(4 cl-colors:+blue+)
	(5 cl-colors:+magenta+)
	(6 cl-colors:+cyan+)
	(7 cl-colors:+white+))
      (case color-code
	(0 cl-colors:+darkgrey+)
	(1 cl-colors:+darkred+)
	(2 cl-colors:+darkgreen+)
	(3 cl-colors:+wheat+) 	;kind of darkyellow
	(4 cl-colors:+darkblue+)
	(5 cl-colors:+darkmagenta+)
	(6 cl-colors:+darkcyan+)
	(7 cl-colors:+grey+))))

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
  (switch eq-colors color
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

(defun build-control-string (color)
  "Build the basic control character list"
  (let ((codes (mapcar #'(lambda (n)
			  (explode (write-to-string n)))
		(find-color-set color))))
    (if (= (length codes) 1)
	(car codes)
	(extend (first codes) #\; (second codes)))))

(defun make-color-string (color)
  "Takes either a cl-color or a list denoting the ANSI colors and
returns a string sufficient to change to the given color"
  (concatenate 'string
	       (append
		`( ,(code-char 27) #\[)
		(build-control-string color)
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
