;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Paul Nathan 2013
;;;; cl-ansi-text.lisp
;;;;
;;;; Portions of this code were written by taksatou under the
;;;; cl-rainbow name.
;;;;
;;;; A library to produce ANSI escape sequences. Particularly,
;;;; produces colorized text on terminals

(defpackage :cl-ansi-text
  (:use :common-lisp)
  (:export
   #:with-color
   #:make-color-string
   #:+reset-color-string+
   #:*enabled*
   #:black
   #:red
   #:green
   #:yellow
   #:blue
   #:magenta
   #:cyan
   #:white))
(in-package :cl-ansi-text)

;;; !!! NOTE TO CCL USERS !!!
;;;
;;; This seems to be *required* to make this compile in CCL. The
;;; reason is that CCL expects to be able to inline on compile, but
;;; structs don't set up that infrastructure by default.
;;;
;;; At least from the thread "Compiler problem, MCL 3.9" by Arthur
;;; Cater around '96.
#+ccl(common-lisp:eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod make-load-form ((obj cl-colors:rgb )  &optional env)
	 (make-load-form-saving-slots obj)))

(defparameter *enabled* t
  "Turns on/off the colorization of functions")

(defparameter +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m))
  "This string will reset ANSI colors")

(defvar +cl-colors+
  (vector
   cl-colors:+black+
   cl-colors:+red+
   cl-colors:+green+
   cl-colors:+yellow+
   cl-colors:+blue+
   cl-colors:+magenta+
   cl-colors:+cyan+
   cl-colors:+white+)
  "CL-COLORS colors")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +term-colors+
    (vector
     :black
     :red
     :green
     :yellow
     :blue
     :magenta
     :cyan
     :white)
    "Basic colors"))

(defparameter +text-style+
  '((:foreground . 30)
    (:background . 40))
  "One or the other. Not an ANSI effect")

(defparameter +term-effects+
  '((:unset . t)
    (:reset . 0)
    (:bright . 1)
    (:italic . 3)
    (:underline . 4)
    (:blink . 5)
    (:inverse . 7)
    (:hide . 8)
    (:normal . 22)
    (:framed . 51)
    (:encircled . 52)
    (:overlined . 53)
    (:not-framed-or-circled . 54)
    (:not-overlined . 55))
  "ANSI terminal effects")

(defun eq-colors (a b)
  "Equality for cl-colors"

  ;; CL-COLORS LIB!

  ;; eql, equal doesn't quite work for compiled cl-colors on CCL
  (and
   (= (cl-colors:rgb-red a)
      (cl-colors:rgb-red b))
   (= (cl-colors:rgb-green a)
      (cl-colors:rgb-green b))
   (= (cl-colors:rgb-blue a)
      (cl-colors:rgb-blue b))))

(defun cl-colors-to-ansi (color)
  (position color +cl-colors+ :test #'eq-colors))

(defun term-colors-to-ansi (color)
  (position color +term-colors+))



;; Find-X-code is the top-level interface for code-finding

(defun find-color-code (color)
  "Find the list denoting the color"
  (typecase color
    ;; Did we get a cl-color that we know about?
    (cl-colors:rgb (cl-colors-to-ansi color))
    (symbol (term-colors-to-ansi color))))

(defun find-effect-code (effect)
  "Returns the number for the text effect OR
t if no effect should be used OR
nil if the effect is unknown.

effect should be a member of +term-effects+"
  (cdr (assoc effect +term-effects+)))

(defun find-style-code (style)
  (cdr (assoc style +text-style+)))

(defun rgb-code-p (color)
  (typecase color
    (list t)
    (integer t)))

(defun generate-control-string (code)
  "General ANSI code"
  (format nil "~c[~a" (code-char #o33) code))

(defun generate-color-string (code)
  ;; m is the action character for color
  (format nil "~am" (generate-control-string code)))

(defun build-control-string (color
			     &optional
			     (effect :unset)
			     (style :foreground))
  "Color (cl-color or term-color)
Effect
Style"
  (let ((effect-code (find-effect-code effect))
	(color-code (find-color-code color))
	(style-code (find-style-code style)))

    ;; Nil here indicates an error
    (assert  effect-code)
    (assert  style-code)

    ;; Returns a list for inspection; next layer turns it back into a
    ;; string.
    (concatenate
     'list
     ;; We split between RGB and 32-color here; this preserves the
     ;; interface without cluttering the 32-color code up.
     ;;
     (let ((codes nil))
       (unless (eq effect-code t)
           (setf codes (cons effect-code codes)))
       (if (rgb-code-p color)
           (setf codes (cons (rgb-color-code color style) codes))
           (setf codes (cons (+ style-code color-code) codes)))

       (generate-color-string (format nil "~{~A~^;~}" codes))))))

;; Public callables.

(defun make-color-string (color &key
				  (effect :unset)
				  (style :foreground)
				  ((enabled *enabled*) *enabled*))
  "Takes either a cl-color or a list denoting the ANSI colors and
returns a string sufficient to change to the given color.

Will be dynamically controlled by *enabled* unless manually specified
otherwise"
  (when *enabled*
    (concatenate 'string
		 (build-control-string color effect style))))

(defmacro with-color ((color  &key
			      (stream t)
			      (effect :unset)
			      (style :foreground))
		      &body body)
  "Writes out the string denoting a switch to `color`, executes body,
then writes out the string denoting a `reset`.

*enabled* dynamically controls expansion.."
  `(progn
     (when *enabled*
      (format ,stream "~a" (make-color-string ,color
					      :effect ,effect
					      :style ,style)))
     (unwind-protect
	 (progn
	   ,@body)
      (when *enabled*
	(format ,stream "~a" +reset-color-string+)))))

(defmacro gen-color-functions (color-names-vector)
  `(progn
     ,@(map 'list
            (lambda (color)
              `(defun ,(intern (symbol-name color)) (string &key
                                                              (effect :unset)
                                                              (style :foreground))
                 ,(concatenate
                   'string
                   "Returns a string with the `" (string-downcase color)
                   "'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function." )
                 (concatenate
                  'string
                  (when *enabled*
                    (format nil "~a" (make-color-string ,color
                                                        :effect effect
                                                        :style style)))
                  string
                  (when *enabled*
                    (format nil "~a" +reset-color-string+)))))
            color-names-vector)))

(gen-color-functions #.(coerce +term-colors+ 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  RGB color codes for some enhanced terminals

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
  (format nil "~d;5;~d"
	  (if (eql style :foreground) 38 48)
          (rgb-to-ansi red green blue)))


(defgeneric rgb-color-code (color &optional style)
  (:documentation
   "Returns the 256-color code suitable for rendering on the Linux
extensions to xterm"))

(defmethod rgb-color-code ((color list) &optional (style :foreground))
  (unless (consp color)
    (error "~a must be a three-integer list" color))

  (unless (and (integerp (first color))
               (integerp (second color))
               (integerp (second color)))
    (error "~a must have three integers" color))

  (code-from-rgb style
                 (first color)
                 (second color)
                 (third color)))

(defmethod rgb-color-code ((color integer) &optional (style :foreground))
  ;; Takes RGB integer ala Web integers
  (code-from-rgb style
                 ;; classic bitmask
                 (ash  (logand color #xff0000) -16)
                 (ash  (logand color #x00ff00) -8)
                 (logand color #x0000ff)))
