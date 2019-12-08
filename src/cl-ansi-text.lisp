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
   #:white
   #:*color-mode*))
(in-package :cl-ansi-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(defvar *enabled* t
  "Turns on/off the colorization of functions")

(declaim (type (member :3bit :8bit :24bit) *color-mode*))
(defvar *color-mode* :24bit
  "Changes the mode used to encode the color")

(defvar +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m))
  "This string will reset ANSI colors")

(defvar +cl-colors-basic-colors+
  (vector
   cl-colors:+black+
   cl-colors:+red+
   cl-colors:+green+
   cl-colors:+yellow+
   cl-colors:+blue+
   cl-colors:+magenta+
   cl-colors:+cyan+
   cl-colors:+white+)
  "CL-COLORS basic colors")

(defvar +term-colors+
  (vector
   :black
   :red
   :green
   :yellow
   :blue
   :magenta
   :cyan
   :white)
  "Basic colors")

(defvar +text-style+
  '((:foreground . 30)
    (:background . 40))
  "One or the other. Not an ANSI effect")

(defvar +term-effects+
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-effect-code (effect)
  "Returns the number for the text effect OR
t if no effect should be used OR
nil if the effect is unknown.

effect should be a member of +term-effects+"
  (cdr (assoc effect +term-effects+)))

(defun find-style-code (style)
  (cdr (assoc style +text-style+)))

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
    (let ((effect-code (find-effect-code effect)))
      ;; Nil here indicates an error
      (assert effect-code)
      (let ((codes nil))
        (unless (eq effect-code t)
          (push effect-code codes))

        ;; on 3bit, a list containing an integer in 30 - 37, 40 - 47.
        ;; on 8bit, a list containing 5 and an integer.               38;5;n
        ;; on 24bit, a list containing 2 and 3 more integers (r,g,b). 38;2;r;g;b
        (setf codes (append (rgb-color-code color style) codes))
        (format nil "~c[~{~A~^;~}m" (code-char #o33) codes)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color handling

(defgeneric rgb-color-code (color &optional style)
  (:documentation
   "Returns a list of codes suitable for the current color mode."))

(defmethod rgb-color-code ((color list) &optional (style :foreground))
  (assert (and (= 3 (length color))
               (every #'realp color)
               (every (lambda (x) (<= 0 x 256)) color))
          nil "~a must be a list of numbers in [0,256]" color)
  (rgb-color-code (apply #'cl-colors:rgb (mapcar (lambda (x) (/ x 256)) color))
                  style))

(defmethod rgb-color-code ((color integer) &optional (style :foreground))
  "Takes RGB integer ala Web integers"
  (rgb-color-code (cl-colors:rgb 
                   ;; classic bitmask
                   (/ (ash  (logand color #xff0000) -16) 256)
                   (/ (ash  (logand color #x00ff00) -8) 256)
                   (/ (logand color #x0000ff) 256))
                  style))

(defmethod rgb-color-code ((color string) &optional (style :foreground))
  "Takes RGB integer ala Web integers"
  (rgb-color-code (cl-colors:hex-to-rgb color)
                  style))

(defmethod rgb-color-code ((color cl-colors:rgb) &optional (style :foreground))
  (code-from-rgb color style))

(defmethod rgb-color-code ((color cl-colors:hsv) &optional (style :foreground))
  (code-from-rgb (cl-colors:hsv-to-rgb color) style))

(defmethod rgb-color-code ((color symbol) &optional (style :foreground))
  (code-from-rgb (aref +cl-colors-basic-colors+ (position color +term-colors+)) style))

(defun code-from-rgb (color style)
  (ecase *color-mode*
    (:3bit
     (list (+ (find-style-code style) ; 30 or 40
              ;; 0-7
              (rgb-to-ansi-3bit color))))
    (:8bit
     (list (+ (find-style-code style) 8) ; 38 or 48
           5
           (rgb-to-ansi-8bit color)))
    (:24bit
     (list (+ (find-style-code style) 8)
           2
           (ceiling (* 255 (cl-colors:rgb-red color)))
           (ceiling (* 255 (cl-colors:rgb-green color)))
           (ceiling (* 255 (cl-colors:rgb-blue color)))))))

(defun rgb-to-ansi-3bit (color)
  "find the closest color from +cl-colors-basic-colors+"
  (labels ((square (x)
             (* x x))
           (distance (color2)
             (+ (square (- (cl-colors:rgb-red   color) (cl-colors:rgb-red   color2)))
                (square (- (cl-colors:rgb-green color) (cl-colors:rgb-green color2)))
                (square (- (cl-colors:rgb-blue  color) (cl-colors:rgb-blue  color2))))))
    (position (reduce (lambda (a b)
                        (if (< (distance a) (distance b))
                            a b))
                      +cl-colors-basic-colors+)
              +cl-colors-basic-colors+)))

(defun rgb-to-ansi-8bit (color)
  "http://www.frexx.de/xterm-256-notes/"
  (+ 16
     (* 36 (min 5 (floor (* 6 (cl-colors:rgb-red   color)))))
     (* 6  (min 5 (floor (* 6 (cl-colors:rgb-green color)))))
     (* 1  (min 5 (floor (* 6 (cl-colors:rgb-blue  color)))))))
