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
   #:color-specifier
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
   #:*color-mode*
   #:color-string))
(in-package :cl-ansi-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(defvar *enabled* t
  "Turns on/off the colorization.")

(declaim (type (member :3bit :8bit :24bit) *color-mode*))
(defvar *color-mode* :3bit
  "Controls the way `make-color-string` emits the color code.

It should be one of the following keyword symbols: `:3bit`, `:8bit`, `:24bit`.
The specified color is converted to the nearest color in the color space.

Note that the actual appearance of the screen in the `:3bit` mode may be affected by
the terminal setting -- For example, many terminals do not use `FF0000` for the red.")

(defvar +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m))
  "A constant string that resets the color state of the terminal.")

(defvar +cl-colors-basic-colors+
  (vector
   cl-colors2:+black+
   cl-colors2:+red+
   cl-colors2:+green+
   cl-colors2:+yellow+
   cl-colors2:+blue+
   cl-colors2:+magenta+
   cl-colors2:+cyan+
   cl-colors2:+white+)
  "CL-COLORS2 basic colors")

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

(defun color-string-p (string)
  (every (lambda (c)
           (member c '(#\# #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                       #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))
         string))

(deftype color-specifier ()
  `(or unsigned-byte
       (cons (real 0 256)
             (cons (real 0 256)
                   (cons (real 0 256)
                         null)))
       cl-colors2:rgb
       cl-colors2:hsv
       term-colors
       color-string))

(deftype color-string ()
  `(and (or (string 3)
            (string 4)
            (string 6)
            (string 7))
        (satisfies color-string-p)))

(deftype term-colors ()
  `(member :black :red :green :yellow :blue :magenta :cyan :white))

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
				  ((:enabled *enabled*) *enabled*))
  "Takes an object of `color-specifier` and returns a string sufficient to change to the given color.

Colorization is controlled by *enabled* unless manually specified otherwise by `:enabled` keyword."
  (declare (type color-specifier color))
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
  "Writes out the ANSI escape code string
denoting `effect`, `style`, and a switch to `color`, then executes `body`,
then writes out the string that resets the decoration."
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
  (rgb-color-code (apply #'cl-colors2:rgb (mapcar (lambda (x) (/ x 256)) color))
                  style))

(defmethod rgb-color-code ((color integer) &optional (style :foreground))
  "Takes RGB integer ala Web integers"
  (rgb-color-code (cl-colors2:rgb
                   ;; classic bitmask
                   (/ (ash  (logand color #xff0000) -16) 256)
                   (/ (ash  (logand color #x00ff00) -8) 256)
                   (/ (logand color #x0000ff) 256))
                  style))

(defmethod rgb-color-code ((color string) &optional (style :foreground))
  "Takes RGB integer ala Web integers"
  (rgb-color-code (cl-colors2:parse-hex-rgb color)
                  style))

(defmethod rgb-color-code ((color cl-colors2:rgb) &optional (style :foreground))
  (code-from-rgb color style))

(defmethod rgb-color-code ((color cl-colors2:hsv) &optional (style :foreground))
  (code-from-rgb (cl-colors2:hsv-to-rgb color) style))

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
           (ceiling (* 255 (cl-colors2:rgb-red color)))
           (ceiling (* 255 (cl-colors2:rgb-green color)))
           (ceiling (* 255 (cl-colors2:rgb-blue color)))))))

(defun rgb-to-ansi-3bit (color)
  "find the closest color from +cl-colors-basic-colors+"
  (labels ((square (x)
             (* x x))
           (distance (color2)
             (+ (square (- (cl-colors2:rgb-red   color) (cl-colors2:rgb-red   color2)))
                (square (- (cl-colors2:rgb-green color) (cl-colors2:rgb-green color2)))
                (square (- (cl-colors2:rgb-blue  color) (cl-colors2:rgb-blue  color2))))))
    (position (reduce (lambda (a b)
                        (if (< (distance a) (distance b))
                            a b))
                      +cl-colors-basic-colors+)
              +cl-colors-basic-colors+)))

(defun rgb-to-ansi-8bit (color)
  "http://www.frexx.de/xterm-256-notes/"
  (+ 16
     (* 36 (min 5 (floor (* 6 (cl-colors2:rgb-red   color)))))
     (* 6  (min 5 (floor (* 6 (cl-colors2:rgb-green color)))))
     (* 1  (min 5 (floor (* 6 (cl-colors2:rgb-blue  color)))))))
