;; requires cl-colors
;; Also requires my switch macro

(defun ansi-to-cl-colors (color-code &optional bright)
  (if (or bright
	  (eql bright 1))
      (case color-code
	(0 cl-colors:+black+)
	(1 cl-colors:+red+)
	(2 cl-colors:+green+)
	(3 cl-colors:+yellow+)		;kind of darkyellow
	(4 cl-colors:+blue+)
	(5 cl-colors:+magenta+)
	(6 cl-colors:+cyan+)
	(7 cl-colors:+white+))
      (case color-code
	(0 cl-colors:+darkgrey+)
	(1 cl-colors:+darkred+)
	(2 cl-colors:+darkgreen+)
	(3 cl-colors:+wheat+)
	(4 cl-colors:+darkblue+)
	(5 cl-colors:+darkmagenta+)
	(6 cl-colors:+darkcyan+)
	(7 cl-colors:+grey+))))

(defun cl-colors-to-ansi (color)
  (switch eql color
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
  (typecase color
      (CL-COLORS:RGB (cl-colors-to-ansi color))
      (list  color)))



(defun build-control-string (color)
  (let ((codes (mapcar #'(lambda (n)
			  (explode (write-to-string n)))
		(find-color-set color))))
    ;(format t "~a" codes)
  (if (= (length codes) 1)
      (car codes)
      (extend (first codes) #\; (second codes)))))

(find-color-set cl-colors:+wheat+)
(build-control-string cl-colors:+red+)

(defun make-color-string (color)
  (concatenate 'string
	       (append
		`( ,(code-char 27) #\[)
		(build-control-string color)
		'(#\m))))


(defmacro format-with-color (color dest control-string &rest args)
  (let ((sym (gensym)))
    `(let ((,sym (format nil "~a~a~a"
			 (make-color-string ,color)
			 ,control-string
			 +reset-color-string+)))
       (format ,dest ,sym ,args))))

(defparameter +reset-color-string+
  (concatenate 'string (list (code-char 27) #\[ #\0 #\m)))
