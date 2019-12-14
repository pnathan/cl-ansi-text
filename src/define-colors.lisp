
(in-package :cl-ansi-text)

(defmacro define-colors ()
  `(progn
     ,@(map 'list
            (lambda (color)
              `(defun ,(intern (symbol-name color)) (string &key (effect :unset) (style :foreground))
                 ,(format nil "Returns a string decorated in ~(~a~)." color)
                 (with-output-to-string (s)
                   (with-color (,color :stream s :effect effect :style style)
                     (write-string string s)))))
            +term-colors+)))

(define-colors)


