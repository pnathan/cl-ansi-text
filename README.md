# cl-ansi-text

Because color in your terminal is nice.

## Usage
```lisp
* (ql:quickload :cl-ansi-text)
;To load "cl-ansi-text":
;  Load 1 ASDF system:
;    cl-ansi-text
;; Loading "cl-ansi-text"
; => (:CL-ANSI-TEXT)
```

### `(with-color ((color  &key (stream t) (effect :unset) (style :foreground)) &body body))`

The main macro is called `with-color`, which creates an enviroment where everything that is put on `stream` gets colored according to `color`. Color options are `:black`, `:red`, `:green`, `:yellow`, `:blue`, `:magenta`, `:cyan` and `:white`. You can also use a color structure from cl-colors, like `cl-colors:+red+`.
```lisp
* (import 'cl-ansi-text:with-color)  
; => T
* (with-color (:red)
    (princ "Gets printed red...")
    (princ "and this too!"))
; Gets printed red...and this too!
; => "and this too!"
```

There are also functions with the name of the colors, that return the string, colored:
```lisp
* (import 'cl-ansi-text:yellow)
; => T
* (yellow "Yellow string")
; => "Yellow string"
* (princ (yellow "String with yellow background" :style :background))
; "String with yellow background"
; => "String with yellow background"
* (import 'cl-ansi-text:red)
; => T
* (princ
   (concatenate
    'string
    (yellow "Five") " test results went " (red "terribly wrong") "!"))
; Five test results went terribly wrong!
; => "Five test results went terribly wrong!"

# Note

Note that your terminal MUST be ANSI-compliant to show these
colors. My SLIME REPL (as of Feb 2013) does not display these
colors. I have to use a typical Linux/OSX terminal to see them.

This has been tested to work on a Linux system with SBCL, CLISP and
CCL. CCL may not work quite perfectly, some level of conniptions were
encountered in testing. The interested reader is advised to check the
MAKE-LOAD-FORM defmethod in cl-ansi-text.lisp.

An earlier variant was tested on OSX 10.6 with SBCL.

License: LLGPL
