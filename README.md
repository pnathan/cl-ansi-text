# cl-ansi-text

Because color in your terminal is nice.
[![Build Status](https://travis-ci.org/pnathan/cl-ansi-text.svg?branch=master)](https://travis-ci.org/pnathan/cl-ansi-text)

## Usage example -
```lisp
* (ql:quickload :cl-ansi-text)
;To load "cl-ansi-text":
;  Load 1 ASDF system:
;    cl-ansi-text
;; Loading "cl-ansi-text"
; => (:CL-ANSI-TEXT)
```

The main macro is called `with-color`, which creates an enviroment where everything that is put on `stream` gets colored according to `color`. Color options are `:black`, `:red`, `:green`, `:yellow`, `:blue`, `:magenta`, `:cyan` and `:white`. You can also use a color structure from `CL-COLORS`, like `cl-colors:+red+`.

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
```
At any point, you can bind the `*enabled*` special variable to `nil`, and anything inside that binding will not be printed colorfully:
```lisp
* (let (cl-ansi-text:*enabled*)
    (princ (red "This string is printed normally")))
```

# API


## BLUE

Returns a string with the `blue'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## MAGENTA

Returns a string with the `magenta'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## CYAN

Returns a string with the `cyan'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## GREEN

Returns a string with the `green'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## WITH-COLOR

Writes out the string denoting a switch to `color`, executes body,
then writes out the string denoting a `reset`.

*enabled* dynamically controls expansion..

## YELLOW

Returns a string with the `yellow'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## BLACK

Returns a string with the `black'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## *ENABLED*

Turns on/off the colorization of functions

## MAKE-COLOR-STRING

Takes either a cl-color or a list denoting the ANSI colors and
returns a string sufficient to change to the given color.

Will be dynamically controlled by *enabled* unless manually specified
otherwise

## RED

Returns a string with the `red'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## WHITE

Returns a string with the `white'string denotation preppended and the `reset' string denotation appended.

*enabled* dynamically controls the function.

## +RESET-COLOR-STRING+

This string will reset ANSI colors


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
