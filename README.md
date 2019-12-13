# cl-ansi-text

Because color in your terminal is nice.
[![Build Status](https://travis-ci.org/pnathan/cl-ansi-text.svg?branch=master)](https://travis-ci.org/pnathan/cl-ansi-text)

Installation: `(ql:quickload :cl-ansi-text)`

## Usage example -

The main macro is `with-color`, which creates an enviroment where everything that is put on `stream` gets colored according to `color`.

Color options comes in several forms.

### Keyword Symbol

Basic 8 colors in the 3-bit color mode are supported, which are `:black`, `:red`, `:green`, `:yellow`, `:blue`, `:magenta`, `:cyan` and `:white`.

```lisp
* (with-color (:red)
    (princ "Gets printed red...")
    (princ "and this too!"))
; Gets printed red...and this too!
; => "and this too!"
```
### `CL-COLORS2:RGB` and `CL-COLORS2:HSV` object

These are color structures from `CL-COLORS2` (a maintained fork of `CL-COLORS`).
`CL-COLORS2` has several constants e.g. `cl-colors:+red+` that holds the corresponding color values.
`CL-COLORS2` also supports useful blending operations on colors.
Note that `CL-COLORS2` library provides a package `CL-COLORS`, not `CL-COLORS2`.

### Hex representation

These are CSS-style color strings such as `"#FF0000"`.

### Integer as a 24-bit color

It treats an integer as a hex string.
The bottom 8 bit is used for the blue, the 8th to 16th bits are used for green,
the 16th to 24th bits are used for red.
Remaining bits are ignored.

### List of numbers as a 24-bit color

It takes a list of three numbers (RGB) between 0 and 256.

## Function interface for printing in specific colors

We provide shorthand functions for generating a colored strings:

```lisp
* (yellow "Yellow string")
; => "Yellow string"
* (princ (yellow "String with yellow background" :style :background))
; "String with yellow background"
; => "String with yellow background"
* (princ
   (concatenate
    'string
    (yellow "Five") " test results went " (red "terribly wrong") "!"))
; Five test results went terribly wrong!
; => "Five test results went terribly wrong!"
```

You can bind the `*enabled*` special variable to `nil` to control the colorization:

```lisp
* (let (cl-ansi-text:*enabled*)
    (princ (red "This string is printed normally")))
```

# API

## *Type* color-specifier

``` lisp
(or unsigned-byte
    (cons (real 0 256)
          (cons (real 0 256)
                (cons (real 0 256)
                      nil)))
    cl-colors:rgb
    cl-colors:hsv
    term-colors
    color-string)
```

## *Type* term-colors

``` lisp
(member :black :red :green :yellow :blue :magenta :cyan :white)
```

## *Type* color-string

A string of length 3, 4, 6, or 7, that optionally starts with a `#`, and
the rest consists of 3 or 6 hexademical alphadigits (case-insensitive).

## *Macro* with-color

``` lisp
with-color (color &key (stream t) (effect :unset) (style :foreground)) &body body
```

Writes out the ANSI escape code string 
denoting `effect`, `style`, and a switch to `color`, then executes `body`,
then writes out the string that resets the decoration.

## *Function* make-color-string

``` lisp
make-color-string color &key (effect :unset) (style :foreground) enabled
```

Takes an object of `color-specifier` and returns a string sufficient to change to the given color.

Colorization is controlled by *enabled* unless manually specified otherwise by `:enabled` keyword.

## *Function* black, red, green, yellow, blue, magenta, cyan, white

Shortcut functions that takes a single argument, `string`, and returns a string
decorated by the corresponding color.

## *Special variable* `*enabled*`

Turns on/off the colorization.

## *Special variable* `*color-mode*`

Controls the way `make-color-string` emits the color code.

It should be one of the following keyword symbols: `:3bit`, `:8bit`, `:24bit`.
The specified color is converted to the nearest color in the color space.
The default value is `:8bit`.

Note that the actual appearance of the screen in the `:3bit` mode may be affected by
the terminal setting -- For example, many terminals do not use `FF0000` for the red.

## *Constant* `+reset-color-string+`

A constant string that resets the color state of the terminal.


# Running test

Run `./testscr.ros` with Roswell. You can also manually run the test with
`(ql:quickload :cl-ansi-text.test) (fiveam:run! :cl-ansi-text)`.

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
