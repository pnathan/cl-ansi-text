cl-ansi-text

Because color in your terminal is nice.



Examples:

```
[2]> (cl-ansi-text:with-color (cl-colors:+red+) (format t "hi"))
hi
NIL
[3]> (cl-ansi-text:with-color (cl-colors:+green+) (format t "hi"))
hi
NIL
[4]> (cl-ansi-text:with-color (cl-colors:+blue+) (format t "hi"))
hi
NIL
```

Note that your terminal MUST be ANSI-compliant to show these
colors. My SLIME REPL (as of Feb 2013) does not display these
colors. I have to use a typical Linux/OSX terminal to see them.

This has been tested to work on a Linux system with SBCL, CLISP and
CCL. CCL may not work quite perfectly, some level of conniptions were
encountered in testing. The interested reader is advised to check the
MAKE-LOAD-FORM defmethod in cl-ansi-text.lisp.

An earlier variant was tested on OSX 10.6 with SBCL.

License: LLGPL
