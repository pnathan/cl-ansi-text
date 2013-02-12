cl-ansi-text

Because color in your terminal is nice.



Examples:

[2]> (cl-ansi-text:with-color (cl-colors:+red+) (format t "hi"))
hi
NIL
[3]> (cl-ansi-text:with-color (cl-colors:+green+) (format t "hi"))
hi
NIL
[4]> (cl-ansi-text:with-color (cl-colors:+blue+) (format t "hi"))
hi
NIL


Note that your terminal MUST be ANSI-compliant to show these
colors. My SLIME REPL (as of Feb 2013) does not display these
colors. I have to use a typical Linux/OSX terminal to see them.

This has been tested to work on a Linux system with SBCL and CLISP.