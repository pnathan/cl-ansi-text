#!/bin/bash
error=0
if which sbcl; then
    echo "CI run using SBCL"
    sbcl --script run-tests.lisp
    error=$?
fi

if which lx86cl64; then
   echo "CI run using CCL"
   lx86cl64 -b --load run-tests.lisp
   error=$(($error+$?))
fi

exit $error
