#!/bin/bash
echo "CI run using SBCL"
sbcl --eval "(ql:quickload '(:cl-colors :alexandria :fiveam))" --load cl-ansi-text.lisp --load test/cl-ansi-text-test.lisp --eval "(cl-ansi-text-test::ci-run)"  --non-interactive
