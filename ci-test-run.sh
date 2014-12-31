#!/bin/bash
echo "CI run using SBCL"
sbcl --script travis-tests.lisp
