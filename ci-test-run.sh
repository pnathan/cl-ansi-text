#!/bin/bash
echo "CI run using SBCL"
sbcl --script run-tests.lisp
