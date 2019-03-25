#!/bin/sh

raco pkg remove lisp
cd $(dirname $0)
raco pkg install
rm -r compiled lang/compiled
