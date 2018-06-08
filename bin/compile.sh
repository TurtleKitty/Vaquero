#!/bin/sh

cd interpreter
csc -require-extension r7rs -w -O2 -d0 main.scm -o ../vaquero

