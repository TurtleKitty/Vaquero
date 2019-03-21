#!/bin/sh

cd interpreter
csc -X r7rs -R r7rs -w -O2 -d0 main.scm -o ../vaquero

