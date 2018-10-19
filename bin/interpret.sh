#!/bin/sh

cd interpreter
rlwrap csi -R r7rs -script main.scm $1 $2 $3

