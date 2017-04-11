#!/bin/sh

cd interpreter
rlwrap csi -script main.scm $1 $2 $3

