#!/bin/sh

cd interpreter
csc -w -O2 -d0 main.scm -o ../vaquero

