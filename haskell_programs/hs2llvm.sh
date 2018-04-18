#!/bin/sh

stack ghc $1 -- -O0 -fllvm -keep-llvm-file 2> /dev/null
cat *.ll | less
rm *.hi *.ll
