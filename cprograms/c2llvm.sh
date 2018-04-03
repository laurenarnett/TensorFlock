#!/bin/sh

clang -S -O0 -emit-llvm -o temp.ll $1
cat temp.ll
rm temp.ll

