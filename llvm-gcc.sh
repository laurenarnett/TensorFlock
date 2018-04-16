#!/bin/sh

if [ "$(uname)" != "Linux" ]
then
    echo "Please run this script using 'make docker-shell'"
    exit 1
fi

/root/llvm-gcc4.2-2.9-x86_64-linux/bin/llvm-gcc --sysroot=/root/llvm-gcc4.2-2.9-x86_64-linux -fnested-functions -S -O0 -emit-llvm $1
