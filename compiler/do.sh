#!/bin/bash

./Main.native < $1 > a.ll
llc a.ll -o a.s
clang -o a.out a.s libtony.a 2> /dev/null
