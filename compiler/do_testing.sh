#!/bin/bash

MYLANG=tony
MYCOMP=Main.native
LIB=libtony.a

name="$1"
OPT=false
F=false
I=false

case $2 in
  "-O" O=true;;
  "-f" F=true;;
  "-i" I=true;;
esac

case $3 in
  "-O" O=true;;
  "-f" F=true;;
  "-i" I=true;;
esac

case $4 in
  "-O" O=true;;
  "-f" F=true;;
  "-i" I=true;;
esac

if [$F]; then
  ./$MYCOMP < ??? > llvmIR_name
  if OPT then
    opt llvmIR_name -S -o llvmIR_name
  fi
  llc llvmIR_name
  ;
else if [$I]; then
  if [$OPT]; then
    ./$MYCOMP < ??? > llvmIR_name
    opt llvmIR_name -f -S
    ;
  else
    ./$MYCOMP < ??? >
    ;
  fi
  ;
else
  n="{name/%.MYLANG}"
  llvmIR_name="$n".ll
  assembly_name="$n".s
  output_name="$n".out
  ./$MYCOMP < $1 > llvmIR_name
  if OPT then
    opt llvmIR_name -S -o llvmIR_name
  fi
  llc llvmIR_name -o assembly_name
  clang -o output_name assembly_name $LIB -lgc 2> /dev/null
  ;
fi
fi
