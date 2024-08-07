#!/bin/bash

set -e

g++-12 -c main.cpp
g++-12 -o main main.o wrappers/*.o src/*.o /opt/homebrew/lib/gcc/12/libgfortran.dylib
./main