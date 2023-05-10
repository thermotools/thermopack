#!/bin/bash

set -e

gfortran -c main.f90
gfortran -o fortran_main main.o src/*.o
./fortran_main