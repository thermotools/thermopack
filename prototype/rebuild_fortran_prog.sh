#!/bin/bash

set -e

cd src

rm ./*.o
rm ./*.mod

gfortran -c compdata.f90 compdatadb.f90 compdatautils.f90 constants.f90 base_eos.f90 variants.f90 notidgas.f90

cp constants.mod ../fortran_test/constants.mod
cp base_eos.mod ../fortran_test/base_eos.mod
cp notidgas_mod.mod ../fortran_test/notidgas_mod.mod
cp variants.mod ../fortran_test/variants.mod
cp compdata_mod.mod ../fortran_test/compdata_mod.mod
cp compdatadb.mod ../fortran_test/compdatadb.mod
cp compdatautils.mod ../fortran_test/compdatautils.mod

cd ../fortran_test

gfortran -c main.f90
gfortran -o fortran_main main.o ../src/*.o
./fortran_main