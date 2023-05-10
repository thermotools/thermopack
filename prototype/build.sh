#!/bin/bash

set -e

# Compiling fortran source, building archive, and moving mod files to cpp_wrapper directory
cd src || exit 1
gfortran -c constants.f90 base_eos.f90 notidgas.f90 variants.f90
ar -r libdemo_fortran.a *.o
mv libdemo_fortran.a ../cpp_wrapper/libdemo_fortran.a
cp constants.mod ../cpp_wrapper/constants.mod
cp base_eos.mod ../cpp_wrapper/base_eos.mod
cp notidgas_mod.mod ../cpp_wrapper/notidgas_mod.mod
cp variants.mod ../cpp_wrapper/variants.mod
python -m fortwrap -g -i ../FortWrapOptions.txt --no-vector

# Compiling ISO_C_BINDINGS and C++ wrapper.
cd ../cpp_wrapper || exit 1
gfortran -c FortranISOWrappers.f90
ar -r libdemo_fortran.a FortranISOWrappers.o
ar -s libdemo_fortran.a
g++-12 -c BaseEos.cpp NotIdGas.cpp VariantEoS.cpp Variant1.cpp Variant2.cpp

# Compiling and linking C++ wrapper with pybind11
cd build || exit 1
cmake .. && make
cp libdemo.cpython-39-darwin.so ../../py_wrapper/libdemo.so

# Compile and link C++ main program
cd ../..
g++-12 -c main.cpp
g++-12 -o cpp_main main.o cpp_wrapper/*.o src/*.o /opt/homebrew/lib/gcc/12/libgfortran.dylib

echo "Finished building and wrapping."
echo "Executable is at : ./cpp_main"
echo "Fortran archive is at : cp_wrapper/libdemo_fortran.a"
echo "Python binary lib is at : py_wrapper/libdemo.so"