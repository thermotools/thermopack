# Python utilities for Thermopack

This folder contains suplements to thermopack.

**./exports**: Script for generating lists used for symbol export for the different linkers (GCC - libthermopack_export.version, CLANG - libthermopack_export.symbols and MSVC - thermopack.def).  
  
**./generate_dependensies.py**: Generate dependencies for ../../Makefile by looping fortran source files in ../../src.  
  
**./complist.py (using compdata.py)**: Read files from ../../fluids and generate compdatadb.f90 file for thermopack. Must be copied manually to ../../src/compdatadb.f90.  
  
**./binarylist.py (using binarydata.py)**: Read files from ../../binaries and generate mixdatadb.f90 file for thermopack. Must be copied manually to ../../src/mixdatadb.f90.  
