# Python utilities for Thermopack

This folder contains suplements to thermopack.

**./exports**: Script for generating lists used for symbol export for the different linkers (GCC - libthermopack_export.version, CLANG - libthermopack_export.symbols and MSVC - thermopack.def).  
  
**./dependencies**: Generate dependencies for ../../Makefile by looping fortran source files in ../../src.  
  
**./datadb**: Read files from ../../fluids and ../../binaries and generate compdatadb.f90/mixdatadb.f90/saftvrmie_datadb.f90 for thermopack. Must be copied manually to ../../src/. 
    Also generates the file Component-name-mapping.md for the wiki.
