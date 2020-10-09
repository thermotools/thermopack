# Guide for code contributions

## Branch model

Thermopack is developed mainly through the main branch, and pull requests should
be [fork based](https://help.github.com/articles/using-pull-requests/).

## Documentation

When adding a new model or a new numerical solver, please add a memo describing equations with required differentials and main functionallity. Add memo to [thermopack/doc/memo](https://github.com/SINTEF/thermopack/tree/main/doc/memo).

## Code

Try to keep new code consistent with the current style of Thermopack.

When submitting code for thermopack, please adhere to the following standards:

- Use 2 space indentaion - no tabs!
- Write readable code
    - Break lines for readability
        - Line should ideally not be longer than 80 columns
    - Use comments:
        - For complex code that is difficult to understand
        - Simple code does not need comments
- Thread safe code:
    - Do not initialize method (function or subroutine) variables at declaration, as this will trigger the SAVE option.
    - Use of global variables should be avoided, as this is not thread safe. Instead plase all variables in the eos specific containers inheriting the abstract base_eos_param class.
- Use *real* and *integer* and not *real(8)* and *integer(8)* etc. The default real and integer size is defined in the compiler option.
- Use good variable names
    - The name should indicate what the variable is/does
    - All variables should be explicit, ie. use *implicit none*
    - All method variables should have explicit intent. Ex. *real, intent(in) :: Temperature*
    - All variables should have a short description and a unit. Ex. *real, intent(in) :: T !> Temperature (K)*
- Ideally the code comments should be written using syntax recogniced by doxygen
- Add reference to relevant articles in the code. It is sufficient to add author names and digital object identifier (doi). Ex. *Peng and Robinson (10.1021%2Fi160057a011)*


New fluids and binary interaction data should be added as new *json* files, or modification to exsisting files, in [thermopack/fluids](https://github.com/SINTEF/thermopack/tree/main/fluids) and [thermopack/binaries](https://github.com/SINTEF/thermopack/main/tree/binaries), and generate compdatadb.f90 and mixdatadb.f90 form the scripts in [thermopack/addon/pyUtils](https://github.com/SINTEF/thermopack/tree/main/pyUtils).

Thermopack reqires analytical differentials in temperature, volume and mol numbers to second order for the residual Helmholtz energy. New models should include these differentials.

## Tests

New functionality should be accompanied by unit tests. The test files should be written for pFUnit, a unit testing framework enabling JUnit-like testing of serial and MPI-parallel software written in Fortran. The new tests should be included in [thermopack/unittests](https://github.com/SINTEF/thermopack/tree/main/unittest). The compilation (`make unittest_mode_compiler`) depends on the variable PFUNIT pointing to a working installation of pFUnit.

The new code need to compile, in debug, optim and openmp mode, and run using the gfortran (Linux) and the Intel FORTRAN (Linux/Windows) compilers. The existing and new unittests should pass when compiled with both gfortran and Intel FORTRAN compiled in debug, optim and opnemp mode. So if possible, new code should have been tested as extensively as possible.

The python interface pycThermopack functionallity should also not be broken. To ensure a workin python plugin, please run all example cases in [thermopack/addon/pyExample](https://github.com/SINTEF/thermopack/tree/main/addon/pyExample), and verify that all cases run by running [thermopack/addon/pyExample/run_all_examples.sh](https://github.com/SINTEF/thermopack/tree/main/addon/pyExample/run_all_examples.sh). Also run the exsisting python tests using pytest.

If new functionallity is added to pycThermopack, please add new test for the [thermopack/addon/pycThermopack/tests](https://github.com/SINTEF/thermopack/tree/main/addon/pycThermopack/tests)

A minimal procedure for testing on Linux would be to execute:
```bash
cd thermopack
make unittest_optim_gfortran
./run_unittest
cd thermopack/addon/pycThermopack
make optim
python -m pytest
cd ../pyExample
./run_all_examples.sh
```
and to repeat for debug and openmp mode.

###Requirements:
- [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit)
- python pytest
