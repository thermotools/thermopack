# Thermopack
Thermopack is a thermodynamics library for multi-component and multi-phase thermodynamics.

### Prerequisites
Thermopack source code can be compiled with the [GNU Fortran compiler](https://gcc.gnu.org/wiki/GFortran) or [Intel Fortran](https://software.intel.com/content/www/us/en/develop/tools/compilers/fortran-compilers.html) and is dependent on the [LAPACK](http://www.netlib.org/lapack/) and [BLAS](http://www.netlib.org/blas/) libraries. On the Windows OS the code can be compiled using [Microsoft Visual Studio](https://visualstudio.microsoft.com/vs/). A solution file is found in [thermopack/MSVStudio](https://github.com/SINTEF/thermopack/MSVStudio), assuming that the Intel Fortran compiler is integrated with Microsoft Visual Studio.

### Initial setup
The Thermopack source code is downloaded by cloning the library to your local computer. The following commands assumes that you have a local installation of [GIT](https://git-scm.com/), gfortran and python3. To compile using intel fortran, use make optim_ifort.

```bash
git clone https://github.com/SINTEF/thermopack.git
cd thermopack
make optim
cd addon/pyThermopack
python3 makescript.py optim
python3 setup.py install
```

### Windows setup

Download and compile lapack and blas libraries (Need cmake and a working compiler)
Intel Fortran with Visual Studio:
Fortran/Data/Default Real KIND = 8
Fortran/External Procedures/Calling Convention = cref
Fortran/External Procedures/Name Case Interpretation = lowercase
Fortran/External Procedures/String Length Argument Parsing = mixed_str_len_arg
Fortran/External Procedures/Append Underscore to External Names = Yes
Fortran/Floating Point/Floating Point Model = precise

Copy lapack and blas libraries to the paths:
thermopack\lapack\lib\Debug
thermopack\lapack\lib\Release

Open thermopack\MSVStudio\thermopack.sln using Visual Studio, and compile the wanted configuration.

## Running the tests
The test files are written for pFUnit, a unit testing framework enabling JUnit-like testing of serial and MPI-parallel software written in Fortran. The code needed to run the tests are found at [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit), and the tests are located in [thermopack/unittests](https://github.com/SINTEF/unittest).

## Please cite
xxxxxxx


## Authors and contact persons
Morten Hammer (Morten.Hammer@sintef.no), Ailo Aasen (Ailo.Aasen@sintef.no), Øivind Wilhelmsen (Oivind.Wilhelmsen@sintef.no)

## License
Thermopack is distributed under the [MIT](https://github.com/SINTEF/LICENSE) license.

## Acknowledgments
A number of colleagues at SINTEF Energy Research have contributed to the development of thermopack. We gratefully acknowledge their contributions.


## File System
>thermopack/: main library
>thermopack/src/: Main path for Fortran source code
>
>thermopack/addon/: Add-on functionality
>thermopack/addon/pyThermopack/: Python interface
>
