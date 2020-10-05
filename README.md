# Thermopack
Thermopack is a thermodynamics library for multi-component and multi-phase thermodynamics developed at SINTEF Energy Resarch. Through decades of research, we have developed a software that performs thermodynamic calculations. A large selection of equations of state has been implemented in this software. Most of these equations of state have been developed by other research groups around the world, but some of them have been developed by us. Thermopack has has been a much-appreciated in-house powerhouse.   
  
With the slogan of SINTEF in mind: *Technology for a better society*  
  
we want to share Thermopack with everybody, free of charge through the MIT open-source license. Thermopack is written in modern FORTRAN to handle heavy numerical computations associated with process and computational fluid dynamics (CDF) simulations. The thermodynamic framework is easily interfaced from C/C++ and contains a flexible Python wrapper to make plotting easy. The Python interface is also a building block for the Thermopack graphical user interface, where it is possible to plot thermodynamic phase diagrams with the most frequently used equations of state. The graphical user interface is currently running on the Windows and Linux operating systems. 


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

To be compatible with the current settings, you need to compile using Intel Fortran with Visual Studio, and conigure as follows:

- Fortran/Data/Default Real KIND = 8
- Fortran/External Procedures/Calling Convention = cref
- Fortran/External Procedures/Name Case Interpretation = lowercase
- Fortran/External Procedures/String Length Argument Parsing = After All Arguments
- Fortran/External Procedures/Append Underscore to External Names = Yes
- Fortran/Floating Point/Floating Point Model = precise

Copy lapack and blas libraries to the paths:

- thermopack\lapack\lib\Debug
- thermopack\lapack\lib\Release

Open thermopack\MSVStudio\thermopack.sln using Visual Studio, and compile the wanted configuration.

## Running the tests
The test files are written for pFUnit, a unit testing framework enabling JUnit-like testing of serial and MPI-parallel software written in Fortran. The code needed to run the tests are found at [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit), and the tests are located in [thermopack/unittests](https://github.com/SINTEF/unittest). The compilation (make unittest_debug) depends on the variable PFUNIT pointing to a working installation of pFUnit.

## Please cite

Thermopack have been developed through many projects, that have produced many articles. If you are writing academic publications, plase cite one or more of the following articles:

- For general usage:  
[Thermodynamic Modeling with Equations of State: Present Challenges with Established Methods](doi.org/10.1021/acs.iecr.7b00317)

- Quantum cubic:  
[Accurate quantum-corrected cubic equations of state for helium, neon, hydrogen, deuterium and their mixtures](doi.org/10.1016/j.fluid.2020.112790)

- SAFT-VR Mie:  
[Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium](doi.org/10.1063/1.5111364)  
[Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium](doi.org/10.1063/1.5136079)  
[Choice of reference, the influence of non-additivity and challenges in thermodynamic perturbation theory for mixtures](doi.org/10.1063/1.5142771)

- CPA, PC-SAFT or cubic models with Wong–Sandler, Huron–Vidal or UNIFAC mixing rules:  
[Thermodynamic models to accurately describe the PVTxy-behavior of water/carbon dioxide mixtures](doi.org/10.1016/j.fluid.2017.02.006)

- Using dry-ice and water-ice model or the tc-PR/tc-RK:  
[Depressurization of \coto-\nto and \coto-\he in a pipe: Experiments and modelling of pressure and temperature dynamics](doi.org/XXX)

- Energy-density and entropy-density flashes:  
[The influence of CO2 mixture composition and equations of state on simulations of transient pipeline decompression](doi.org/10.1016/j.ijggc.2016.07.004)

- Mapping spinodals or critical points:  
[The spinodal of single-and multi-component fluids and its role in the development of modern equations of state](doi.org/10.1016/j.fluid.2016.12.018)  
[Predicting triggering and consequence of delayed LNG RPT](doi.org/10.1016/j.jlp.2018.06.001)


## Authors and contact persons
Morten Hammer (Morten.Hammer@sintef.no), Ailo Aasen (Ailo.Aasen@sintef.no), Øivind Wilhelmsen (Oivind.Wilhelmsen@sintef.no)

## License
Thermopack is distributed under the [MIT](https://github.com/SINTEF/LICENSE) license.

## Acknowledgments
A number of colleagues at SINTEF Energy Research have contributed to the development of thermopack. We gratefully acknowledge their contributions.


## File System

Brief description of file structure:

- `thermopack/`: Main library folder containing make scripts etc.
- `thermopack/src/`: Main path for Fortran source code
- `thermopack/unittest/`: Test files written for pFUunit
- `thermopack/bin/`: Compiled binaries and libraries
- `thermopack/doc/`: Memos and doxygen documentation
- `thermopack/fluids/`: Pure fluid files
- `thermopack/binaries/`: Files containing binary interaction parameters etc.
- `thermopack/MSVStudio/`: Microsoft Visual Studio project and soluition files
- `thermopack/include/`: C/C++ include file
- `thermopack/pyplot/`: Example plot-scripts (Plotting text files generated by Thermopack)
- `thermopack/addon/`: Add-on functionality
- `thermopack/addon/pycThermopack/`: Python interface
- `thermopack/addon/pyUtils/`: Python utilitties for generating fortran code and makefile input.
- `thermopack/addon/trend_interface/`: Interface for working with the TREND/EOSCG library developed by Roland Span ant Ruhr-Universität Bochum

# For discussion
1. Should we aim for a generic unittest of F functions, and then simply run it for all model combos?
2. Should molar mass be given in g/mol or kg/mol (SI)?
