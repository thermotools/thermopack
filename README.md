# Thermopack
Thermopack is a thermodynamics library for multi-component and multi-phase
thermodynamics developed at [SINTEF Energy
Research](https://www.sintef.no/en/sintef-energy/). Through decades of research,
we have developed a software that performs thermodynamic calculations. A large
selection of equations of state has been implemented in this software. Most of
these equations of state have been developed by other research groups around
the world, but some of them have been developed by us. Thermopack has has been
a much-appreciated in-house powerhouse.

With the slogan of SINTEF in mind - *Technology for a better society* - we want
to share Thermopack with everybody, free of charge through the MIT open-source
license. Thermopack is written in modern FORTRAN to handle heavy numerical
computations associated with process and computational fluid dynamics (CFD)
simulations. The thermodynamic framework is easily interfaced from C/C++ and
also contains a flexible Python wrapper to make scripting easy. The Python
interface is also a building block for the Thermopack graphical user interface,
where it is possible to plot thermodynamic phase diagrams with the most
frequently used equations of state. The graphical user interface is currently
running on the Windows and Linux operating systems.

## Table of contents

  * [Setup](#setup)
  * [File System](#file-system)
  * [Please cite](#please-cite)
  * [Authors and contact persons](#authors-and-contact-persons)
  * [License](#license)
  * [Acknowledgments](#acknowledgments)

## Setup
The following sections show how to fetch, compile and install Thermopack and
the Python frontend pycThermopack. When things are properly installed, it may
be useful to look into the examples provided in the
[addon/pyExamples](addon/pyExamples/README.md).

### pypi
Thermopack have been compiled for Windows, Linux and macOS
and made available on the Python Package Index (pypi), and can be
installed using pip.

```bash
pip3 install --user thermopack
```

### Prerequisites
Thermopack source code can be compiled with the [GNU Fortran
compiler](https://gcc.gnu.org/wiki/GFortran) or [Intel
FORTRAN](https://software.intel.com/content/www/us/en/develop/tools/compilers/fortran-compilers.html)
and is dependent on the [LAPACK](http://www.netlib.org/lapack/) and
[BLAS](http://www.netlib.org/blas/) libraries. On the Windows OS the code can
be compiled using [Microsoft Visual
Studio](https://visualstudio.microsoft.com/vs/). A solution file is found in
[thermopack/MSVStudio](https://github.com/thermotools/thermopack/tree/main/MSVStudio),
assuming that the Intel Fortran compiler is integrated with Microsoft Visual
Studio.

### Linux setup
The Thermopack source code is downloaded by cloning the library to your local
computer. The following commands assume that you have a local installation of
[Git](https://git-scm.com/), [gfortran](https://gcc.gnu.org/fortran/) and
[Python 3](https://www.python.org/) with [pip](https://pypi.org/project/pip/).
To compile using Intel FORTRAN, use `make optim_ifort`.

```bash
# Fetch and compile
git clone https://github.com/thermotools/thermopack.git
cd thermopack
make optim

# Prepare and install pycThermopack, aka "thermopack"
# Remark: On some systems, Python 3 is installed as python, not python3. If so,
# you can replace "python3" with "python" and perhaps also "pip3" with "pip" in
# the below.
cd addon/pycThermopack
python3 makescript.py optim
pip3 install --user .
```

If you are working actively with the Thermopack code, either the Fortran
backend or the Python frontend, then it may be useful to install in editable
mode (aka develop mode). This will install a _link_ to the develop files
instead of copying the files when installing. This can be done with the `-e`
option, i.e.:

```bash
pip3 install -e --user .
```

See also [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
more details on pycThermopack.

### MacOS setup
The easiest way to get started is via [Homebrew](https://brew.sh). After
following the instructions on their website to set it up, install `gcc`
and `make`. Open a terminal (e.g. the default Terminal.app), and type:
```
brew install gcc make
```
Then follow the Linux instructions above, just replace `make` with `gmake`.
Some additional packages like `git` and `python` can also be installed via
Homebrew before you start, but if you use a recent version of MacOS (e.g.
Catalina), then the versions installed by default should be sufficient.

### Windows setup
Download and compile LAPACK and BLAS libraries (you will need CMake and
a working compiler).

To be compatible with the current settings, you need to compile using Intel
Fortran with Visual Studio, and configure as follows:

```
- Fortran/Data/Default Real KIND = 8
- Fortran/External Procedures/Calling Convention = cref
- Fortran/External Procedures/Name Case Interpretation = lowercase
- Fortran/External Procedures/String Length Argument Parsing = After All Arguments
- Fortran/External Procedures/Append Underscore to External Names = Yes
- Fortran/Floating Point/Floating Point Model = precise
```

Copy LAPACK and BLAS libraries to the paths:

- thermopack\lapack\lib\Debug
- thermopack\lapack\lib\Release

Open thermopack\MSVStudio\thermopack.sln using Visual Studio, and compile the wanted configuration.

See [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
how to install pycThermopack.

#### MSYS2/Mingw-W64 setup
Thermopack can also be compiled using gfortran in the MSYS2 environment. Download MSYS2 from [https://www.msys2.org](https://www.msys2.org), and install and update the package system following the instructions given. Avoid having spaces in the MSYS2 installation directory path, as the Makefile might not work. Having a working MSYS2 installation, thermopack can be compiled after installing the following packages:

```bash
pacman -S git
pacman -S mingw-w64-x86_64-gcc-fortran
pacman -S mingw-w64-x86_64-openblas
pacman -S mingw-w64-x86_64-make
pacman -S mingw-w64-x86_64-dlfcn
```

Open the `MSYS2 MinGW 64-bit` application, and enter the following in the terminal:

```bash
git clone https://github.com/thermotools/thermopack.git
cd thermopack
mingw32-make.exe optim
```

See [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
how to install pycThermopack for the MSYS2 environment.

### Docker setup
See [addon/docker/README.md](addon/docker/README.md) for
available Dockerfiles to run Thermopack with docker.

### CMake setup
See [thermopack_cmake](https://github.com/morteham/thermopack_cmake) for prototype CMake scripts to compile Thermopack.

## File System
Brief description of file structure:

- `thermopack/`: Main library folder containing make scripts etc.
- `thermopack/src/`: Main path for Fortran source code
- `thermopack/unittest/`: Test files written for pFUunit
- `thermopack/bin/`: Compiled binaries and libraries
- `thermopack/doc/`: Memos and doxygen documentation
- `thermopack/fluids/`: Pure fluid files
- `thermopack/binaries/`: Files containing binary interaction parameters etc.
- `thermopack/MSVStudio/`: Microsoft Visual Studio project and solution files
- `thermopack/include/`: C/C++ include file
- `thermopack/pyplot/`: Example plot-scripts (Plotting text files generated by Thermopack)
- `thermopack/addon/`: Add-on functionality
- `thermopack/addon/pycThermopack/`: Python interface
- `thermopack/addon/pyUtils/`: Python utilitties for generating fortran code and makefile input.
- `thermopack/addon/trend_interface/`: Interface for working with the TREND/EOSCG library developed by Roland Span and Ruhr-Universität Bochum

## Please cite
Thermopack has been developed through many projects, and have produced many
articles. If you are writing academic publications, please cite one or more of
the following articles:

- For general usage:  
[Thermodynamic Modeling with Equations of State: Present Challenges with Established Methods](https://doi.org/10.1021/acs.iecr.7b00317)

- Quantum cubic:  
[Accurate quantum-corrected cubic equations of state for helium, neon, hydrogen, deuterium and their mixtures](https://doi.org/10.1016/j.fluid.2020.112790)

- SAFT-VR Mie and SAFT-VRQ Mie:  
[Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium](https://doi.org/10.1063/1.5111364)  
[Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium](https://doi.org/10.1063/1.5136079)  
[Choice of reference, the influence of non-additivity and challenges in thermodynamic perturbation theory for mixtures](https://doi.org/10.1063/1.5142771)

- CPA, PC-SAFT or cubic models with Wong–Sandler, Huron–Vidal or UNIFAC mixing
rules:  
[Thermodynamic models to accurately describe the PVTxy-behavior of water/carbon dioxide mixtures](https://doi.org/10.1016/j.fluid.2017.02.006)

- Using dry-ice and water-ice model or the tc-PR/tc-RK:  
[Depressurization of CO<sub>2</sub>-N<sub>2</sub> and CO<sub>2</sub>-He in a pipe: Experiments and modelling of pressure and temperature dynamics](https://doi.org/10.1016/j.ijggc.2021.103361)

- Energy-density and entropy-density flashes:  
[The influence of CO2 mixture composition and equations of state on simulations of transient pipeline decompression](https://doi.org/10.1016/j.ijggc.2016.07.004)

- Mapping spinodals or critical points:  
[The spinodal of single-and multi-component fluids and its role in the development of modern equations of state](https://doi.org/10.1016/j.fluid.2016.12.018)  
[Predicting triggering and consequence of delayed LNG RPT](https://doi.org/10.1016/j.jlp.2018.06.001)

- Perturbation theories for Lennard-Jones spline fluid:  
[Perturbation theories for fluids with short-ranged attractive forces: A case study of the Lennard-Jones spline fluid](https://doi.org/10.1063/5.0082690)  
[Thermodynamic properties of the 3D Lennard-Jones/spline model](https://doi.org/10.1080/00268976.2019.1664780)

## Authors and contact persons
Morten Hammer (morten.hammer@sintef.no)<br>
Ailo Aasen (ailo.aasen@sintef.no)<br>
Øivind Wilhelmsen (oivind.wilhelmsen@sintef.no)

## License
Thermopack is distributed under the [MIT license](https://github.com/thermotools/thermopack/blob/main/LICENSE).

## Acknowledgments
A number of colleagues at SINTEF Energy Research have contributed to the
development of thermopack. We gratefully acknowledge their contributions.
