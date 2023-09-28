---
layout: default
version: 
title: Installing the latest version of ThermoPack
permalink: /vcurrent/source_build.html
---

## Using pip
Thermopack has been compiled for Windows, Linux and macOS
and made available on the [Python Package Index](https://pypi.org/project/thermopack/) (pypi), and can be
installed using pip

```bash
pip3 install thermopack
```

For documentation on the version available on pypi, refer to the appropriate version number in the sidebar.

## Building from source
The following sections show how to fetch, compile and install Thermopack and
the Python frontend pycThermopack. When things are properly installed, it may
be useful to look into the examples provided in the
[addon/pyExamples](addon/pyExamples/README.md).

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

#### MacOS with arm64 architecture (M1, M2 etc.)

Use the alternative `Makefile_arm64` and `Makefile_arm64.code` by renaming the files
```
Makefile => Makefile_x86
Makefile.code => Makefile_x86.code
Makefile_arm64 => Makefile
Makefile_arm64.code => Makefile.code
```


then follow the steps under MacOS setup. Please feel free to leave an issue if you have build problems, the build system for MacOS on Apple Silicon has not been as thouroughly tested as other build systems, and is still somewhat a work in progress.

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