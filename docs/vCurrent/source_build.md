---
layout: default
version: 
title: Installing the latest version of ThermoPack
permalink: /vcurrent/source_build.html
---

- [Using pip](#using-pip)
- [Installing from wheels](#installing-from-wheels)
- [Building from source](#building-from-source)
  - [Prerequisites](#prerequisites)
  - [CMake setup (macOS and Linux)](#cmake-setup-macos-and-linux)
  - [CMake setup (Windows)](#cmake-setup-windows)
- [Legacy build system (without CMake)](#legacy-build-system-without-cmake)
  - [Linux setup](#linux-setup)
  - [MacOS setup](#macos-setup)
  - [Windows setup](#windows-setup)
    - [MSYS2/Mingw-W64 setup](#msys2mingw-w64-setup)
  - [Docker setup](#docker-setup)

## Using pip
Thermopack has been compiled for Windows, Linux and macOS
and made available on the [Python Package Index](https://pypi.org/project/thermopack/) (pypi), and can be
installed using pip

```bash
pip3 install thermopack
```

For documentation on the version available on pypi, refer to the appropriate version number in the sidebar.

## Installing from wheels
Pre-built wheels for the latest version of ThermoPack on GitHub are available for download [here](). Refer to the linked page for instructions on how to install packages directly from a python wheel. Please note that the latest version on GitHub may be less stable, tested, and well documented than the versions distributed on PyPI.

## Building from source
The following sections show how to fetch, compile and install Thermopack and
the Python frontend pycThermopack. When things are properly installed, it may
be useful to look into the examples provided in the [getting started guide](getting_started.html), and the 
[pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples).

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

For macOS and Linux, Lapack and Blas can likely be installed using `apt`, `brew`, or similar. For windows, Lapack will need to be built from source. The [CMake setup for Windows](#cmake-setup-windows) is configured to handle this automatically.

### CMake setup (macOS and Linux)

The `cmake`-based build system assumes that you have Lapack and gfortran installed, see above instructions for more on that.

Build and install thermopack by running
```bash
mkdir build
cd build
cmake ..
make install
```

This will ensure that the thermopack dynamic library is properly installed to `thermopack/installed` and `thermopack/addon/pycThermopack/thermopack`.

To set up the python wrapper, 
```bash
python addon/pycThermopack/map_platform_specifics.py
pip install addon/pycThermopack/
```
this will generate the file `addon/pycThermopack/thermopack/platform_specifics.py` and install thermopack to your activated virtual environment.

ThermoPack can be configured to return computed properties as either tuples (`v2`) or using the `Property` struct (`v3`), this is toggled with
the `-diffs` flag when running `map_platform_specifics.py` as
```bash
python map_platform_specifics.py -diffs=v2 # Use tuples
python map_platform_specifics.py -diffs=v3 # use Property
```
the default value is `-diffs=v3`.

### CMake setup (Windows)

To compile thermopack (and Lapack) with intel fortran, run
```
mkdir build
cd build
cmake .. -G Ninja -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_BUILD_TYPE=Release
cmake --build . --config=Release --target install
```
Compile and install Lapack, and install the thermopack dynamic library to `thermopack/installed` and `thermopack/addon/pycThermopack/thermopack`.

To configure and install the python-wrapper, run
```
python addon/pycThermopack/map_platform_specifics.py
pip install addon/pycThermopack/
```

*Note:* If your thermopack dynamic library is called `libthermopack.dll`, and not `thermopack.dll`, you will instead need to run
```
python addon/pycThermopack/map_platform_specifics.py --ifort=True
pip install addon/pycThermopack/
```

## Legacy build system (without CMake)

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

See also [addon/pycThermopack/README.md](https://github.com/thermotools/thermopack/tree/main/addon) for
more details on pycThermopack.

### MacOS setup
The easiest way to get started is via [Homebrew](https://brew.sh). After
following the instructions on their website to set it up, install `gcc`
and `make`. Open a terminal (e.g. the default Terminal.app), and type:
```
brew install gcc make
```
Then follow the Linux instructions above (you may need to replace `make` with `gmake`).
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

See [addon/pycThermopack/README.md](https://github.com/thermotools/thermopack/tree/main/addon) for
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

See [addon/pycThermopack/README.md](https://github.com/thermotools/thermopack/tree/main/addon) for
how to install pycThermopack for the MSYS2 environment.

### Docker setup
See [addon/docker/README.md](https://github.com/thermotools/thermopack/tree/main/addon/docker) for
available Dockerfiles to run Thermopack with docker.