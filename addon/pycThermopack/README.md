# Introduction

pycThermopack: Python interface for Thermopack using ctypes

For examples on how to use, see [../pyExamples/](../pyExamples/README.md).

Note that the files `../../libthermopack_export.version` (Linux) and
`../../MSVStudio/thermopack.def` (Windows) define which symbols are exported
from the dynamic thermopack library.

# Prerequisites

Before you build (and possibly install) pycThermopack, make sure that
you have compiled thermopack using the provided Makefiles or Visual
Studio solution files. Compiling using GNU/Intel (Linux) or Intel (Windows)
FORTRAN should be straightforward. If you prefer another compiler,
you will need to tweak the get_platform_specifics method in thermo.py
to get the module exports correct. These exports are compiler
dependent.

To use pycThermopack you need to have `numpy` available. It is also recommended
to install `matploblib`.

# Installation

## Linux

To prepare pycThermopack, a `libthermopack.so` file must be copied to
the pyctp folder. This is done as follows:

```sh
# Either
make optim

# or
./makescript.py optim

# or
python makescript.py optim

# or, if Python 3 is available as python3
python3 makescript.py optim
```

One may also, optionally, install `pyctp` on a user or system level, which
allows one to import pycThermopack from anywhere. If this is desired, one can
either install at system level or at user level with
[pip](https://pypi.org/project/pip/). Use `pip3` for Python 3, as `pip` often
defaults to `pip` for Python 2.

```sh
# System level
sudo pip3 install .

# User level
pip3 install --user .
```

If you are working actively with the Thermopack code, either the Fortran
backend or the Python frontend, then it may be useful to install in editable
mode (aka develop mode). This will install a _link_ to the develop files
instead of copying the files when installing. This can be done with the `-e`
option, e.g.:

```bash
pip3 install -e --user .
```

## Windows:

Compile using Microsoft Visual Studio solution file (requires Intel Fortran
license) in `../../MSVStudio`, and `libthermopack.dll` will be copied to the
pyctp folder.

Make sure the symbol mapping is correct for the python interface
```bash
cd addon/pycThermopack
python map_platform_specifics.py
```

#### MSYS2/Mingw-W64 setup
Having compiled thermopack for MSYS2, you can get pycThermopack up and running
as follows. Open the `MSYS2 MinGW 64-bit` application, navigate to the
thermopack folder and run the following commands:

```bash
pacman -S mingw-w64-x86_64-python
pacman -S mingw-w64-x86_64-python-numpy
pacman -S mingw-w64-x86_64-python-matplotlib
cp bin/dynamic/libthermopack_optim_gfortran_MSYS.so addon/pycThermopack/pyctp/thermopack.dll
cd addon/pycThermopack
python map_platform_specifics.py
cd ../../pyExamples
python cpa.py
```

# Testing

To test pycThermopack, run one of

```sh
python3 -m pytest
```

# Thermopack GUI

To run the GUI application:

```sh
python thermopack_gui.py
```

The GUI application requires the following Python packages:

* `pyqt5` for the GUI framework
* `pint` for managing units
* `matplotlib`
* `numpy`
