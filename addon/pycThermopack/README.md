# Introduction

Python interface for Thermopack using ctypes (thermopack)

For examples on how to use, see
[../pyExamples/](../pyExamples/README.md). Also the function headers
of the EoS classes [../thermopack/](../thermopack), as well as the base class
`thermo.py`, will help you understand what thermopack can do.


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
the thermopack folder. This is done as follows:

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

One may also, optionally, install `thermopack` on a user or system level, which
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
thermopack folder.

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
cp bin/dynamic/libthermopack_optim_gfortran_MSYS.so addon/pycThermopack/thermopack/thermopack.dll
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

# Building thermopack wheel for pypi

After copying the dynamic library to the thermopack folder the license
files should be made available in the pycThermopack folder. To
generate the manylinux2014 wheel the `auditwheel repair` command
should be executed.

```sh
ln -s ../../LICENCE-MIT
ln -s ../../LICENCE-APACHE
python map_platform_specifics.py
python -m pip wheel .
auditwheel repair thermopack-2.0.0-py3-none-any.whl -w .
```

Note that the following Python packages `pip`, `setuptools` and
`wheel` are required to build the wheel.

# For Mac

Instead of `auditwheel ...` run
```
delocate-wheel -w fixed_wheels -v thermopack-2.0.0-py3-none-any.whl
```

# For cross-compiling on Mac for x86

Run the following from a `Rosetta` terminal (skip setting up virtualenv if already in place)
```
arch -x86_64 python3 venv venv_x86
arch -x86_64 pip install numpy matplotlib
arch -x86_64 python makescript.py optim
arch -x86_64 python map_platform_specifics.py
arch -x86_64 python -m pip wheel .
arch -x86_64 delocate-wheel -w fixed_wheels -v thermopack-2.0.0-py3-none-any.whl
```
