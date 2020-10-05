# Introduction

pycThermopack: Python interface for Thermopack

For examples on how to use, see `example.py`, and `../pyExamples/`

Note that the files **../../libthermopack_export.version** (Linux) and **../../MSVStudio/thermopack.def** (Windows) define what symbols are exported from the dynamic thermopack library.

# Prerequisites

Before you build (and possibly install) pycThermopack, make sure that
you have compiled thermopack using the provides Makefiles or Visual
Studio solution files. Compiling using GNU/Intel (Linux) or Intel (Windows)
FORTRAN should be straight forward. If you prefer another compiler,
you will need to tweake the get_platform_specifics method in thermo.py
to get the module exports correct. These exports are compiler
dependent.

To use pycThermopack you need have `numpy` available.

# Installation

##Linux

To prepare pycThermopack, a libthermopack.so file must be copied to
the pyctp folder. This is done as follows:

```sh
# Either
make optim

# or
./makescript.py optim

# or
python makescript.py optim
```

One may also, optionally, install `pyctp` on a user or system level, which
allwos one to import pycThermopack from anywhere. If this is desired, one can
do:

```sh
# System level
## Either
sudo ./install

## or (to specify the python version)
sudo python setup.py install

# User level
python setup.py install
```

## Windows:

Compile using Microsoft Visual Studio solution file (requires Intel Fortran license) in ../../MSVStudio, and a libthermopack.dll will be copied to the pyctp folder.


# Testing

To test pycThermopack, run one of

```sh
python3 example.py
python3 -m pytest
```

# Thermopack GUI

To run the GUI application:

```sh
python thermopack_gui.py
```

You will need to have installed the following Python packages:

* `pyqt5` for the GUI framework
* `pint` for managing units
* `matplotlib`
* `numpy`
