# Introduction

pycThermopack: Python interface for Thermopack

For examples on how to use, see `example.py`.

# Prerequisites

Before you build (and possibly install) pycThermopack, make sure that
you have compiled thermopack using the provides Makefiles or Visual
Studio solution files. Compiling using GNU (Linux) or Intel (Windows)
FORTRAN should be straight forward. If you prefer another compiler,
you will need to tweake the get_platform_specifics method in thermo.py
to get the module exports correct. These exports are compiler
dependent.

To use pycThermopack you need have `numpy` available.

Currently only python3 on Linux and Windows have been tested.

# Installation

##Linux

To prepare pycThermopack, a libthermopack.so file must be copied to
the pyctp folder. This is done as follows:

```sh
# Either
./makescript.py optim

# or
python makescript.py optim

# or (to specify the python version)
make optim
```

One may also, optionally, install `pyctp` on a user or system level, which
allwos one to import pycThermopack from anywhere. If this is desired, one can
do:

```sh
### System level
sudo python3 install

### User level
python install
```

## Windows:

.....


# Testing

To test pycThermopack, run one of

```sh
python3 example.py
python3 -m pytest
```
