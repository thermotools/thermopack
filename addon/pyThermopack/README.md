# Introduction

pyThermopack: Python interface for Thermopack

For examples on how to use, see `example.py`. Some nice functions for making
quick plots:

Functions for quick plots:
- `pytp.demo.test_tpflash_twophase()`
- `pytp.demo.test_density()`

# Prerequisites

Before you build (and possibly install) pyThermopack, make sure that you have
compiler Thermopack with `gfortran`.

To build pyThermopack one must have `f2py` available. This ships with `numpy`,
which you'll need anyway. Here are some notable `f2py` resources:
* `hg clone https://f2py.googlecode.com/hg/ f2py `
* http://www.f2py.com/
* http://code.google.com/p/f2py/
* http://cens.ioc.ee/projects/f2py2e/FAQ.html#q-how-to-use-f2py-under-windows

# Installation

To build pyThermopack, one can do

```sh
# Either
./makescript.py optim

# or (to specify the python version)
python makescript.py optim
```

One may also, optionally, install `pytp` on a user or system level, which
allwos one to import pyThermopack from anywhere. If this is desired, one can
do:

```sh
# System level
sudo python install

# User level
python install
```

## Windows/MinGW:

Things should work if one installs Enthought Canopy (Python-distribution) (32
bit). Note that this has not yet been properly tested.

# Testing

To test pyThermopack, run one of

```sh
python example.py
pylint -rn pytp
```
