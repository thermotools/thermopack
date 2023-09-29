# Program structure
The core of ThermoPack is the Fortran module. For more information on that, see the [GitHub page](https://github.com/thermotools/thermopack).
This page contains an introduction to the ThermoPack python-wrapper, which is an object-oriented implementation of a variety of equations of state (EoS), 
with integrated methods for computing thermodynamic properties.

Each EoS in thermopack is a class, which inherits from the `thermopack` class found in `thermo.py`. The primary documentation
for the thermopack python wrapper consists of the [docstrings of the thermopack class](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py).
This class contains all generic methods used to compute thermodynamic properties, phase equilibria, etc. The inheriting 
classes simply ensure that the correct part of the Fortran-module is linked when performing calculations, and provide 
some extended functionality for handling EoS parameters and such. See the [wiki](https://github.com/thermotools/thermopack/wiki/) 
for more information on this. 

Fluid parameters are compiled into the Fortran-module, and are not directly accessible through the Python-wrapper. 
The entire fluid parameter database used by thermopack may be found in the [`/fluids` directory](https://github.com/thermotools/thermopack/tree/main/fluids) 
in the GitHub repo. In order to model fluids not currently supported in the module available through `pip`, thermopack
must be compiled from source with the new parameters. See the [wiki](https://github.com/thermotools/thermopack/wiki/) 
for information on how to add new fluids, and the [GitHub README](https://github.com/thermotools/thermopack) for a guide 
on how to compile from source. Please feel free to leave a PR for new parameter sets such that these can be included in 
future releases of thermopack.