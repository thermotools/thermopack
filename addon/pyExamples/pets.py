#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import pets
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init PeTS object
PeTS = pets.pets()
PeTS.init()
PeTS.set_tmin(temp=5.0)

# Plot phase envelope
z = np.array([1.0])
T, P, v = PeTS.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = PeTS.critical(z)
plt.plot(1.0/v, T)
plt.plot([1.0/vc], [Tc], "ko")
plt.xlabel(r"$\rho$ (mol/m$^3$)")
plt.ylabel(r"$T$ (K)")
plt.title("PeTS phase diagram")
plt.show()
plt.clf()
