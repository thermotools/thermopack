#!/usr/bin/python3
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import multiparameter
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init multiparameter object
nist = multiparameter.multiparam()
nist.init("C3", "NIST_MEOS")

# Plot phase envelope
z = np.array([1.0])
T, P, v = nist.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = nist.critical(z)
plt.plot(1.0/v, T)
plt.plot([1.0/vc], [Tc], "ko")
plt.xlabel(r"$\rho$ (mol/m$^3$)")
plt.ylabel(r"$T$ (K)")
plt.title("NIST phase diagram for propane")
plt.show()
plt.clf()
