#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.pcsaft import pcsaft
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init PC-SAFT object
pcs = pcsaft("CO2,C1")

# Plot phase envelope
z = np.array([0.9,0.1])
T, P, v = pcs.get_envelope_twophase(1.0e5, z, minimum_temperature=180.0,
                                    maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = pcs.critical(z)
plt.plot(T, P*1.0e-6)
plt.plot([Tc], [Pc*1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("PC-SAFT phase diagram")
plt.show()
plt.clf()
