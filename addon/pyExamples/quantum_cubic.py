#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from pyctp.quantum_cubic import qcubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init Quantum-cubic object
qPR = qcubic("He,H2,Ne")
qPR.set_tmin(temp=2.0)

# Plot phase envelope
z = np.array([0.01, 0.1, 0.89])
T, P, v = qPR.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7,
                                    minimum_temperature=2.0, calc_v=True)
Tc, vc, Pc = qPR.critical(z)
plt.plot(T, P*1.0e-6)
plt.plot([Tc], [Pc*1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("Quantum-cubic phase diagram")
plt.show()
plt.clf()
