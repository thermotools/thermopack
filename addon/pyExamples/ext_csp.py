#!/usr/bin/python3
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import extended_csp
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init extended corrensponding state object
csp = extended_csp.ext_csp()
csp.init("C1,C2,C3,NC4", "SRK", "Classic", "vdW", "NIST_MEOS", "C3")

# Plot phase envelope
z = np.array([0.86, 0.1, 0.03, 0.01])
T, P = csp.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7)
Tc, vc, Pc = csp.critical(z)
plt.plot(T, P*1.0e-6)
plt.plot([Tc], [Pc*1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("Extended corrensponding state phase diagram")
plt.show()
plt.clf()
