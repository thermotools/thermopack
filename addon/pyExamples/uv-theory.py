#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.uv_theory import uv_theory
import numpy as np
import matplotlib.pyplot as plt

# Instanciate and init uv_theory object. Model by van Westen and Gross (10.1063/5.0073572)

uv = uv_theory("Ar", "WCA")
uv.set_tmin(5.0)

# Plot phase envelope
z = np.array([1.0])
T, P, v = uv.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = uv.critical(z)
plt.plot(T, P*1.0e-6)
plt.plot([Tc], [Pc*1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("Argon UV-theory phase diagram")
plt.show()
plt.clf()
