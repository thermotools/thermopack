#!/usr/bin/python3
import matplotlib.pyplot as plt
import numpy as np
import sys
sys.path.insert(0,'../pycThermopack/')
from thermopack.saftvrqmie import saftvrqmie
import numpy as np
import matplotlib.pyplot as plt

# Instantiate and init SAFT-VRQ Mie object
qSAFT = saftvrqmie("He,H2,Ne")
qSAFT.set_tmin(temp=2.0)

# Plot phase envelope
z = np.array([0.01, 0.89, 0.1])
T, P, v = qSAFT.get_envelope_twophase(5.0e4, z, maximum_pressure=2.0e6, calc_v=True)
Tc, vc, Pc = qSAFT.critical(z)
plt.plot(T, P * 1.0e-6)
plt.plot([Tc], [Pc * 1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("SAFT-VRQ Mie phase diagram")
plt.show()
plt.clf()
