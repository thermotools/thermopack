#!/usr/bin/python3
# Modify system path
import matplotlib.pyplot as plt
import numpy as np
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init tcPR object. Translated and consistent cubic EoS by le Guennec et al. (10.1016/j.fluid.2016.09.003)

tc_pr = tcPR.tcPR()
tc_pr.init("CO2,N2")

# Plot phase envelope
z = np.array([0.9,0.1])
T, P, v = tc_pr.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = tc_pr.critical(z)
plt.plot(T, P*1.0e-6)
plt.plot([Tc], [Pc*1.0e-6], "ko")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("tcPR phase diagram")
plt.show()
plt.clf()
