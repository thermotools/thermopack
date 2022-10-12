#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init cubic object
cb = cubic.cubic("CO2,N2", "PR", "HV", "Classic")
cb.init_solid("CO2")
z = np.array([0.98, 0.02])
lines, crits = cb.solid_envelope_plot(1.0e5, z)
p_scaling = 1.0e-6
for i in range(len(lines)):
    plt.plot(lines[i][:,0], lines[i][:,1]*p_scaling)
label = "Critical point"
for i in range(len(crits)):
    plt.plot(crits[i][0], crits[i][1]*p_scaling, linestyle="None",
             marker="o", color="k", label=label)
    label = None
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("Solid-gas-liquid phase diagram")
plt.show()
plt.clf()
