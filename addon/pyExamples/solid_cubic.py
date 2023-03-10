#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.cubic import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init cubic object
cb = cubic("CO2,N2", "PR", "HV", "Classic")
cb.init_solid("CO2")
cb.set_pmax(5.0e9)
z = np.array([0.98,0.02])
lines, crits, triples = cb.solid_envelope_plot(1.0e5, z, calc_esv = True, maximum_pressure=1.0e9)
p_scaling = 1.0e-6
plt.figure()
for i in range(len(lines)):
    plt.plot(lines[i][:,0], lines[i][:,1]*p_scaling)
label = "Critical point"
for i in range(len(crits)):
    plt.plot(crits[i][0], crits[i][1]*p_scaling, linestyle="None",
             marker="o", color="k", label=label)
    label = None
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylim([0.0, 15.0])
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("Solid-gas-liquid phase diagram")
plt.figure()

# Plotting phase diagram for temperatures and density
n_scaling = 0.001# m3 to Liters
for i in range(len(lines)):
    densities = n_scaling/lines[i][:,4]
    plt.plot(densities,lines[i][:,0])

# Critical point
label = "Critical point"
for i in range(len(crits)):
    plt.plot(n_scaling/crits[i][4], crits[i][0], linestyle="None",
             marker="o", color="k", label=label)
    label = None

plt.ylabel(r"$T$ (K)")
plt.xlabel(r"$\rho$ (mol/L)")
plt.title("CO2,N2 solid-gas-liquid phase diagram")
leg = plt.legend(loc="best", numpoints=1, frameon=False)


# Instanciate and init cubic object
cb = cubic("CO2", "PR", "HV", "Classic")
cb.init_solid("CO2")
cb.set_pmax(5.0e9)
z = np.array([1.0])
lines, crits, triples = cb.solid_envelope_plot(1.0e5, z, calc_esv = True, maximum_pressure=1.0e9)
p_scaling = 1.0e-6
plt.figure()
for i in range(len(lines)):
    plt.plot(lines[i][:,0], lines[i][:,1]*p_scaling)
label = "Critical point"
for i in range(len(crits)):
    plt.plot(crits[i][0], crits[i][1]*p_scaling, linestyle="None",
             marker="o", color="k", label=label)
    label = None
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.ylim([0.0, 15.0])
plt.title("CO2 solid-gas-liquid phase diagram")
plt.figure()

# Plotting phase diagram for temperatures and density
n_scaling = 0.001# m3 to Liters
for i in range(len(lines)):
    densities = n_scaling/lines[i][:,4]
    plt.plot(densities,lines[i][:,0])

# Critical point
label = "Critical point"
for i in range(len(crits)):
    plt.plot(n_scaling/crits[i][4], crits[i][0], linestyle="None",
             marker="o", color="k", label=label)
    label = None

# Add triple line
plt.plot(n_scaling/np.array([lines[0][0,4], lines[4][0,4]]),
         [lines[0][-1,0], lines[0][-1,0]],
         linestyle="--",
         color="k",
         label="Triple point")

plt.ylabel(r"$T$ (K)")
plt.xlabel(r"$\rho$ (mol/L)")
plt.title("CO2 solid-gas-liquid phase diagram")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.show()
plt.clf()
