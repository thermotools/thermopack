#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.cubic import cubic
from thermopack.tcPR import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init cubic object
cb = cubic("CO2,N2", "PR", "HV", "Classic")
cb.init_solid("CO2")
cb.set_pmax(5.0e9)

# Calculate some solid-phase properties
T = 200.0
press = 1.0e5
z = np.array([1.0, 0.0])
print(f"Enthalpy {cb.solid_enthalpy(T, press, z):.5f} J/mol")
print(f"Entropy {cb.solid_entropy(T, press, z):.5f} J/mol.K")
print(f"Volume {cb.solid_volume(T, press, z):.5f} m3/mol")

# Plot solid-gas-liquid phase diagram
z = np.array([0.98,0.02])
lines, crits, triples = cb.solid_envelope_plot(1.0e5, z, calc_esv = True, maximum_pressure=1.0e9)
p_scaling = 1.0e-6
plt.figure()
for i in range(len(lines)):
    plt.plot(lines[i][:, 0], lines[i][:, 1] * p_scaling)
label = "Critical point"
for i in range(len(crits)):
    plt.plot(crits[i][0], crits[i][1] * p_scaling, linestyle="None",
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
n_scaling = 0.001 # m3 to Liters
for i in range(len(lines)):
    densities = n_scaling / lines[i][:, 4]
    plt.plot(densities, lines[i][:, 0])

# Critical point
label = "Critical point"
for i in range(len(crits)):
    plt.plot(n_scaling / crits[i][4], crits[i][0], linestyle="None",
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
lines, crits, triples = cb.solid_envelope_plot(1.0e5, z, calc_esv=True, maximum_pressure=1.0e9)
p_scaling = 1.0e-6
plt.figure()
for i in range(len(lines)):
    plt.plot(lines[i][:, 0], lines[i][:, 1] * p_scaling)
label = "Critical point"
for i in range(len(crits)):
    plt.plot(crits[i][0], crits[i][1] * p_scaling, linestyle="None",
             marker="o", color="k", label=label)
    label = None
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.ylim([0.0, 15.0])
plt.title("CO2 solid-gas-liquid phase diagram")
plt.figure()

# Plotting phase diagram for temperatures and density
n_scaling = 0.001 # m3 to Liters
for i in range(len(lines)):
    densities = n_scaling / lines[i][:, 4]
    plt.plot(densities, lines[i][:, 0])

# Critical point
label = "Critical point"
for i in range(len(crits)):
    plt.plot(n_scaling / crits[i][4], crits[i][0], linestyle="None",
             marker="o", color="k", label=label)
    label = None

# Add triple line
plt.plot(n_scaling / np.array([lines[0][0, 4], lines[4][0, 4]]),
         [lines[0][-1, 0], lines[0][-1, 0]],
         linestyle="--",
         color="k",
         label="Triple point")

plt.ylabel(r"$T$ (K)")
plt.xlabel(r"$\rho$ (mol/L)")
plt.title("CO2 solid-gas-liquid phase diagram")
leg = plt.legend(loc="best", numpoints=1, frameon=False)

# Correlations for sublimation and melting curve
Tm, pm = cb.melting_pressure_correlation(1,maximum_temperature=350.0, nmax=200)
Ts, ps = cb.sublimation_pressure_correlation(1,minimum_temperature=190.0)
plt.figure()
ax = plt.gca()
ax.set_yscale('log')
ax.plot(lines[0][:, 0], lines[i][:, 1] * p_scaling, label="Saturation curve")
ax.plot(Tm, pm * p_scaling, label="Melting curve (correlation)")
ax.plot(Ts, ps * p_scaling, label="Sublimation curve (correlation)")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
ax.set_ylabel(r"$P$ (MPa)")
ax.set_xlabel(r"$T$ (K)")
plt.title("Pure CO2 solid-gas-liquid phase diagram")

# Component with low triple point pressure
cb = tcPR("IC4")
cb.set_tmin(50.0)
cb.set_pmin(1.0e-6) # Allow for low pressure solution
Tc, vc, Pc = cb.critical(z)

Tm, pm = cb.melting_pressure_correlation(1,maximum_temperature=Tc, nmax=200)
T, P = cb.get_envelope_twophase(initial_pressure=0.0, z=z, initial_temperature=Tm[0]) # Get equidistant points in temperature
plt.figure()
ax = plt.gca()
ax.set_yscale('log')
ax.plot(Tm, pm * p_scaling, label="Melting curve (correlation)")
plt.plot(T, P * p_scaling, label="Saturation curve")
plt.plot([Tc], [Pc * p_scaling], "ko")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
ax.set_ylabel(r"$P$ (MPa)")
ax.set_xlabel(r"$T$ (K)")
plt.title("Iso-Butane phase diagram")
plt.show()

