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

# Instanciate and init PC-SAFT (doi: 10/dt7kgb) object
pcs = pcsaft("C1,CO2")

# Use PCP-SAFT instead?
pcps = pcsaft("C1,CO2", parameter_reference="Gross2005/Gross2006", polar=True)
pcps.set_kij(1,2,-0.12) # No rigorous tuning

plt.figure()
models = [pcs, pcps]
for im, m in enumerate(models):
    lle, l1ve, l2ve = m.get_binary_pxy(223.71)
    plt.plot(l1ve.x, l1ve.p * 1.0e-6, color="b",
             label="$T$=223.71 K" if im == 0 else None,
             ls= "-" if im == 0 else "--")
    plt.plot(l1ve.y, l1ve.p * 1.0e-6, color="b",
             ls= "-" if im == 0 else "--")

    lle, l1ve, l2ve = m.get_binary_pxy(241.33)
    plt.plot(l1ve.x, l1ve.p * 1.0e-6, color="g",
             label="$T$=241.33 K" if im == 0 else None,
             ls= "-" if im == 0 else "--")
    plt.plot(l1ve.y, l1ve.p * 1.0e-6, color="g",
             ls= "-" if im == 0 else "--")

    lle, l1ve, l2ve = m.get_binary_pxy(271.48)
    plt.plot(l1ve.x, l1ve.p * 1.0e-6, color="r",
             label="$T$=271.48 K" if im == 0 else None,
             ls= "-" if im == 0 else "--")
    plt.plot(l1ve.y, l1ve.p * 1.0e-6, color="r",
             ls= "-" if im == 0 else "--")

    lle, l1ve, l2ve = m.get_binary_pxy(293.4)
    plt.plot(l1ve.x, l1ve.p * 1.0e-6, color="k",
             label="$T$=293.4 K" if im == 0 else None,
             ls= "-" if im == 0 else "--")
    plt.plot(l1ve.y, l1ve.p * 1.0e-6, color="k",
             ls= "-" if im == 0 else "--")

# Data: doi: 10.1021/ie50531a036
P = np.array([2.392480783,3.102640785,4.074801564,4.702224479,5.26069982,6.267334386,6.839599242,7.570443515,7.901391866])
x = np.array([0.0413,0.086,0.137,0.166,0.191,0.286,0.322,0.426,0.501])
y = np.array([0.404,0.521,0.605,0.629,0.652,0.676,0.686,0.68,0.672])
plt.plot(x, P, color="g", marker="o", ls="None", label="Donnelly 1954 (241.33 K)")
plt.plot(y, P, color="g", marker="o", ls="None")

# Data: doi: 10.1016/0378-3812(92)85150-7
P = np.array([5.73,6.14,6.23,6.32,6.64,6.82,7.18,7.32,7.4,7.43,7.72,7.89,7.98])
x = np.array([0.0,0.017,0.02,0.026,0.039,0.06,0.063,0.071,0.076,0.079,0.097,0.113,0.132])
y = np.array([0.0,0.04,0.048,0.068,0.086,0.094,0.116,0.122,0.126,0.128,0.136,0.136,0.132])
plt.plot(x, P, color="k", marker="o", ls="None", label="Xu 1992 (293.4 K)")
plt.plot(y, P, color="k", marker="o", ls="None")
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$x/y$")
plt.title("PC-SAFT (-) and PCP-SAFT (--) binary Pxy phase diagram for CO2 and CH4")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)

# Plot phase envelope
z = np.array([0.1,0.9])
T, P, v = pcs.get_envelope_twophase(1.0e5, z, minimum_temperature=180.0,
                                    maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = pcs.critical(z)
plt.figure()
plt.plot(T, P * 1.0e-6, color="b", label="PC-SAFT")
plt.plot([Tc], [Pc * 1.0e-6], color="b", marker="o")

# Do simplified PC-SAFT (doi: 10.1021/ie020753p)
pcs.init("C1,CO2", simplified=True)

# Plot phase envelope
T, P, v = pcs.get_envelope_twophase(1.0e5, z, minimum_temperature=180.0,
                                    maximum_pressure=1.5e7, calc_v=True)
Tc, vc, Pc = pcs.critical(z)
plt.plot(T, P * 1.0e-6, color="g", label="sPC-SAFT")
plt.plot([Tc], [Pc * 1.0e-6], color="g", marker="o")

plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.title("PC-SAFT phase diagram for CO2 and CH4")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.show()
plt.clf()
