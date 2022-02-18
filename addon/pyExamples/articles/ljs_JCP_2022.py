#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../../pycThermopack/')
sys.path.append('../')
# Importing pyThermopack
from pyctp import ljs_wca, ljs_bh
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho, \
    calc_reduced_P, calc_reduced_heat_capacity

# Instanciate and init LJS objects
uv = ljs_wca.ljs_uv()
wca = ljs_wca.ljs_wca()
bh = ljs_bh.ljs_bh()
LJS = []
for (ljs, label) in ((uv, "UV"), (wca, "WCA"), (bh, "BH")):
    ljs.init()
    ljs.set_tmin(temp=2.0)
    sigma, eps = ljs.get_sigma_eps()
    ljs_dict = {}
    ljs_dict["obj"] = ljs
    ljs_dict["label"] = label
    ljs_dict["sigma"] = sigma
    ljs_dict["eps"] = eps
    LJS.append(ljs_dict)

z = np.array([1.0])
colors = ["orange", "b", "g", "k", "r"]

ax1 = plt.subplot(1, 3, 1)
ax2 = plt.subplot(1, 3, 2)
ax3 = plt.subplot(1, 3, 3)

# Plot phase envelope
for i, ljs in enumerate(LJS):
    T, P, v = ljs["obj"].get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
    T_s = calc_reduced_T(T, ljs["eps"])
    rho_s = calc_reduced_rho(1.0/v, ljs["sigma"])
    P_s = calc_reduced_P(P, ljs["eps"], ljs["sigma"])
    ax1.plot(T_s, P_s, color=colors[i], label=ljs["label"])
    ax2.plot(rho_s, P_s, color=colors[i], label=ljs["label"])
    ax3.plot(rho_s, T_s, color=colors[i], label=ljs["label"])

ax1.set_ylabel(r"$P^*$")
ax1.set_xlabel(r"$T^*$")
ax1.set_ylim((0.0,0.12))
ax1.set_xlim((0.55,1.0))
ax2.set_ylabel(r"$P^*$")
ax2.set_xlabel(r"$\rho^*$")
ax2.set_ylim((0.0,0.12))
ax3.set_ylabel(r"$T^*$")
ax3.set_xlabel(r"$\rho^*$")
ax3.set_ylim((0.55,1.0))
plt.suptitle("Figure 4: Phase diagrams")
plt.show()
plt.close()

# Plot Joule-Thompson inversion curve
for i, ljs in enumerate(LJS):
    T, P, v = ljs["obj"].joule_thompson_inversion(z)
    T_s = calc_reduced_T(T, ljs["eps"])
    P_s = calc_reduced_P(P, ljs["eps"], ljs["sigma"])
    plt.plot(P_s, T_s, color=colors[i], label=ljs["label"])

plt.ylabel(r"$T^*$")
plt.xlabel(r"$P^*$")
plt.title("Figure 7: Joule-Thompson inversion curves")
plt.show()
plt.close()


n = 100
Ts = [0.7, 1.0, 1.46]
rhos = [np.linspace(1.0e-3, 0.85, n),
        np.linspace(1.0e-3, 0.9, n),
        np.linspace(1.0e-3, 1.0, n)]

ax = []
ax.append(plt.subplot(1, 3, 1))
ax.append(plt.subplot(1, 3, 2))
ax.append(plt.subplot(1, 3, 3))

# Plot reduced heat-capcity
for iT, Tsi in enumerate(Ts):
    Ti = get_real_T([Tsi], ljs["eps"])
    for i, ljs in enumerate(LJS):
        rho = calc_real_rho(rho_s[iT], ljs["sigma"])
        Cv = np.zeors_like(rho)
        for i, ljs in enumerate(LJS):
            for ir, r in enumerate(rho):
                u, Cv[ir] = ljs["obj"].internal_energy_tv(Ti, 1/r, z,
                                                          dudt=True,
                                                          property_flag="R")
            Cv_s = calc_reduced_heat_capacity(Cv)
            plt.plot(rho_s[iT], Cv_s, color=colors[i], label=ljs["label"])

plt.ylabel(r"$C_V^{\rm{res}}/Nk_{\rm{B}}$")
plt.xlabel(r"$\rho^*$")
plt.title("Figure 8a: Isochoric heat capacity")
plt.show()
plt.close()

