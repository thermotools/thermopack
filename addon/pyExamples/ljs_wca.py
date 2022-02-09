#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import ljs_wca
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho, calc_reduced_P

LJS = []
for i in range(4):
    # Instanciate and init LJS-WCA object
    ljs = ljs_wca.ljs_wca()
    ljs.init()
    ljs.set_tmin(temp=2.0)
    if i > 0:
        if i == 1:
            enable_a4=False
            enable_a3=True
            enable_a2=True
        elif i == 2:
            enable_a4=False
            enable_a3=False
            enable_a2=True
        elif i == 3:
            enable_a4=False
            enable_a3=False
            enable_a2=False
        ljs.model_control(enable_a2=enable_a2,enable_a3=enable_a3,enable_a4=enable_a4)
        ljs.redefine_critical_parameters()
    LJS.append(ljs)

# Get parameters for Argon
sigma, eps = LJS[0].get_sigma_eps()

colors = ["k", "b", "g", "r"]
labels = ["$a_{1-4}$", "$a_{1-3}$", "$a_{1-2}$", "$a_{1}$"]
z = np.array([1.0])

# Plot phase envelopes
for i, ljs in enumerate(LJS):
    T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
    T_s = calc_reduced_T(T, eps)
    rho_s = calc_reduced_rho(1.0/v, sigma)
    Tc, vc, Pc = ljs.critical(z)
    Tc_s = calc_reduced_T(np.array([Tc]), eps)
    rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
    plt.plot(rho_s, T_s, color=colors[i], label=labels[i])
    plt.plot(rhoc_s, Tc_s, color=colors[i], marker="o")

plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.title("LJS-WCA phase diagram")
plt.show()
plt.close()

# Plot Joule-Thompson inversion curve
for i, ljs in enumerate(LJS):
    T, P, v = ljs.joule_thompson_inversion(z)
    T_s = calc_reduced_T(T, eps)
    P_s = calc_reduced_P(P, eps, sigma)
    plt.plot(P_s, T_s, color=colors[i], label=labels[i])

plt.ylabel(r"$T^*$")
plt.xlabel(r"$P^*$")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.title("LJS-WCA Joule-Thompson inversion curves")
plt.show()
plt.close()


fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)
T_star = [0.6, 0.75, 1.0, 2.0, 4.0]
colors = ["g", "b", "r", "grey", "k"]
rho_star = np.linspace(0.0, 0.9, 50)
a1 = np.zeros_like(rho_star)
a2 = np.zeros_like(rho_star)
a3 = np.zeros_like(rho_star)
a4 = np.zeros_like(rho_star)
for j in range(len(T_star)):
    label = '$T^*=${:.2f}'.format(T_star[j])
    color = colors[j]
    for i in range(len(rho_star)):
        a1[i], a2[i], a3[i], a4[i] = ljs.get_pert_a(T_star[j], rho_star[i])
    ax1.plot(rho_star, a1, color=color, label=label)
    ax2.plot(rho_star, a2, color=color, label=label)
    ax3.plot(rho_star, a3, color=color, label=label)
    ax4.plot(rho_star, a4, color=color, label=label)

ax1.set(xlabel=r"$\rho^*$", ylabel=r"a$_1/\epsilon$")
ax2.set(xlabel=r"$\rho^*$", ylabel=r"a$_2/\epsilon^2$")
ax3.set(xlabel=r"$\rho^*$", ylabel=r"a$_3/\epsilon^3$")
ax4.set(xlabel=r"$\rho^*$", ylabel=r"a$_4/\epsilon^4$")
handles, labels = ax1.get_legend_handles_labels()
leg = fig.legend(handles, labels, bbox_to_anchor = (1.3, 0.6))
fig.tight_layout()
plt.show()
plt.close()
