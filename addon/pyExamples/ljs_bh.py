#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import ljs_bh
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Avogadros number
NA = 6.02214076e23

def calc_reduced_T(Ta, eps):
    """ Calculate reduced temperature
    """
    Tstar = np.zeros_like(Ta)
    Tstar = Ta/eps
    return Tstar

def calc_reduced_rho(rhoa, sigma):
    """ Calculate reduced density
    """
    rhoStar = np.zeros_like(rhoa)
    rhoStar = sigma**3*NA*rhoa
    return rhoStar

# Instanciate and init LJS-BH object
ljs = ljs_bh.ljs_bh()
ljs.init()
ljs.set_tmin(temp=2.0)

# Get parameters for Argon
sigma, eps = ljs.get_sigma_eps()

# Plot phase envelope using a1-a3
z = np.array([1.0])
T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = ljs.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s, color="k", label="$a_1+a_2+a_3$")
plt.plot(rhoc_s, Tc_s, color="k", marker="o")

# Disable a_2
ljs.model_control(enable_a3=False)
ljs.redefine_critical_parameters()
T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = ljs.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s, color="b", label="$a_1+a_2$")
plt.plot(rhoc_s, Tc_s, color="b", marker="o")

# Disable a_2 and a_3
ljs.model_control(enable_a2=False, enable_a3=False)
ljs.redefine_critical_parameters()
T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = ljs.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s, color="g", label="$a_1$")
plt.plot(rhoc_s, Tc_s, color="g", marker="o")

plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.title("LJS-BH phase diagram")
plt.ylim([0.45, 1.05])
plt.show()
plt.close()


fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)
T_star = [0.6, 0.75, 1.0, 2.0, 4.0]
colors = ["g", "b", "r", "grey", "k"]
rho_star = np.linspace(0.0, 0.9, 50)
a1 = np.zeros_like(rho_star)
a2 = np.zeros_like(rho_star)
a3 = np.zeros_like(rho_star)
for j in range(len(T_star)):
    label = '$T^*=${:.2f}'.format(T_star[j])
    color = colors[j]
    for i in range(len(rho_star)):
        a1[i], a2[i], a3[i] = ljs.get_pert_a(T_star[j], rho_star[i])
    ax1.plot(rho_star, a1, color=color, label=label)
    ax2.plot(rho_star, a2, color=color, label=label)
    ax3.plot(rho_star, a3, color=color, label=label)

ax1.set(xlabel=r"$\rho^*$", ylabel=r"a$_1/\epsilon$")
ax2.set(xlabel=r"$\rho^*$", ylabel=r"a$_2/\epsilon^2$")
ax3.set(xlabel=r"$\rho^*$", ylabel=r"a$_3/\epsilon^3$")
handles, labels = ax1.get_legend_handles_labels()
ax4.legend(handles, labels, loc='center')
ax4.axis('off')
plt.tight_layout()
plt.show()
plt.close()
