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
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho, \
    calc_reduced_P

# Instanciate and init LJS-UV object
ljs = ljs_wca.ljs_uv()
ljs.init()
ljs.set_tmin(temp=2.0)

# Get parameters for Argon
sigma, eps = ljs.get_sigma_eps()

# Plot phase envelope
z = np.array([1.0])
T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = ljs.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s, color="k", label="UV")
plt.plot(rhoc_s, Tc_s, color="k", marker="o")

plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
plt.title("LJS-UV phase diagram")
plt.show()
plt.close()

# Plot Joule-Thompson inversion curve
T, P, v = ljs.joule_thompson_inversion(z)
T_s = calc_reduced_T(T, eps)
P_s = calc_reduced_P(P, eps, sigma)
plt.plot(P_s, T_s, color="k", label="UV")

plt.ylabel(r"$T^*$")
plt.xlabel(r"$P^*$")
plt.title("LJS-UV Joule-Thompson inversion curves")
plt.show()
plt.close()

