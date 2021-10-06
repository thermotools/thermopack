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
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho

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

# Disable a_4
ljs.model_control(use_temperature_dependent_u_fraction=True)
ljs.redefine_critical_parameters()
T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = ljs.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s, color="b", label="UV (Temp. dep. frac.)")
plt.plot(rhoc_s, Tc_s, color="b", marker="o")

plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.title("LJS-UV phase diagram")
plt.show()
plt.close()
