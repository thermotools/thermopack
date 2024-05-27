#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.saftvrmie import saftvrmie
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from thermopack_example_utils import NA, calc_reduced_T, calc_reduced_rho

# Instantiate and init SAFT-VR Mie object
svrm = saftvrmie("H2")
svrm.set_tmin(temp=2.0)

# Get parameters for H2
m, sigma, eps, lambda_a, lambda_r = svrm.get_pure_fluid_param(1)

# Plot phase envelope
z = np.array([1.0])
T, P, v = svrm.get_envelope_twophase(1.0e3, z, maximum_pressure=1.5e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = svrm.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.plot(rho_s, T_s)
plt.plot(rhoc_s, Tc_s, "ko")
plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
plt.title("SAFT-VR Mie phase diagram")

# Set parameters for LJ
m = 1
sigma = 3.0e-10
eps = 30.0
lambda_a = 6.0
lambda_r = 12.0
svrm.init("Ar")  # Re-initialize with new component
svrm.set_tmin(temp=2.0)
svrm.set_pure_fluid_param(1, m, sigma, eps, lambda_a, lambda_r)
svrm.redefine_critical_parameters(False)

# Plot phase envelope
z = np.array([1.0])
T, P, v = svrm.get_envelope_twophase(1.0e3, z, maximum_pressure=1.5e7, calc_v=True)
T_s = calc_reduced_T(T, eps)
rho_s = calc_reduced_rho(1.0/v, sigma)
Tc, vc, Pc = svrm.critical(z)
Tc_s = calc_reduced_T(np.array([Tc]), eps)
rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
plt.figure()
plt.plot(rho_s, T_s)
plt.plot(rhoc_s, Tc_s, "ko")
plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
plt.title("LJ phase diagram")

# Show plots
plt.show()
