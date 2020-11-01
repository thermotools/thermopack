#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import saftvrmie
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

# Instanciate and init SAFT-VR Mie object
svrm = saftvrmie.saftvrmie()
svrm.init("H2")
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
plt.show()
plt.clf()

# Set parameters for LJ
m = 1
sigma = 1.0e-10
eps = 30.0
lambda_a = 6.0
lambda_r = 12.0
svrm.init("Ar")
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
plt.plot(rho_s, T_s)
plt.plot(rhoc_s, Tc_s, "ko")
plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
plt.title("LJ phase diagram")
plt.show()
plt.clf()
