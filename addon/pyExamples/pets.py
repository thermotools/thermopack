#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.pets import pets
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from thermopack_example_utils import calc_reduced_rho, calc_reduced_T, calc_reduced_P

# Instanciate and init PeTS object
PeTS = pets()

sigma = PeTS.sigma[0]
eps = PeTS.eps_div_kb[0]
z = np.array([1.0])
Tc, vc, Pc = PeTS.critical(z)
T_triple = 0.56*PeTS.eps_div_kb[0]

# Plot phase envelope
Psat = PeTS.bubble_pressure(T_triple, z)
T, P, v = PeTS.get_envelope_twophase(Psat[0], z, maximum_pressure=1.5e7, calc_v=True)
T_s,v_s,P_s = PeTS.spinodal(z,
                            initial_pressure=1.0e5,
                            initial_liquid_temperature=T_triple,
                            min_temperature_vapor=T_triple)

plt.figure()
plt.plot(calc_reduced_rho(1.0/v, sigma), calc_reduced_T(T ,eps), color="k")
plt.plot(calc_reduced_rho(np.array([1.0/vc]), sigma), calc_reduced_T(np.array([Tc]) ,eps), "ko")
plt.plot(calc_reduced_rho(1.0/v_s, sigma), calc_reduced_T(T_s ,eps), color="k", linestyle="--")
plt.xlabel(r"$\rho^*$")
plt.ylabel(r"$T^*$")
plt.title("PeTS density-temperature phase diagram")

plt.figure()
plt.plot(calc_reduced_T(T ,eps), calc_reduced_P(P, eps, sigma), color="k")
plt.plot(calc_reduced_T(np.array([Tc]) ,eps), calc_reduced_P(np.array([Pc]), eps, sigma), "ko")
plt.plot(calc_reduced_T(T_s ,eps), calc_reduced_P(P_s, eps, sigma), color="k", linestyle="--")
plt.ylabel(r"$P^*$")
plt.xlabel(r"$T^*$")
plt.ylim([0.0, 0.12])
plt.title("PeTS temperature-pressure phase diagram")
plt.show()
