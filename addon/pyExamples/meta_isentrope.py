#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.tcPR import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init tcPR object. Translated and consistent cubic EoS by le Guennec et al. (10.1016/j.fluid.2016.09.003)

tc_pr = tcPR("CO2")

z = np.array([1.0])

# Initial state
T0 = 303.15
P0 = 1.0e7
s0, = tc_pr.entropy(T0,P0,z,tc_pr.LIQPH)

# Isentrope
T_iso, v_iso, P_iso = tc_pr.map_meta_isentrope(z, P0, s0, 6.0e5, n_max=50)

# Test calculation of point on isentrope
i = 15
P_meta = P_iso[-i-1]
T_meta, v_meta = tc_pr.ps_meta(s0, P_meta, z, v_iso[-i], T_iso[-i])
assert abs(T_meta - T_iso[-i-1])/T_iso[-i-1] < 1.0e-5
assert abs(v_meta - v_iso[-i-1])/v_iso[-i-1] < 1.0e-5

# Spinodal
T_spin, v_spin, P_spin = tc_pr.spinodal(z,
                                        initial_pressure=6.0e5,
                                        initial_liquid_temperature=None,
                                        dlnv=None,
                                        min_temperature_vapor=None)

# Phase envelope
T, P, v = tc_pr.get_envelope_twophase(5.0e5, z, maximum_pressure=1.5e7, calc_v=True)

plt.plot(T, P*1.0e-6, label="Saturaton curve")
plt.plot(T_iso, P_iso*1.0e-6, label="Isentrope")
plt.plot(T_spin, P_spin*1.0e-6, label="Spinodals", linestyle=":")
plt.legend(frameon=False)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.xlim([215.0,310.0])
plt.show()
plt.clf()
