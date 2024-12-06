#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
from thermopack.cubic import cubic
import numpy as np
import matplotlib.pyplot as plt
import itertools as it
import csv

cb = cubic("CO2,N2,H2O","SRK")
Z_CO2 = np.array([0.85,0.15])
Z_H2O = np.array([0.05,1e-2,1e-3,1e-4])
Z_H2O = np.array([5.0, 30.0, 100.0, 1000.0])*1e-6
initial_pressure = 5.0e3
minimum_temperature = 140.0
maximum_pressure = 150.0e5
p_scaling = 1.0e-6
colors = [ "black", "blue", "red", "green"]
linestyles = [ "-", "--", ":", "-."]

z = np.zeros((3))
for i in range(len(Z_H2O)):
    z[0:2] = (1-Z_H2O[i])*Z_CO2
    z[2] = Z_H2O[i]
    fluid, water = cb.get_multi_phase_envelope_tv(initial_pressure, z,
                                                  minimum_temperature,
                                                  maximum_pressure)
    t_vals, p_vals = (fluid.t, fluid.p)
    tw_vals, pw_vals = (water.t, water.p)
    plt.plot(t_vals, p_vals*p_scaling, linestyle="-", color=colors[i])
    plt.plot(tw_vals, pw_vals*p_scaling, linestyle="--", color=colors[i],
             label="H2O: {} ppm".format(round(z[-1]*1e6)))

cb.init("CO2,N2","SRK","Classic","Classic")
Z_CO2 = np.array([0.85,0.15])
T, P, v = cb.get_envelope_twophase_tv(initial_pressure, Z_CO2,
                                      maximum_pressure=maximum_pressure,
                                      minimum_temperature=minimum_temperature)
plt.plot(T, P*p_scaling, linestyle="-", color="cyan", label="No water")

plt.title("SRK: CO2 (0.85), N2 (0.15) and variable H2O")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")

plt.figure()
cb.init("CO2,H2O","SRK","Classic","Classic")
initial_pressure = 1.0e3
minimum_temperature = 100.0
maximum_pressure = 150.0e5
p_scaling = 1.0e-6
colors = [ "black", "blue", "red", "green"]
linestyles = [ "-", "--", ":", "-."]

arrays = []
z = np.zeros((2))
for i in range(len(Z_H2O)):
    z[0] = (1-Z_H2O[i])
    z[1] = Z_H2O[i]
    fluid, water = cb.get_multi_phase_envelope_tv(initial_pressure, z,
                                                  minimum_temperature,
                                                  maximum_pressure)
    t_vals, p_vals = (fluid.t, fluid.p)
    tw_vals, pw_vals = (water.t, water.p)
    arrays += [t_vals, p_vals, tw_vals, pw_vals]
    plt.plot(t_vals, p_vals*p_scaling, linestyle="-", color=colors[i])
    plt.plot(tw_vals, pw_vals*p_scaling, linestyle="--", color=colors[i],
             label="H2O: {} ppm".format(round(z[-1]*1e6)))

# with open('co2_h2o.csv', 'w') as f:
#     csv.writer(f, delimiter='\t').writerows(it.zip_longest(*arrays, fillvalue =np.NaN))

cb.init("CO2","SRK","Classic","Classic")
Z_CO2 = np.array([1.0])
T, P, v = cb.get_envelope_twophase_tv(initial_pressure, Z_CO2,
                                       maximum_pressure=maximum_pressure,
                                       minimum_temperature=minimum_temperature)
plt.plot(T, P*p_scaling, linestyle="-", color="cyan", label="No water")

plt.title("SRK: CO2 and variable H2O")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")

plt.show()
