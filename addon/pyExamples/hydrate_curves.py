#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
from thermopack.cubic import cubic
import numpy as np
import matplotlib.pyplot as plt
import itertools as it
import csv

cb = cubic("CO2,H2O","SRK")
cb.init_hydrate()

Z_H2O = np.array([30.0, 100.0, 250.0])*1e-6
initial_pressure = 1.0e3
minimum_temperature = 100.0
maximum_pressure = 150.0e5
p_scaling = 1.0e-6
colors = [ "black", "blue", "red", "green", "m"]
linestyles = [ "-", "--", ":", "-.", "."]

z = np.zeros((2))
for i in range(len(Z_H2O)):
    z[0] = (1-Z_H2O[i])
    z[1] = Z_H2O[i]
    fluid, water = cb.get_multi_phase_envelope_tv(initial_pressure, z,
                                                  minimum_temperature,
                                                  maximum_pressure)
    t_vals, p_vals = (fluid.t, fluid.p)
    tw_vals, pw_vals = (water.t, water.p)
    t_hyd_vals, p_hyd_vals = cb.get_hydrate_apperance_curve(initial_pressure, z,
                                                            minimum_temperature,
                                                            maximum_pressure)
    plt.plot(t_vals, p_vals*p_scaling, linestyle="-", color=colors[i])
    #plt.plot(tw_vals, pw_vals*p_scaling, linestyle=":", color=colors[i],
    #         label="H2O: {} ppm".format(round(z[-1]*1e6)))
    plt.plot(t_hyd_vals, p_hyd_vals*p_scaling, linestyle="--", color=colors[i],
             label="Hyd. H2O: {} ppm".format(round(z[-1]*1e6)))

# z = np.ones((2))*0.5
# t_hyd_vals, p_hyd_vals = cb.get_hydrate_apperance_curve(initial_pressure, z,
#                                                         minimum_temperature,
#                                                         maximum_pressure)
# plt.plot(t_hyd_vals, p_hyd_vals*p_scaling, linestyle="--", color="orange",
#          label="Hyd. excess H2O")

plt.title("SRK: CO2 and variable H2O")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.tight_layout()
plt.savefig("hydrate_co2.pdf")

plt.figure()
cb.init("CO2,N2,H2O","SRK","Classic","Classic")
cb.init_hydrate()
Z_CO2 = np.array([0.85,0.15])
Z_H2O = np.array([30.0, 100.0, 250.0])*1e-6
initial_pressure = 5.0e3

z = np.zeros((3))
for i in range(len(Z_H2O)):
    z[0:2] = (1-Z_H2O[i])*Z_CO2
    z[2] = Z_H2O[i]
    fluid, water = cb.get_multi_phase_envelope_tv(initial_pressure, z,
                                                  minimum_temperature,
                                                  maximum_pressure)
    t_vals, p_vals = (fluid.t, fluid.p)
    tw_vals, pw_vals = (water.t, water.p)
    t_hyd_vals, p_hyd_vals = cb.get_hydrate_apperance_curve(initial_pressure, z,
                                                            minimum_temperature,
                                                            maximum_pressure)

    plt.plot(t_vals, p_vals*p_scaling, linestyle="-", color=colors[i])
    # plt.plot(tw_vals, pw_vals*p_scaling, linestyle=":", color=colors[i],
    #         label="H2O: {} ppm".format(round(z[-1]*1e6)))
    plt.plot(t_hyd_vals, p_hyd_vals*p_scaling, linestyle="--", color=colors[i],
             label="Hyd. H2O: {} ppm".format(round(z[-1]*1e6)))


plt.title("SRK: CO2 (0.85), N2 (0.15) and variable H2O")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.tight_layout()
plt.savefig("hydrate_co2_n2.pdf")
plt.show()
plt.clf()
