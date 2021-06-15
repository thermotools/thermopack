#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

cb = cubic.cubic()
cb.init("Ne,H2","SRK","Classic","Classic")
cb.set_kij(1,2,0.19)
KSTYPE, VLE, LLVE, CRIT, AZ = cb.global_binary_plot(minimum_pressure=1.0e5, minimum_temperature=2.0, include_azeotropes=True)
p_scaling = 1.0e-6
colors = [ "black", "blue", "red", "green"]
linestyles = [ "-", "--", ":", "-."]
label = "VLE"
for i in range(len(VLE)):
    plt.plot(VLE[i][:,0], VLE[i][:,1]*p_scaling, linestyle=linestyles[0],
             color=colors[0], label=label)
    label = None

label = "VLLE"
for i in range(len(LLVE)):
    plt.plot(LLVE[i][:,0], LLVE[i][:,1]*p_scaling, linestyle=linestyles[1],
             color=colors[1], label=label)
    label = None

label = "Critical"
for i in range(len(CRIT)):
    plt.plot(CRIT[i][:,0], CRIT[i][:,1]*p_scaling, linestyle=linestyles[2],
             color=colors[2], label=label)
    label = None

label = "AZ"
for i in range(len(AZ)):
    plt.plot(AZ[i][:,0], AZ[i][:,1]*p_scaling, linestyle=linestyles[3],
             color=colors[3], label=label)
    label = None

if KSTYPE == 1:
    ks_str = "I"
elif KSTYPE == 2:
    ks_str = "II"
elif KSTYPE == 3:
    ks_str = "III"
elif KSTYPE == 4:
    ks_str = "IV"
elif KSTYPE == 5:
    ks_str = "V"
else:
    ks_str = str(KSTYPE)

plt.title("van Konynenburg and Scott type: " + ks_str)
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylim([1.0e5*p_scaling, 0.3e7*p_scaling])
plt.ylabel(r"$P$ (MPa)")
plt.xlabel(r"$T$ (K)")
plt.show()
plt.clf()
