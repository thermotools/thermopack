#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.cpa import cpa
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt


p_scaling = 1.0e-3 # Pa -> kPa

# Pemberton and Mash (1978). DOI: 10.1016/0021-9614(78)90160-X
T_exp = 303.15 # K
x_exp = np.array([0.00435, 0.01524, 0.02727, 0.04633, 0.06783, 0.10991,
                  0.17111, 0.24688, 0.32385, 0.38655, 0.41758, 0.50492,
                  0.58087, 0.63434, 0.72455, 0.80840, 0.85785, 0.89064,
                  0.89934, 0.92444, 0.95370, 0.97315, 0.98153])
y_exp = np.array([0.0412, 0.1280, 0.2043, 0.2975, 0.3753, 0.4743, 0.5479,
                  0.5907, 0.6194, 0.6406, 0.6508, 0.6797, 0.7087, 0.7329,
                  0.7810, 0.8337, 0.8705, 0.8979, 0.9056, 0.9284, 0.9556,
                  0.9739, 0.9819])
p_exp = np.array([4.413, 4.803, 5.203, 5.781, 6.386, 7.329, 8.189, 8.723,
                  9.085, 9.303, 9.403, 9.663, 9.869, 9.999, 10.199, 10.341,
                  10.394, 10.427, 10.435, 10.445, 10.457, 10.467, 10.473]) # kPa

# Yang, Demirgian, Solsky, Kikta and Marinsky (1979). DOI: 10.1021/j100484a013
T_Yang = 298.15 # K
x_weight_ethanol = np.linspace(0.1, 0.9, 9)
p_tot = np.array([31.03, 38.15, 43.48, 47.54, 49.95, 51.72, 53.42, 55.80, 57.80]) # mmHg
p_ethanol = np.array([8.43, 16.88, 23.52, 28.73, 32.38, 35.33, 37.89, 42.56, 48.78]) # mmHg

# Mixture
cpa_srk = cpa("ETOH,H2O", parameter_reference="Queimada2005")
# Rough visual tuning
kij_a=-0.08
kij_eps=0.015
cpa_srk.set_kij(1, 2, kij_a=kij_a, kij_eps=kij_eps)
cpa_srk.set_kij(2, 1, kij_a=kij_a, kij_eps=kij_eps)
# Molecular weights
mws = [cpa_srk.compmoleweight(i+1)*1e-3 for i in range(2)] # kg/mol

# Pxy phase envelope
lle, l1ve, l2ve = cpa_srk.get_binary_pxy(T_exp)

# Plotting Liquid - Vapour equilibria
fig, ax = plt.subplots()
ax.plot(l1ve.x, l1ve.p * p_scaling, color="k")
ax.plot(l1ve.y, l1ve.p * p_scaling, color="k")
ax.plot(x_exp, p_exp, color="k", marker="o", ls="None", label="Pemberton and Mash (1978)")
ax.plot(y_exp, p_exp, color="k", marker="o", ls="None")

ax.set_ylabel(r"$P$ (kPa)")
ax.set_xlabel(r"$x/y$ Ethanol (-)")
ax.legend(loc="best", frameon=False)
fig.suptitle(u"Ethanol-Water at 30\N{DEGREE SIGN}C")

# Pxy phase envelope
lle, l1ve, l2ve = cpa_srk.get_binary_pxy(T_Yang)

# Convert to mass fraction
xw = l1ve.x*mws[0]/(l1ve.x*mws[0] + (1-l1ve.x)*mws[1])

# Plotting Liquid - Vapour equilibria
p_scaling = 1.0/133.3223684
fig, ax = plt.subplots()
ax.plot(xw, l1ve.p * p_scaling, color="k", label="Total")
ax.plot(xw, l1ve.y*l1ve.p * p_scaling, color="k", label="Ethanol", ls=":")
ax.plot(xw, (1-l1ve.y)*l1ve.p * p_scaling, color="k", label="Water", ls="--")
ax.plot(x_weight_ethanol, p_tot, color="k", marker="s", ls="None", markerfacecolor='none')
ax.plot(x_weight_ethanol, p_ethanol, color="k", marker="<", ls="None", markerfacecolor='none')
ax.plot(x_weight_ethanol, p_tot-p_ethanol, color="k", marker="o", ls="None", markerfacecolor='none')

ax.set_ylabel(r"$P$ (mmHg)")
ax.set_xlabel(r"$x$ Ethanol (-)")
ax.legend(loc="best", frameon=False)
fig.suptitle(u"Ethanol-Water at 25\N{DEGREE SIGN}C. Data from Yang et al. (1979)")

plt.show()
