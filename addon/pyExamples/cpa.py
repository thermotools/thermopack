#!/usr/bin/python3
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import cpa
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init PeTS object
cpa_srk = cpa.cpa()
cpa_srk.init("CO2,H2O")

fig, (ax, ax2) = plt.subplots(1, 2, sharey=True, facecolor='w')
ax.set_xlim(0, 0.04)
ax2.set_xlim(0.97, 1.0)
ax.spines['right'].set_visible(False)
ax2.spines['left'].set_visible(False)
ax.yaxis.tick_left()
ax2.yaxis.tick_right()
ax2.yaxis.set_label_position("right")
ax2.tick_params(labelright='off')

# Pxy phase envelope
p_scaling = 1.0e-6
LLE, L1VE, L2VE = cpa_srk.get_binary_pxy(298.1)
if LLE[0] is not None:
    ax.plot(LLE[0], LLE[2]*p_scaling)
    ax.plot(LLE[1], LLE[2]*p_scaling)
    ax2.plot(LLE[0], LLE[2]*p_scaling)
    ax2.plot(LLE[1], LLE[2]*p_scaling)
if L1VE[0] is not None:
    ax.plot(L1VE[0], L1VE[2]*p_scaling)
    ax.plot(L1VE[1], L1VE[2]*p_scaling)
    ax2.plot(L1VE[0], L1VE[2]*p_scaling)
    ax2.plot(L1VE[1], L1VE[2]*p_scaling)
if L2VE[0] is not None:
    ax.plot(L2VE[0], L2VE[2]*p_scaling)
    ax.plot(L2VE[1], L2VE[2]*p_scaling)
    ax2.plot(L2VE[0], L2VE[2]*p_scaling)
    ax2.plot(L2VE[1], L2VE[2]*p_scaling)

d = .015
kwargs = dict(transform=ax.transAxes, color='k', clip_on=False)
ax.plot((1-d, 1+d), (-d, +d), **kwargs)
ax.plot((1-d, 1+d), (1-d, 1+d), **kwargs)

kwargs.update(transform=ax2.transAxes)  # switch to the bottom axes
ax2.plot((-d, +d), (1-d, 1+d), **kwargs)
ax2.plot((-d, +d), (-d, +d), **kwargs)

ax.set_ylabel(r"$P$ (MPa)")
fig.text(0.5, 0.02, r"$x/y$ CO$_2$ (-)", ha='center')
fig.suptitle("CO$_2$-H$_2$O binary phase diagram")

plt.show()
plt.clf()
