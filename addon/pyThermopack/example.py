#!/usr/bin/env python
"""Simple example on how to use pytp."""

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

from pytp import utils, tp


# Show documentation for interface:
utils.showdoc()

# Initialize thermopack
tp.init("Thermopack", "SRK", "Classic", "Classic", 2, "CO2,N2", 2, 1, 1)

# Setting feed composition:
z = np.array([0.9, 0.1])

# Setting up T, P grid
ngrid = 200
Tvals = np.linspace(250, 320, ngrid)
pvals = np.linspace(10, 100, ngrid)*1e5

# Do TP-flashes
print("---Doing %s TP-flashes" % (ngrid**2))
phasegrid = np.zeros((ngrid, ngrid))
betagrid = np.zeros((ngrid, ngrid))
beta_guess = 0.5

for i in range(ngrid):
    for j in range(ngrid):
        flashresult = tp.tpflash_twophase(Tvals[i], pvals[j], z, beta_guess)
        beta, betal, phase, x, y = flashresult
        phasegrid[j, i] = phase
        betagrid[j, i] = beta
print("Done. Making plot...")

# Example plot: Get a grid which is beta in the two-phase area, and -1 outside
betatwophase_grid = np.ones((ngrid, ngrid))*(-1)
betatwophase_grid[phasegrid == 0] = betagrid[phasegrid == 0]

# Make T, P grid
Tgrid, pgrid = np.meshgrid(Tvals, pvals)

# Make plot
plt.contourf(Tgrid, pgrid*1e-5, betatwophase_grid,
             levels=np.linspace(0.0, 1.0, 9))
plt.colorbar()
plt.xlabel(r"$T$ (K)")
plt.ylabel(r"$P$ (bar)")
plt.title(r"$\beta$")
plt.savefig("example.pdf")
plt.show()
