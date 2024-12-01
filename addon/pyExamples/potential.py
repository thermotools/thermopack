#!/usr/bin/python3
import matplotlib.pyplot as plt
import numpy as np
import sys
sys.path.insert(0,'../pycThermopack/')
from thermopack.saftvrqmie import saftvrqmie
from thermopack.saftvrmie import saftvrmie
import numpy as np
import matplotlib.pyplot as plt

qsvrm = saftvrqmie("H2")
T = 20.28
# Get parameters for H2
m, sigma, eps, lambda_a, lambda_r = qsvrm.get_pure_fluid_param(1)
svrm = saftvrmie("H2")
svrm.set_pure_fluid_param(1, m, sigma, eps, lambda_a, lambda_r)

r = np.linspace(0.7, 3.0, 200)*sigma
qpot, qforce = qsvrm.potential(1, 1, r, T, force=True)
pot, force = svrm.potential(1, 1, r, T, force=True)

# Plot reduced potential
plt.figure()
plt.plot(r/sigma, qpot/eps, label="FH1")
plt.plot(r/sigma, pot/eps, label="Mie")
plt.ylabel(r"$u/\epsilon$")
plt.xlabel(r"$r/\sigma$")
plt.ylim((-1.2, 5))
plt.title("Reduced potential for hydrogen")

# Plot reduced force
plt.figure()
plt.plot(r/sigma, sigma*qforce/eps, label="FH1")
plt.plot(r/sigma, sigma*force/eps, label="Mie")
plt.ylabel(r"$-\sigma u_r/\epsilon$")
plt.xlabel(r"$r/\sigma$")
plt.ylim((-5, 10))
plt.title("Reduced force for hydrogen")

plt.show()
