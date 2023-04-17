#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.uv_theory import uv_theory
import numpy as np
import matplotlib.pyplot as plt

# Instantiate and init uv_theory object. Model by van Westen and Gross (10.1063/5.0073572)

def plot_BH_WCA_comparison(ax, comp, pert, paramref, kw):
    uv = uv_theory(comp, pert, paramref)
    uv.set_tmin(5.0)

    if paramref == "SUTHERLAND":
        print ("set params")
        uv.set_params(ic=1,jc=1,nt=2, \
                      c_vec=[3.8847749838093475, -3.8847749838093475], \
                      lam_vec=[12.26, 6.0], \
                      sigma=3.41e-10, \
                      epsdivk=118.7, \
                      beta_expo=None)

    # Plot phase envelope
    z = np.array([1.0])
    T, P, v = uv.get_envelope_twophase(1.0e4, z, maximum_pressure=1.5e7, calc_v=True)
    Tc, vc, Pc = uv.critical(z)
    ax.plot(1/v, P*1.0e-6, **kw)
    ax.plot([1/vc], [Pc*1.0e-6], "ko")

ax = plt.gca()
plot_BH_WCA_comparison(ax, "Ar", "WCA", "SVRMIE", kw={'label':"UV-WCA", 'ls':'-'})
plot_BH_WCA_comparison(ax, "Ar", "BH", "SVRMIE", kw={'label':"UV-BH", 'ls':'--'})

plot_BH_WCA_comparison(ax, "Ar", "BH", "SUTHERLAND", kw={'label':"UV-LAFITTE", 'ls':'-.'})

ax.set_ylabel(r"$P$ (MPa)")
ax.set_xlabel(r"$\rho$ (mol/m3)")
ax.set_title("Argon UV-theory phase diagram")
ax.set_xlim(70,35000)
ax.set_ylim(0,6)
ax.grid()
plt.legend()
plt.savefig("AR_COMPARISON.pdf")
plt.show()
