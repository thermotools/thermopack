#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.cubic import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

srk = cubic("N2,NO", "SRK")
lle, l1ve, l2ve = srk.get_binary_txy(1.0e6, minimum_temperature=80.0)
plt.figure()
plt.plot(l1ve.x, l1ve.T, color="b",
         label="$P$=1.0 MPa", ls= "-")
plt.plot(l1ve.y, l1ve.T, color="b", ls="-")
plt.plot([l1ve.x[0], l1ve.y[0]], [l1ve.T[0]] * 2, color="k", ls="--")
plt.plot(l2ve.x, l2ve.T, color="b", ls="-")
plt.plot(l2ve.y, l2ve.T, color="b", ls="-")
plt.plot(lle.x1, lle.T, color="g", ls="-")
plt.plot(lle.x2, lle.T, color="g", ls="-")
plt.xlabel(r"$x_{\rm{N}_2}/y_{\rm{N}_2}$")
plt.ylabel(r"$T (K)$")
plt.title(r"$\rm{N}_2 - \rm{NO}$ at P=2.6 MPa")

srk2 = cubic("N2,C2", "SRK")
lle, l1ve, l2ve = srk2.get_binary_txy(2.0e6, minimum_temperature=100.0)
plt.figure()
plt.plot(l1ve.x, l1ve.T, color="b",
         label="$P$=2.0 MPa", ls= "-")
plt.plot(l1ve.y, l1ve.T, color="b", ls="-")
plt.plot([l1ve.x[0], l1ve.y[0]], [l1ve.T[0]] * 2, color="k", ls="--")
plt.plot(l2ve.x, l2ve.T, color="b", ls="-")
plt.plot(l2ve.y, l2ve.T, color="b", ls="-")
plt.plot(lle.x1, lle.T, color="g", ls="-")
plt.plot(lle.x2, lle.T, color="g", ls="-")
plt.xlabel(r"$x_{\rm{N}_2}/y_{\rm{N}_2}$")
plt.ylabel(r"$T (K)$")
plt.title(r"$\rm{N}_2 - \rm{C}_2\rm{H}_6$ at P=2.0 MPa")


srk3 = cubic("BENZENE,TOLU", "SRK")
lle, l1ve, l2ve = srk3.get_binary_txy(1.01325e5, minimum_temperature=200.0)
plt.figure()
plt.plot(l1ve.x, l1ve.T, color="b",
         label="$P$1 Atm", ls= "-")
plt.plot(l1ve.y, l1ve.T, color="b", ls="-")
plt.xlabel(r"$x_{\rm{Benzene}}/y_{\rm{Benzene}}$")
plt.ylabel(r"$T (K)$")
plt.title(r"Benzene - Toluene at P=1 atm")

plt.show()

