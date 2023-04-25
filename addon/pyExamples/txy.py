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
LLE, L1VE, L2VE = srk.get_binary_txy(1.0e6,minimum_temperaturte=80.0)
plt.figure()
plt.plot(L1VE[0], L1VE[2], color="b",
         label="$P$=2.6 MPa", ls= "-")
plt.plot(L1VE[1], L1VE[2], color="b", ls= "-")
plt.plot([L1VE[1][0],L1VE[0][0]], [L1VE[2][0]]*2, color="k", ls= "--")
plt.plot(L2VE[0], L2VE[2], color="b", ls= "-")
plt.plot(L2VE[1], L2VE[2], color="b", ls= "-")
plt.plot(LLE[0], LLE[2], color="g", ls= "-")
plt.plot(LLE[1], LLE[2], color="g", ls= "-")
plt.xlabel(r"$x_{\rm{N}_2}/y_{\rm{N}_2}$")
plt.ylabel(r"$T (K)$")
plt.title(r"$\rm{N}_2 - \rm{NO}$ at P=2.6 MPa")

srk2 = cubic("N2,C2", "SRK")
LLE, L1VE, L2VE = srk2.get_binary_txy(2.0e6,minimum_temperaturte=100.0)
plt.figure()
plt.plot(L1VE[0], L1VE[2], color="b",
         label="$P$=2.0 MPa", ls= "-")
plt.plot(L1VE[1], L1VE[2], color="b", ls= "-")
plt.plot([L1VE[1][0],L1VE[0][0]], [L1VE[2][0]]*2, color="k", ls= "--")
plt.plot(L2VE[0], L2VE[2], color="b", ls= "-")
plt.plot(L2VE[1], L2VE[2], color="b", ls= "-")
plt.plot(LLE[0], LLE[2], color="g", ls= "-")
plt.plot(LLE[1], LLE[2], color="g", ls= "-")
plt.xlabel(r"$x_{\rm{N}_2}/y_{\rm{N}_2}$")
plt.ylabel(r"$T (K)$")
plt.title(r"$\rm{N}_2 - \rm{C}_2\rm{H}_6$ at P=2.0 MPa")


srk3 = cubic("BENZENE,TOLU", "SRK")
LLE, L1VE, L2VE = srk3.get_binary_txy(1.01325e5,minimum_temperaturte=200.0)
plt.figure()
plt.plot(L1VE[0], L1VE[2], color="b",
         label="$P$=2.0 MPa", ls= "-")
plt.plot(L1VE[1], L1VE[2], color="b", ls= "-")
plt.xlabel(r"$x_{\rm{Benzene}}/y_{\rm{Benzene}}$")
plt.ylabel(r"$T (K)$")
plt.title(r"Benzene - Toluene at P=1 atm")

plt.show()

