#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Exapmle of virial coefficient plot

tc_pr = tcPR.tcPR("CO2,C1")

# Plot phase envelope
z = np.array([0.9, 0.1])

# Mixture virial_coeffcients
Bmix, Cmix = tc_pr.virial_coeffcients(300.0, z)
print("Bmix, Cmix: ", Bmix, Cmix)

# Second virial matrix
B = tc_pr.second_virial_matrix(300.0)
print("B matrix: ", B)
print("Bmix, Sum Sum z_iz_jB_ij", Bmix, np.matmul(np.matmul(B, z), z))

# Third virial matrix
C = tc_pr.binary_third_virial_matrix(300.0)
print("C matrix: ", C)

T = np.linspace(150.0, 800.0, 300)
B1 = np.zeros_like(T)
B2 = np.zeros_like(T)
B12 = np.zeros_like(T)

for i in range(len(T)):
    B = tc_pr.second_virial_matrix(T[i])
    B1[i] = B[0, 0]
    B2[i] = B[1, 1]
    B12[i] = B[0, 1]

scaling = 1.0e6
plt.plot(T, B1*scaling, label="$B_{11}$")
plt.plot(T, B2*scaling, label="$B_{22}$")
plt.plot(T, B12*scaling, label="$B_{12}$")
plt.ylabel(r"$B$ (cm$^3$/mol)")
plt.xlabel(r"$T$ (K)")
leg = plt.legend(loc="best", numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.title("Second binary")
plt.show()
plt.clf()
