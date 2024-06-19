#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.ideal import ideal
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

ide = ideal("CO2,N2")
z = np.array([0.9,0.1])
T = 300.0
P = 1.0e5
v = ide.specific_volume(T, P, z, ide.VAPPH)
print(f"Ideal volume (m3/mol) {v}, {sum(z)*ide.Rgas*T/P}")

h = ide.enthalpy_tv(T, v, z)
h_id = sum([z[j]*ide.idealenthalpysingle(T, j+1) for j in range(len(z))])
print(f"Ideal enthalpy (J/mol) {h}, {h_id}")
