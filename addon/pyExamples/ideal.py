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

air = ideal("N2,O2,AR")
z = np.array([0.78,0.21,0.01])
T = 298.15
P = 1.01235e5
v = air.specific_volume(T, P, z, air.VAPPH)
print(f"Ideal volume (m3/mol) {v}, {sum(z)*air.Rgas*T/P}")

h = air.enthalpy_tv(T, v, z)
h_id = sum([z[j]*air.idealenthalpysingle(T, j+1) for j in range(len(z))])
print(f"Ideal enthalpy (J/mol) {h}, {h_id}")

c = air.speed_of_sound_tv(T, v, z)
print(f"Speed of sound (m/s) {c}")

