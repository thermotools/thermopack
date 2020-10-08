#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Instanciate and init SRK object.
srk = cubic.cubic()
srk.init("C1,C2,C3", "SRK")
srk.set_tmin(50.0)

print("Rgas = {} (J/K/mol)".format(srk.Rgas))
mw = np.zeros((srk.nc))
for i in(range(len(mw))):
    mw[i] = srk.compmoleweight(i+1)*1.0e-3 # g/mol -> kg/mol
    comp_name = srk.get_comp_name(i+1).lower()
    print("Molar weight {}: {} (kg/mol)".format(comp_name, mw[i]))

z = np.array([0.85, 0.1, 0.05])
mw_z = np.matmul(z, mw)
P = 1.01325e5
T, y = srk.bubble_temperature(P, z)
print(u"Atmospheric bubble temperature: {} (\N{DEGREE SIGN}C)".format(T-273.15))

# Fugacity coefficient
lnfug_l, = srk.thermo(T, P, z, srk.LIQPH)
lnfug_g, = srk.thermo(T, P, y, srk.VAPPH)
eq = lnfug_l + np.log(z) - lnfug_g - np.log(y)
print("Equilibrium condition from fugacity coefficient: {} (-)".format(eq))

# Properties evaluated in temperature, pressure and mol fractions
v, = srk.specific_volume(T, P, z, srk.LIQPH)
print("Saturated liquid density: {} (kg/m3)".format(mw_z/v))

h, dhdt, = srk.enthalpy(T, P, z, srk.LIQPH, dhdt=True)
print("Saturated liquid isobaric heat capacity: {} (J/kg/K)".format(dhdt/mw_z))

s, dsdt, = srk.entropy(T, P, z, srk.LIQPH, dsdt=True)
print("Saturated liquid isobaric heat capacity (form entropy): {} (J/kg/K)".format(T*dsdt/mw_z))

zFac, = srk.zfac(T, P, z, srk.LIQPH)
print("Compressibility factor: {}".format(zFac))

sos = srk.speed_of_sound(T, P, x=z, y=z, z=z, betaV=0.0, betaL=1.0, phase=srk.LIQPH)
print("Saturated liquid speed of sound: {} (m/s)".format(sos))

# Properties evaluated in temperature volume and mol numbers
p, dpdv,  = srk.pressure_tv(T, v, z, dpdv=True)
print("Isothermal liquid compressibillity: {} (1/GPa)".format(-1.0e9/v/dpdv))

e, Cv = srk.internal_energy_tv(T, v, z, dedt=True)
print("Saturated liquid isochoric heat capacity: {} (J/kg/K)".format(Cv/mw_z))

s, = srk.entropy_tv(T, v, z)
a,  = srk.helmholtz_tv(T, v, z)
print("Helmholtz free energy of liquid: {} (J/kg)".format(a/mw_z))
print("Helmholtz free energy og liquid (e-Ts): {} (J/kg)".format((e-T*s)/mw_z))

print("Internal energy of liquid: {} (J/kg)".format(e/mw_z))
h, = srk.enthalpy_tv(T, v, z)
print("Internal energy og liquid (h-P*V): {} (J/kg)".format((h-p*v)/mw_z))

mu_l, = srk.chemical_potential_tv(T, v, z)
v_g, = srk.specific_volume(T, P, y, srk.VAPPH)
mu_g, = srk.chemical_potential_tv(T, v_g, y)
print("Equilibrium condition from chemical potential: {} (-)".format(mu_l - mu_g))
lnphi_l, = srk.fugacity_tv(T, v, z)
lnphi_g, = srk.fugacity_tv(T, v_g, y)
print("Equilibrium condition from fugacity: {} (-)".format(lnphi_l - lnphi_g))


