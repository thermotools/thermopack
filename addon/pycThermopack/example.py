#!/usr/bin/python3

# Importing pyThermopack
from pyctp import thermo
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt


# Instanciate thermopack object
tp = thermo.thermopack()

print("calling get_phase_flags")
print(tp.get_phase_flags())

print("calling thermo_init")
tp.init_thermo("Thermopack","PR","Classic","Classic",2,"CO2,C1",2,liq_vap_discr_method=1,kij_setno=2,alpha_setno=1,b_exponent=2,csp_eos="SRK",csp_ref_comp="C3",saft_setno=[1,1])

z = np.array([0.9,0.1])
v, dvdt, dvdn = tp.specific_volume(270.0,1.0e6,z,1,dvdt=True,dvdn=True)
print(v, dvdt, dvdn)

print("mw1",tp.compmoleweight(1))
print("mw2",tp.compmoleweight(2))

zFac, = tp.zfac(270.0,1.0e6,z,1)
print("zfac:", zFac)

print("thermo:", tp.thermo(270.0,1.0e6,z,tp.VAPPH))
lnfug, dlnfugdt, dlnfugdp, dlnfugdn, ophase, v = tp.thermo(270.0,1.0e6,z,tp.MINGIBBSPH,dlnfugdt=True,dlnfugdp=True,dlnfugdn=True,ophase=True,v=True)
print("ophase",ophase)
print("v",v)
print("dlnfugt", dlnfugdt)
print("dlnfugp", dlnfugdp)
print("dlnfugn", dlnfugdn)
print("0:",dlnfugdn[0][:])
print("1:",dlnfugdn[1][:])

h, dhdt, dhdp, dhdn = tp.enthalpy(270.0,1.0e6,z,tp.VAPPH,dhdt=True,dhdp=True,dhdn=True)
print(h, dhdt, dhdp, dhdn)

s, dsdt, dsdp, dsdn = tp.entropy(270.0,1.0e6,z,tp.VAPPH,dsdt=True,dsdp=True,dsdn=True)
print(s, dsdt, dsdp, dsdn)

hi, dhidt, dhidp = tp.idealenthalpysingle(300.0,1.0e5,1,dhdt=True,dhdp=True)
print(hi, dhidt, dhidp)

sos = tp.speed_of_sound(300.0,1e5,x=z,y=z,z=z,betaV=1.0,betaL=0.0,phase=tp.VAPPH)
print("sos",sos)
tp.set_ph_tolerance(tol=1.0e-8)

x, y, betaV, betaL, phase = tp.two_phase_tpflash(270.0,2.0e6,z)
print(x, y, betaV, betaL, phase)

entropy = s
temp, x, y, betaV, betaL, phase = tp.two_phase_psflash(2.0e6,z,entropy,temp=None)
print(temp, x, y, betaV, betaL, phase)
temp = 270.0
temp, x, y, betaV, betaL, phase = tp.two_phase_psflash(2.0e6,z,entropy,temp=temp)
print(temp, x, y, betaV, betaL, phase)

enthalpy = h
temp, x, y, betaV, betaL, phase = tp.two_phase_phflash(2.0e6,z,enthalpy,temp=None)
print(temp, x, y, betaV, betaL, phase)
temp = 270.0
temp, x, y, betaV, betaL, phase = tp.two_phase_phflash(2.0e6,z,enthalpy,temp=temp)
print(temp, x, y, betaV, betaL, phase)

p = 1.0e6
energy = h - p*v
volume = v
temp, press, x, y, betaV, betaL, phase = tp.two_phase_uvflash(z,energy, volume, temp=temp, press=p)
print(temp, press, x, y, betaV, betaL, phase)




