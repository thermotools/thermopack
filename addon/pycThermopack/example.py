#!/usr/bin/python
# Support for python2
from __future__ import print_function
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
#tp.init_thermo("Thermopack","PR","Classic","Classic",2,"CO2,C1",2,liq_vap_discr_method=1,kij_setno=2,alpha_setno=1,b_exponent=2,csp_eos="SRK",csp_ref_comp="C3",saft_setno=[1,1])
tp.init_cubic("CO2,C1","PR","Classic","Classic")

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

print("calling enthalpy:")
h, dhdt, dhdp, dhdn = tp.enthalpy(270.0,1.0e6,z,tp.VAPPH,dhdt=True,dhdp=True,dhdn=True)
print(h, dhdt, dhdp, dhdn)

print("calling entroy:")
s, dsdt, dsdp, dsdn = tp.entropy(270.0,1.0e6,z,tp.VAPPH,dsdt=True,dsdp=True,dsdn=True)
print(s, dsdt, dsdp, dsdn)

print("calling ideal enthalpy:")
hi, dhidt, dhidp = tp.idealenthalpysingle(300.0,1.0e5,1,dhdt=True,dhdp=True)
print(hi, dhidt, dhidp)

sos = tp.speed_of_sound(300.0,1e5,x=z,y=z,z=z,betaV=1.0,betaL=0.0,phase=tp.VAPPH)
tp.set_ph_tolerance(1.0e-8)
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

print("Testing UV flash")
p = 1.0e6
energy = h - p*v
volume = v
p = 1.5e6
t = 280.0
temp, press, x, y, betaV, betaL, phase = tp.two_phase_uvflash(z,energy, volume, temp=temp, press=p)
print(temp, press, x, y, betaV, betaL, phase)

phase = tp.guess_phase(temp, press, z)
print("Guessing phase: ", phase)


print("Rgas",tp.Rgas)
tp.set_tmin(100.0)
tp.set_pmin(9999.0)


p, = tp.pressure_tv(270.0,v,z)
print("p ",p)
p, dpdt ,dpdv, dpdn = tp.pressure_tv(270.0,v,z,dpdt=True,dpdv=True,dpdn=True)
print("p, dpdt ,dpdv, dpdn ",p, dpdt ,dpdv, dpdn)

e, = tp.internal_energy_tv(270.0,v,z)
print("e ",e)
e, dedt ,dedv = tp.internal_energy_tv(270.0,v,z,dedt=True,dedv=True)
print("e, dedt ,dedv ",e, dedt ,dedv)

s, = tp.entropy_tv(270.0,v,z)
print("s ",s)
s, dsdt ,dsdv, dsdn = tp.entropy_tv(270.0,v,z,dsdt=True,dsdv=True,dsdn=True)
print("s, dsdt ,dsdv, dsdn ",s, dsdt ,dsdv, dsdn)

h, = tp.enthalpy_tv(270.0,v,z)
print("h ",h)
h, dhdt ,dhdv, dhdn = tp.enthalpy_tv(270.0,v,z,dhdt=True,dhdv=True,dhdn=True)
print("h, dhdt ,dhdv, dhdn ",h, dhdt ,dhdv, dhdn)

a, = tp.helmholtz_tv(270.0,v,z)
print("a ",a)
a, dadt ,dadv = tp.helmholtz_tv(270.0,v,z,dadt=True,dadv=True)
print("a, dadt ,dadv ",a, dadt ,dadv)

mu, = tp.chemical_potential_tv(270.0,v,z)
print("mu ",mu)
mu, dmudt ,dmudv, dmudn = tp.chemical_potential_tv(270.0,v,z,dmudt=True,dmudv=True,dmudn=True)
print("mu, dmudt ,dmudv, dmudn ",mu, dmudt ,dmudv, dmudn)

lnphi, = tp.fugacity_tv(270.0,v,z)
print("lnphi ",lnphi)
lnphi, dlnphidt ,dlnphidv, dlnphidn = tp.fugacity_tv(270.0,v,z,dlnphidt=True,dlnphidv=True,dlnphidn=True)
print("lnphi, dlnphidt ,dlnphidv, dlnphidn ",lnphi, dlnphidt ,dlnphidv, dlnphidn)

T_c, v_c, P_c = tp.critical(z)
print("T_c, v_c, P_c", T_c, v_c, P_c)

B,C = tp.virial_coeffcients(300.0,z)
print(B,C)

B = tp.second_virial_matrix(300.0)
print(B)

C = tp.binary_third_virial_matrix(300.0)
print(C)

# Saturation intefaces

T, y = tp.bubble_temperature(5.0e5,z)
print(T, y)
T, x = tp.dew_temperature(5.0e5,z)
print(T, x)

P, y = tp.bubble_pressure(150.0,z)
print(P, y)
P, x = tp.dew_pressure(220.0,z)
print(P, x)

Tvals, Pvals = tp.get_envelope_twophase(1.0e5, z)
print(Pvals)
plt.plot(Tvals, Pvals)
plt.show()
plt.clf()

LLE, L1VE, L2VE = tp.get_binary_pxy(250.0)
if LLE[0] is not None:
    plt.plot(LLE[0], LLE[2])
    plt.plot(LLE[1], LLE[2])
if L1VE[0] is not None:
    plt.plot(L1VE[0], L1VE[2])
    plt.plot(L1VE[1], L1VE[2])
if L2VE[0] is not None:
    plt.plot(L2VE[0], L2VE[2])
    plt.plot(L2VE[1], L2VE[2])
plt.show()
plt.clf()
