#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import pets
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from pyctp_example_utils import calc_real_T, \
    calc_reduced_rho, calc_reduced_T, calc_reduced_P, calc_reduced_energy

# Instanciate and init PeTS object
PeTS = pets.pets()
PeTS.init()
PeTS.set_tmin(temp=5.0)

sigma = PeTS.sigma[0]
eps = PeTS.eps_div_kb[0]
z = np.array([1.0])
Tc, vc, Pc = PeTS.critical(z)
muc, = PeTS.chemical_potential_tv(temp=Tc, volume=vc, n=z)
T_triple = 0.56*PeTS.eps_div_kb[0]
g = 2.0

# Plot phase envelope
Psat = PeTS.bubble_pressure(T_triple, z)
T, P, v = PeTS.get_envelope_twophase(Psat[0], z, maximum_pressure=1.5e7, calc_v=True)
mu = np.zeros_like(T)
for i in range(np.shape(mu)[0]):
    mu[i], = PeTS.chemical_potential_tv(temp=T[i], volume=v[i], n=z)

# Spinoidal
T_s,v_s,P_s = PeTS.spinodal(z,
                            initial_pressure=1.0e5,
                            initial_liquid_temperature=T_triple,
                            min_temperature_vapor=T_triple)
mu_s = np.zeros_like(T_s)
for i in range(np.shape(mu_s)[0]):
    mu_s[i], = PeTS.chemical_potential_tv(temp=T_s[i], volume=v_s[i], n=z)

Tstar = 0.741
gamma0_0741 = 0.5075817348473084
Tiso =  calc_real_T(np.array([Tstar]), eps)[0]
n = 50
vz, rho = PeTS.map_meta_isotherm(temperature=Tiso,
                                 z=z,
                                 phase=PeTS.LIQPH,
                                 n=n)
Pz = np.zeros_like(vz)
Prho = np.zeros_like(vz)
muz = np.zeros_like(vz)
for i in range(n):
    Pz[i], = PeTS.pressure_tv(Tiso,vz[i],z)
    Prho[i], = PeTS.pressure_tv(Tiso,1.0,rho[i][:])
    muz[i], = PeTS.chemical_potential_tv(temp=Tiso, volume=vz[i], n=z)

Tstar = 0.9
gamma0_09 = 0.2372
Tiso_09 =  calc_real_T(np.array([Tstar]), eps)[0]
vz_09, rho_09 = PeTS.map_meta_isotherm(temperature=Tiso_09,
                                 z=z,
                                 phase=PeTS.LIQPH,
                                 n=n)
Pz_09 = np.zeros_like(vz)
Prho_09 = np.zeros_like(vz)
muz_09 = np.zeros_like(vz)
for i in range(n):
    Pz_09[i], = PeTS.pressure_tv(Tiso_09,vz_09[i],z)
    Prho_09[i], = PeTS.pressure_tv(Tiso_09,1.0,rho_09[i][:])
    muz_09[i], = PeTS.chemical_potential_tv(temp=Tiso_09, volume=vz_09[i], n=z)

plt.figure()
plt.plot(calc_reduced_rho(1.0/v, sigma), calc_reduced_T(T ,eps), color="k")
plt.plot(calc_reduced_rho(np.array([1.0/vc]), sigma), calc_reduced_T(np.array([Tc]) ,eps), "ko")
plt.plot(calc_reduced_rho(1.0/v_s, sigma), calc_reduced_T(T_s ,eps), color="k", linestyle="--")
plt.plot(calc_reduced_rho(1.0/vz, sigma), calc_reduced_T(np.array([Tiso]*n) ,eps), color="tab:blue")
plt.plot(calc_reduced_rho(rho, sigma), calc_reduced_T(np.array([Tiso]*n) ,eps), color="tab:green")
plt.plot(calc_reduced_rho(1.0/vz_09, sigma), calc_reduced_T(np.array([Tiso_09]*n) ,eps), color="tab:red")
plt.plot(calc_reduced_rho(rho_09, sigma), calc_reduced_T(np.array([Tiso_09]*n) ,eps), color="tab:orange")
plt.xlabel(r"$\rho^*$")
plt.ylabel(r"$T^*$")

plt.figure()
plt.plot(calc_reduced_T(T ,eps), calc_reduced_P(P, eps, sigma), color="k")
plt.plot(calc_reduced_T(np.array([Tc]) ,eps), calc_reduced_P(np.array([Pc]), eps, sigma), "ko")
plt.plot(calc_reduced_T(T_s ,eps), calc_reduced_P(P_s, eps, sigma), color="k", linestyle="--")
plt.plot(calc_reduced_T(np.array([Tiso]*n) ,eps), calc_reduced_P(Pz, eps, sigma), color="tab:blue")
plt.plot(calc_reduced_T(np.array([Tiso]*n) ,eps), calc_reduced_P(Prho, eps, sigma), color="tab:green")
plt.plot(calc_reduced_T(np.array([Tiso_09]*n) ,eps), calc_reduced_P(Pz_09, eps, sigma), color="tab:red")
plt.plot(calc_reduced_T(np.array([Tiso_09]*n) ,eps), calc_reduced_P(Prho_09, eps, sigma), color="tab:orange")
plt.ylabel(r"$P^*$")
plt.xlabel(r"$T^*$")

R_0741 = np.zeros_like(vz)
R_09 = np.zeros_like(vz)
dP_0741 = np.zeros_like(vz)
dP_0741[:] = calc_reduced_P(Pz, eps, sigma) - calc_reduced_P(Prho, eps, sigma)
dP_09 = np.zeros_like(vz)
dP_09[:] = calc_reduced_P(Pz_09, eps, sigma) - calc_reduced_P(Prho_09, eps, sigma)

R_0741[0] = np.nan
R_09[0] = np.nan
for i in range(1,n):
    R_0741[i] = -g*gamma0_0741/dP_0741[i]
    R_09[i] = -g*gamma0_09/dP_09[i]

plt.figure()
plt.plot(dP_0741, R_0741, color="tab:green", label="$T^*=0.741$")
plt.plot(dP_09, R_09, color="tab:orange", label="$T^*=0.9$")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.title("Bubbles")
plt.ylabel(r"$R*$")
plt.xlabel(r"$\Delta P^*$")

plt.figure()
plt.plot(calc_reduced_P(P, eps, sigma), calc_reduced_energy(mu ,eps), color="k")
plt.plot(calc_reduced_P(np.array([Pc]), eps, sigma), calc_reduced_energy(np.array([muc]) ,eps), "ko")
plt.plot(calc_reduced_P(P_s, eps, sigma), calc_reduced_energy(mu_s ,eps), color="k", linestyle="--")
plt.plot(calc_reduced_P(Pz, eps, sigma), calc_reduced_energy(muz ,eps), color="tab:blue")
plt.plot(calc_reduced_P(Prho, eps, sigma), calc_reduced_energy(muz ,eps), color="tab:green")
plt.plot(calc_reduced_P(Pz_09, eps, sigma), calc_reduced_energy(muz_09 ,eps), color="tab:red")
plt.plot(calc_reduced_P(Prho_09, eps, sigma), calc_reduced_energy(muz_09 ,eps), color="tab:orange")
#leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlim([-0.6, plt.gca().get_xlim()[1]])
plt.ylabel(r"$\mu^*$")
plt.xlabel(r"$P^*$")


######################################################################

Tstar = 0.741
Tiso =  calc_real_T(np.array([Tstar]), eps)[0]
n = 25
vz, rho = PeTS.map_meta_isotherm(temperature=Tiso,
                                 z=z,
                                 phase=PeTS.VAPPH,
                                 n=n)
Pz = np.zeros_like(vz)
Prho = np.zeros_like(vz)
muz = np.zeros_like(vz)
for i in range(n):
    Pz[i], = PeTS.pressure_tv(Tiso,vz[i],z)
    Prho[i], = PeTS.pressure_tv(Tiso,1.0,rho[i][:])
    muz[i], = PeTS.chemical_potential_tv(temp=Tiso, volume=vz[i], n=z)

Tstar = 0.9
Tiso_09 =  calc_real_T(np.array([Tstar]), eps)[0]
vz_09, rho_09 = PeTS.map_meta_isotherm(temperature=Tiso_09,
                                 z=z,
                                 phase=PeTS.VAPPH,
                                 n=n)
Pz_09 = np.zeros_like(vz)
Prho_09 = np.zeros_like(vz)
muz_09 = np.zeros_like(vz)
for i in range(n):
    Pz_09[i], = PeTS.pressure_tv(Tiso_09,vz_09[i],z)
    Prho_09[i], = PeTS.pressure_tv(Tiso_09,1.0,rho_09[i][:])
    muz_09[i], = PeTS.chemical_potential_tv(temp=Tiso_09, volume=vz_09[i], n=z)

plt.figure()
plt.plot(calc_reduced_rho(1.0/v, sigma), calc_reduced_T(T ,eps), color="k")
plt.plot(calc_reduced_rho(np.array([1.0/vc]), sigma), calc_reduced_T(np.array([Tc]) ,eps), "ko")
plt.plot(calc_reduced_rho(1.0/v_s, sigma), calc_reduced_T(T_s ,eps), color="k", linestyle="--")
plt.plot(calc_reduced_rho(1.0/vz, sigma), calc_reduced_T(np.array([Tiso]*n) ,eps), color="tab:blue")
plt.plot(calc_reduced_rho(rho, sigma), calc_reduced_T(np.array([Tiso]*n) ,eps), color="tab:green")
plt.plot(calc_reduced_rho(1.0/vz_09, sigma), calc_reduced_T(np.array([Tiso_09]*n) ,eps), color="tab:red")
plt.plot(calc_reduced_rho(rho_09, sigma), calc_reduced_T(np.array([Tiso_09]*n) ,eps), color="tab:orange")
plt.xlabel(r"$\rho^*$")
plt.ylabel(r"$T^*$")

plt.figure()
plt.plot(calc_reduced_T(T ,eps), calc_reduced_P(P, eps, sigma), color="k")
plt.plot(calc_reduced_T(np.array([Tc]) ,eps), calc_reduced_P(np.array([Pc]), eps, sigma), "ko")
plt.plot(calc_reduced_T(T_s ,eps), calc_reduced_P(P_s, eps, sigma), color="k", linestyle="--")
plt.plot(calc_reduced_T(np.array([Tiso]*n) ,eps), calc_reduced_P(Prho, eps, sigma), color="tab:green")
plt.plot(calc_reduced_T(np.array([Tiso]*n) ,eps), calc_reduced_P(Pz, eps, sigma), color="tab:blue")
plt.plot(calc_reduced_T(np.array([Tiso_09]*n) ,eps), calc_reduced_P(Prho_09, eps, sigma), color="tab:orange")
plt.plot(calc_reduced_T(np.array([Tiso_09]*n) ,eps), calc_reduced_P(Pz_09, eps, sigma), color="tab:red")
plt.ylabel(r"$P^*$")
plt.xlabel(r"$T^*$")


R_0741 = np.zeros_like(vz)
R_09 = np.zeros_like(vz)
dP_0741 = np.zeros_like(vz)
dP_0741[:] = calc_reduced_P(Pz, eps, sigma) - calc_reduced_P(Prho, eps, sigma)
dP_09 = np.zeros_like(vz)
dP_09[:] = calc_reduced_P(Pz_09, eps, sigma) - calc_reduced_P(Prho_09, eps, sigma)

R_0741[0] = np.nan
R_09[0] = np.nan
for i in range(1,n):
    R_0741[i] = -g*gamma0_0741/dP_0741[i]
    R_09[i] = -g*gamma0_09/dP_09[i]

plt.figure()
plt.plot(dP_0741, R_0741, color="tab:green", label="$T^*=0.741$")
plt.plot(dP_09, R_09, color="tab:orange", label="$T^*=0.9$")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.title("Droplets")
plt.ylabel(r"$R*$")
plt.xlabel(r"$\Delta P^*$")

plt.figure()
plt.plot(calc_reduced_P(P, eps, sigma), calc_reduced_energy(mu ,eps), color="k")
plt.plot(calc_reduced_P(np.array([Pc]), eps, sigma), calc_reduced_energy(np.array([muc]) ,eps), "ko")
plt.plot(calc_reduced_P(P_s, eps, sigma), calc_reduced_energy(mu_s ,eps), color="k", linestyle="--")
plt.plot(calc_reduced_P(Pz, eps, sigma), calc_reduced_energy(muz ,eps), color="tab:blue")
plt.plot(calc_reduced_P(Prho, eps, sigma), calc_reduced_energy(muz ,eps), color="tab:green")
plt.plot(calc_reduced_P(Pz_09, eps, sigma), calc_reduced_energy(muz_09 ,eps), color="tab:red")
plt.plot(calc_reduced_P(Prho_09, eps, sigma), calc_reduced_energy(muz_09 ,eps), color="tab:orange")
#leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlim([-0.2, plt.gca().get_xlim()[1]])
plt.ylabel(r"$\mu^*$")
plt.xlabel(r"$P^*$")



plt.show()

