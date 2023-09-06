#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.pets import pets
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from thermopack_example_utils import calc_real_T, \
    calc_reduced_rho, calc_reduced_T, calc_reduced_P, \
    calc_reduced_energy, calc_real_rho

# Visualization of meta-stable states along three isotherms
# The isotherms where used for studying droplets in:
# Free energy of critical droplets - from the binodal to the spinodal
# The Journal of Chemical Physics 158(11)
# A. Aasen, Ø. Wilhelmsen M. Hammer, D, Reguera
# doi: 10.1063/5.0142533

# Instanciate and init PeTS object
PeTS = pets()

sigma = PeTS.sigma[0]
eps = PeTS.eps_div_kb[0]
z = np.array([1.0])
T_triple = 0.56*PeTS.eps_div_kb[0]
n = 50
phases = {}
phases["LIQ"] = PeTS.LIQPH
phases["VAP"] = PeTS.VAPPH

# Critical point
Tc, vc, Pc = PeTS.critical(z)
muc, = PeTS.chemical_potential_tv(temp=Tc, volume=vc, n=z)
critical = {}
critical["T"] = calc_reduced_T(np.array([Tc]) ,eps)
critical["rho"] = calc_reduced_rho(np.array([1/vc]), sigma)
critical["P"] = calc_reduced_P(np.array([Pc]), eps, sigma)
critical["mu"] = calc_reduced_energy(np.array([muc]), eps)

# Plot phase envelope
Psat = PeTS.bubble_pressure(T_triple, z)
T, P, v = PeTS.get_envelope_twophase(Psat[0], z, maximum_pressure=1.5e7, calc_v=True)
mu = np.zeros_like(T)
for i in range(np.shape(mu)[0]):
    mu[i] = PeTS.chemical_potential_tv(temp=T[i], volume=v[i], n=z)[0][0]
sat = {}
sat["T"] = calc_reduced_T(T ,eps)
sat["rho"] = calc_reduced_rho(1/v, sigma)
sat["P"] = calc_reduced_P(P, eps, sigma)
sat["mu"] = calc_reduced_energy(mu, eps)

# Spinoidal
T_s,v_s,P_s = PeTS.spinodal(z,
                            initial_pressure=1.0e5,
                            initial_liquid_temperature=T_triple,
                            min_temperature_vapor=T_triple)
mu_s = np.zeros_like(T_s)
for i in range(np.shape(mu_s)[0]):
    mu_s[i] = PeTS.chemical_potential_tv(temp=T_s[i], volume=v_s[i], n=z)[0][0]
spin = {}
spin["T"] = calc_reduced_T(T_s ,eps)
spin["rho"] = calc_reduced_rho(1/v_s, sigma)
spin["P"] = calc_reduced_P(P_s, eps, sigma)
spin["mu"] = calc_reduced_energy(mu_s, eps)

# Isotherms
temperatures = ["0.625", "0.741", "0.9"]
isoterms = {}
for Tstar in temperatures:
    isoterms[Tstar] = {}
    for phase in phases:
        isoterms[Tstar][phase] = {}
        Tiso =  calc_real_T(np.array([float(Tstar)]), eps)[0]
        vz, rho = PeTS.map_meta_isotherm(temperature=Tiso,
                                         z=z,
                                         phase=phases[phase],
                                         n=n)
        Pz = np.zeros_like(vz)
        Prho = np.zeros_like(vz)
        muz = np.zeros_like(vz)
        for i in range(n):
            Pz[i], = PeTS.pressure_tv(Tiso,vz[i],z)
            Prho[i], = PeTS.pressure_tv(Tiso,1.0,rho[i][:])
            muz[i] = PeTS.chemical_potential_tv(temp=Tiso, volume=vz[i], n=z)[0][0]
        isoterms[Tstar][phase]["mu"] = calc_reduced_energy(muz ,eps)
        isoterms[Tstar][phase]["rho_z"] = calc_reduced_rho(1.0/vz, sigma)
        isoterms[Tstar][phase]["rho"] = calc_reduced_rho(rho, sigma)
        isoterms[Tstar][phase]["P_z"] = calc_reduced_P(Pz, eps, sigma)
        isoterms[Tstar][phase]["P"] = calc_reduced_P(Prho, eps, sigma)
    isoterms[Tstar]["T"] = calc_reduced_T(np.array([Tiso]*n) ,eps)
    isoterms[Tstar]["rho_spin"] = np.linspace(isoterms[Tstar]["LIQ"]["rho_z"][-1],
                                              isoterms[Tstar]["VAP"]["rho_z"][-1], n)
    rho_spin = calc_real_rho(isoterms[Tstar]["rho_spin"], sigma)
    P_spin = np.zeros_like(rho_spin)
    mu_spin = np.zeros_like(rho_spin)
    for i in range(n):
        P_spin[i], = PeTS.pressure_tv(Tiso,1.0/rho_spin[i],z)
        mu_spin[i] = PeTS.chemical_potential_tv(temp=Tiso, volume=1.0/rho_spin[i], n=z)[0][0]
    isoterms[Tstar]["P_spin"] = calc_reduced_P(P_spin, eps, sigma)
    isoterms[Tstar]["mu_spin"] = calc_reduced_energy(mu_spin ,eps)

plt.figure()
plt.plot(sat["rho"], sat["T"], color="k", label="Binodal")
plt.plot(critical["rho"], critical["T"], "ko")
plt.plot(spin["rho"], spin["T"], color="k", linestyle="--", label="Spinodal")
colors = {temperatures[0]: "tab:blue", temperatures[1]: "tab:gray", temperatures[2]: "tab:green"}
for Tstar in temperatures:
    label=r"$T^*=$"+Tstar
    for phase in phases:
        plt.plot(isoterms[Tstar][phase]["rho_z"], isoterms[Tstar]["T"], color=colors[Tstar], label=label)
        plt.plot(isoterms[Tstar][phase]["rho"], isoterms[Tstar]["T"], color=colors[Tstar])
        label=None
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.ylabel(r"$T^*$")
plt.xlabel(r"$\rho^*$")
plt.tight_layout()
#plt.savefig("pets_Trho.pdf")

plt.figure()
plt.plot(sat["T"], sat["P"], color="k", label="Binodal")
plt.plot(critical["T"], critical["P"], "ko")
plt.plot(spin["T"], spin["P"], color="k", linestyle="--", label="Spinodal")
colors = {temperatures[0]: "tab:blue", temperatures[1]: "tab:gray", temperatures[2]: "tab:green"}
for Tstar in temperatures:
    label=r"$T^*=$"+Tstar
    for phase in phases:
        plt.plot(isoterms[Tstar]["T"], isoterms[Tstar][phase]["P_z"], color=colors[Tstar], label=label)
        plt.plot(isoterms[Tstar]["T"], isoterms[Tstar][phase]["P"], color=colors[Tstar])
        label=None
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlabel(r"$T^*$")
plt.ylabel(r"$P^*$")
plt.tight_layout()
#plt.savefig("pets_TP.pdf")

plt.figure()
plt.plot(sat["P"], sat["mu"], color="k", label="Binodal")
plt.plot(critical["P"], critical["mu"], "ko")
plt.plot(spin["P"], spin["mu"], color="k", linestyle="--", label="Spinodal")
colors = {temperatures[0]: "tab:blue", temperatures[1]: "tab:gray", temperatures[2]: "tab:green"}
for Tstar in temperatures:
    label=r"$T^*=$"+Tstar
    for phase in phases:
        plt.plot(isoterms[Tstar][phase]["P_z"], isoterms[Tstar][phase]["mu"], color=colors[Tstar], label=label)
        plt.plot(isoterms[Tstar][phase]["P"], isoterms[Tstar][phase]["mu"], color=colors[Tstar])
        label=None
    plt.plot(isoterms[Tstar]["P_spin"], isoterms[Tstar]["mu_spin"], color=colors[Tstar], linestyle=":")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlim([-0.2, plt.gca().get_xlim()[1]])
plt.ylabel(r"$\mu^*$")
plt.xlabel(r"$P^*$")
plt.tight_layout()
#plt.savefig("pets_Pmu.pdf")

plt.figure()
plt.plot(sat["rho"], sat["P"], color="k", label="Binodal")
plt.plot(critical["rho"], critical["P"], "ko")
plt.plot(spin["rho"], spin["P"], color="k", linestyle="--", label="Spinodal")
colors = {temperatures[0]: "tab:blue", temperatures[1]: "tab:gray", temperatures[2]: "tab:green"}
for Tstar in temperatures:
    label=r"$T^*=$"+Tstar
    for phase in phases:
        plt.plot(isoterms[Tstar][phase]["rho_z"], isoterms[Tstar][phase]["P_z"], color=colors[Tstar], label=label)
        plt.plot(isoterms[Tstar][phase]["rho"], isoterms[Tstar][phase]["P"], color=colors[Tstar])
        label=None
    plt.plot(isoterms[Tstar]["rho_spin"], isoterms[Tstar]["P_spin"], color=colors[Tstar], linestyle=":")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlabel(r"$\rho^*$")
plt.ylabel(r"$P^*$")
plt.tight_layout()
#plt.savefig("pets_Prho.pdf")
plt.show()
