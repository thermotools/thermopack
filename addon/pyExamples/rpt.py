#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.tcPR import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Phase diagrams for LNG and RPT visualization.
# Similar plots are found in:
# Predicting triggering and consequence of delayed LNG RPT
# Journal of Loss Prevention in the Process Industries
# E. Aursand and M. Hammer
# doi: 10.1016/j.jlp.2018.06.001

P0 = 1.01325e5 # 1 Atm

pr = tcPR("C1,C2,C3,nC4,iC4,N2")
z = np.array([0.94, 0.028, 0.008, 0.002, 0.002, 0.02])

# Critical point
Tc, vc, Pc = pr.critical(z)

# Plot phase envelope
T, P = pr.get_envelope_twophase(P0, z)

# Spinoidal
Ts, vs, Ps = pr.spinodal(z, initial_pressure=P0, min_temperature_vapor=T[-1])

plt.figure()
plt.plot(T, P*1e-5, color="k", label="Saturation curve")
plt.plot(Tc, Pc*1e-5, "ko")
plt.plot(Ts, Ps*1e-5, color="b", linestyle="-", label="Spinodal")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlabel(r"$T$ (K)")
plt.ylabel(r"$P$ (bar)")
plt.title(r"LNG phase envelope and spinodal")
plt.tight_layout()

# Define a mixture enriched in Ethane, Propane and Butane
pr.init("C1,C2,C3,nC4")
z = np.array([0.4, 0.3, 0.18, 0.12])

# Critical point
Tc, vc, Pc = pr.critical(z)

# Plot phase envelope
T, P = pr.get_envelope_twophase(P0, z)

# Spinoidal
Ts, vs, Ps = pr.spinodal(z, initial_pressure=P0)
# Extract liquid spinodal
Ps = Ps[vs <= vc]
Ts = Ts[vs <= vc]
vs = vs[vs <= vc]

# Bubble point
Tb = T[-1]
Pb = P0
# Superheated point
Psh = P0
Tsh = Ts[0] * 0.95 # Approximate superheat limit
vsh = vs[0]
hsh = pr.enthalpy_tv(Tsh, vsh, z)
ush = hsh - vsh * Psh # Internal energy
# Equilibrated point
res = pr.two_phase_uvflash(z, ush, vsh)
Peq = res.p
Teq = res.T
seq_gas = pr.entropy(Teq,Peq,res.y,pr.VAPPH)
seq_liq = pr.entropy(Teq,Peq,res.x,pr.LIQPH)
seq = seq_gas * res.betaV + seq_liq * res.betaL # Equilibrated entropy
heq_gas = pr.enthalpy(Teq, Peq, res.y, pr.VAPPH)
heq_liq = pr.enthalpy(Teq, Peq, res.x, pr.LIQPH)
heq = heq_gas * res.betaV + heq_liq * res.betaL # Equilibrated enthalpy
# Expansion path
n_exp = 40
Texp = np.zeros(n_exp)
Pexp = np.zeros(n_exp)
Pexp[0] = Peq
Texp[0] = Teq
for i in range(n_exp-1):
    Pexp[i + 1] = Peq - (Peq - Pb) * (i + 1) / (n_exp - 1)
    res_ps = pr.two_phase_psflash(Pexp[i + 1], z, seq, temp=Texp[i])
    Texp[i + 1] = res_ps.T

hexp_gas = pr.enthalpy(Texp[-1], Pexp[-1], res_ps.y, pr.VAPPH)
hexp_liq = pr.enthalpy(Texp[-1], Pexp[-1], res_ps.x, pr.LIQPH)
hexp = hexp_gas * res_ps.betaV + hexp_liq * res_ps.betaL # Expanded enthalpy

print(f"Peak explosive pressure: {Peq*1e-5:.2f} bar")
print(f"Theoretical expansion work: {(heq-hexp)*1e-3:.2f} kJ/mol")

plt.figure()
plt.plot(T, P * 1e-5, color="k", label="Saturation curve")
plt.plot(Ts, Ps * 1e-5, color="r", linestyle="-", label="Liquid Spinodal")
plt.plot(Tc, Pc * 1e-5, "ro")
plt.text(120.0, 5.0, "A", fontsize=14)
plt.plot([Tb], [Pb * 1e-5], "ko")
plt.text(250.0, 5.0, "B", fontsize=14)
plt.plot([Tsh], [Psh * 1e-5], "ko")
plt.text(260.0, 55.0, "C", fontsize=14)
plt.plot([Teq], [Peq * 1e-5], "ko")
plt.text(172.0, 5.0, "D", fontsize=14)
plt.plot([Texp[-1]], [Pexp[-1] * 1e-5], "ko")
plt.plot([Tb,Tsh], [Pb * 1e-5,Psh * 1e-5], "g", label=r"A$\rightarrow$B: Superheating")
plt.plot([Tsh,Teq], [Psh * 1e-5,Peq * 1e-5], "b", label=r"B$\rightarrow$C: Equilibration")
plt.plot(Texp, Pexp * 1e-5, "orange", label=r"C$\rightarrow$D: Expansion")
leg = plt.legend(loc="best", numpoints=1, frameon=False)
plt.xlabel(r"$T$ (K)")
plt.ylabel(r"$P$ (bar)")
plt.title(r"Enriched LNG phase envelope and RPT path")
plt.tight_layout()
plt.show()
