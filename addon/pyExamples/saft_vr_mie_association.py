#!/usr/bin/env python
import sys, os
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.saftvrmie import saftvrmie
import numpy as np
import matplotlib.pyplot as plt

def plot_single_component_saturation(comp):
    svrm = saftvrmie(comp)
    mws = svrm.compmoleweight(1)
    z = np.array([1.0])
    Tvals, pvals, vvals = svrm.get_envelope_twophase(initial_pressure=1.0e4, z=z, calc_v=True)

    plt.figure()
    plt.plot(Tvals, pvals*1e-6,label="SAFT-VR Mie")
    path = "./data/" + comp+".dat"
    if os.path.isfile(path):
        data_NIST = np.loadtxt(path, skiprows=1)
        plt.plot(data_NIST[:,0], data_NIST[:,1]*1e-6,label="Reference EOS")
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(comp)
    leg = plt.legend(loc="best",numpoints=1, frameon=False)

    plt.figure()
    plt.plot(mws/vvals,Tvals,label="SAFT-VR Mie",color="k")
    if os.path.isfile(path):
        plt.plot(mws/data_NIST[:,2],data_NIST[:,0],label="Reference EOS",color="g")
        plt.plot(mws/data_NIST[:,3],data_NIST[:,0],color="g")
    plt.ylabel(r"$T$ (K)")
    plt.ylabel(r"$\rho$ (kg/m3)")
    plt.title(comp)
    leg = plt.legend(loc="best",numpoints=1, frameon=False)


def plot_binary_Txy_H2O_CH3OH():
    svrm = saftvrmie("H2O,MEOH")
    # Set kij=0.04
    pressure = 1.01325e5
    x = np.linspace(0.0, 1.0, 20)
    Tb = np.zeros_like(x)
    Td = np.zeros_like(x)
    Yb = np.zeros_like(x)
    Xd = np.zeros_like(x)
    for i, xi in enumerate(x):
        z = np.array([max(0.0, 1.0 - xi), xi])
        Tb[i], Y = svrm.bubble_temperature(pressure, z)
        Yb[i] = Y[1]
        Td[i], X = svrm.dew_temperature(pressure, z)
        Xd[i] = X[1]

    plt.figure()
    plt.plot(Yb, Tb, color="b", label="SAFT-VR Mie")
    plt.plot(Xd, Td, color="b")
    exp_data = np.loadtxt("./data/H2O-MEOH.dat", skiprows=1)
    plt.plot(exp_data[:,0], exp_data[:,2], label="Ramalho et al. (1961)", linestyle="None", marker="o", color="g")
    plt.plot(exp_data[:,1], exp_data[:,2], linestyle="None", marker="o", color="g")
    plt.xlabel(r"$x_{\rm{CH3OH}}$")
    plt.ylabel(r"$T$ (K)")
    plt.title("H2O-CH3OH Txy at P=1 atm")
    leg = plt.legend(loc="best",numpoints=1, frameon=False)


if __name__ == "__main__":
    plot_single_component_saturation("H2O")
    plot_single_component_saturation("NH3")
    plot_single_component_saturation("H2S")
    plot_single_component_saturation("MEOH")
    plot_single_component_saturation("N2H4")
    plot_binary_Txy_H2O_CH3OH()
    plt.show()
