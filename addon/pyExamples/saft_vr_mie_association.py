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


if __name__ == "__main__":
    plot_single_component_saturation("H2O")
    plot_single_component_saturation("NH3")
    plot_single_component_saturation("H2S")
    plot_single_component_saturation("MEOH")
    plot_single_component_saturation("N2H4")
    plt.show()
