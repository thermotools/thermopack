#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import ljs_wca, ljs_bh
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho, \
    calc_reduced_P

def plot_phase_envelope(LJS, labels, title):
    # Plot phase envelopes
    z = np.array([1.0])
    colors = ["k", "b", "g", "r"]
    for i, ljs in enumerate(LJS):
        # Get parameters
        sigma, eps = ljs.get_sigma_eps()
        T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
        T_s = calc_reduced_T(T, eps)
        rho_s = calc_reduced_rho(1.0/v, sigma)
        Tc, vc, Pc = ljs.critical(z)
        Tc_s = calc_reduced_T(np.array([Tc]), eps)
        rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
        plt.plot(rho_s, T_s, color=colors[i], label=labels[i])
        plt.plot(rhoc_s, Tc_s, color=colors[i], marker="o")

    plt.ylabel(r"$T^*$")
    plt.xlabel(r"$\rho^*$")
    leg = plt.legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)
    plt.title(title)
    plt.show()
    plt.close()

def plot_JT_inversion(LJS, labels, title, override_colors=None):
    # Plot Joule-Thompson inversion curves
    z = np.array([1.0])
    if override_colors is not None:
        colors = override_colors
    else:
        colors = ["k", "b", "g", "r"]
    for i, ljs in enumerate(LJS):
        # Get parameters
        sigma, eps = ljs.get_sigma_eps()
        T, P, v = ljs.joule_thompson_inversion(z)
        T_s = calc_reduced_T(T, eps)
        P_s = calc_reduced_P(P, eps, sigma)
        plt.plot(P_s, T_s, color=colors[i], label=labels[i])

    plt.ylabel(r"$T^*$")
    plt.xlabel(r"$P^*$")
    leg = plt.legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)
    plt.title(title)
    plt.show()
    plt.close()

def plot_BH_preturbation_terms(ljs):
    """Plot the Barker-Henderson perturbation terms"""
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)
    T_star = [0.6, 0.75, 1.0, 2.0, 4.0]
    colors = ["g", "b", "r", "grey", "k"]
    rho_star = np.linspace(0.0, 0.9, 50)
    a1 = np.zeros_like(rho_star)
    a2 = np.zeros_like(rho_star)
    a3 = np.zeros_like(rho_star)
    for j in range(len(T_star)):
        label = '$T^*=${:.2f}'.format(T_star[j])
        color = colors[j]
        for i in range(len(rho_star)):
            a1[i], a2[i], a3[i] = ljs.get_pert_a(T_star[j], rho_star[i])
        ax1.plot(rho_star, a1, color=color, label=label)
        ax2.plot(rho_star, a2, color=color, label=label)
        ax3.plot(rho_star, a3, color=color, label=label)

    ax1.set(xlabel=r"$\rho^*$", ylabel=r"a$_1/\epsilon$")
    ax2.set(xlabel=r"$\rho^*$", ylabel=r"a$_2/\epsilon^2$")
    ax3.set(xlabel=r"$\rho^*$", ylabel=r"a$_3/\epsilon^3$")
    handles, labels = ax1.get_legend_handles_labels()
    ax4.legend(handles, labels, loc='center')
    ax4.axis('off')
    plt.suptitle("Barker-Henderson perturbation terms")
    plt.tight_layout()
    plt.show()
    plt.close()

def plot_BH_diameter(ljs):
    """Plot the Barker-Henderson diameter as a function of reduced temperature"""
    T_star = np.linspace(0.5, 12, 50)
    d_bh = np.zeros_like(T_star)
    for j in range(len(T_star)):
        d_bh[j] = ljs.get_bh_diameter_div_sigma(T_star[j])

    plt.plot(T_star, d_bh, color="k")
    plt.ylabel(r"$d_{\rm{BH}}/\sigma$")
    plt.xlabel(r"$T^*$")
    plt.title("Barker-Henderson diameter for Lennard-Jones (splined)")
    plt.show()
    plt.close()

def plot_WCA_preturbation_terms(ljs):
    """Plot the Weeks–Chandler–Anderson perturbation terms"""
    fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2)
    T_star = [0.6, 0.75, 1.0, 2.0, 4.0]
    colors = ["g", "b", "r", "grey", "k"]
    rho_star = np.linspace(0.0, 0.9, 50)
    a1 = np.zeros_like(rho_star)
    a2 = np.zeros_like(rho_star)
    a3 = np.zeros_like(rho_star)
    a4 = np.zeros_like(rho_star)
    for j in range(len(T_star)):
        label = '$T^*=${:.2f}'.format(T_star[j])
        color = colors[j]
        for i in range(len(rho_star)):
            a1[i], a2[i], a3[i], a4[i] = ljs.get_pert_a(T_star[j], rho_star[i])
        ax1.plot(rho_star, a1, color=color, label=label)
        ax2.plot(rho_star, a2, color=color, label=label)
        ax3.plot(rho_star, a3, color=color, label=label)
        ax4.plot(rho_star, a4, color=color, label=label)

    ax1.set(xlabel=r"$\rho^*$", ylabel=r"a$_1/\epsilon$")
    ax2.set(xlabel=r"$\rho^*$", ylabel=r"a$_2/\epsilon^2$")
    ax3.set(xlabel=r"$\rho^*$", ylabel=r"a$_3/\epsilon^3$")
    ax4.set(xlabel=r"$\rho^*$", ylabel=r"a$_4/\epsilon^4$")
    handles, labels = ax1.get_legend_handles_labels()
    leg = fig.legend(handles, labels, bbox_to_anchor = (1.3, 0.6))
    plt.suptitle("Weeks–Chandler–Anderson perturbation terms")
    fig.tight_layout()
    plt.show()
    plt.close()

def get_BH_models():
    """Get model objects with order 1-3 for the Barker-Henderson perturbation theory"""
    LJS_BH = []
    for i in range(3):
        # Instanciate and init LJS-BH object
        ljs = ljs_bh.ljs_bh()
        ljs.init()
        ljs.set_tmin(temp=2.0)
        if i == 1:
            ljs.model_control(enable_a3=False)
        elif i == 2:
            ljs.model_control(enable_a2=False, enable_a3=False)
        if i > 0:
            ljs.redefine_critical_parameters()
        LJS_BH.append(ljs)

    labels_BH = ["$a_1+a_2+a_3$", "$a_1+a_2$", "$a_1$"]

    return LJS_BH, labels_BH

def get_WCA_models():
    """Get model objects with order 1-4 for the Weeks–Chandler–Anderson perturbation theory"""
    LJS_WCA = []
    for i in range(4):
        # Instanciate and init LJS-WCA object
        ljs = ljs_wca.ljs_wca()
        ljs.init()
        ljs.set_tmin(temp=2.0)
        if i > 0:
            if i == 1:
                enable_a4=False
                enable_a3=True
                enable_a2=True
            elif i == 2:
                enable_a4=False
                enable_a3=False
                enable_a2=True
            elif i == 3:
                enable_a4=False
                enable_a3=False
                enable_a2=False
            ljs.model_control(enable_a2=enable_a2,enable_a3=enable_a3,enable_a4=enable_a4)
            ljs.redefine_critical_parameters()
        LJS_WCA.append(ljs)

    labels_WCA = ["$a_{1-4}$", "$a_{1-3}$", "$a_{1-2}$", "$a_{1}$"]

    return LJS_WCA, labels_WCA

if __name__ == '__main__':
    ####################################################################
    # UV perturbation theory
    ####################################################################

    # Instanciate and init LJS-UV object
    ljs = ljs_wca.ljs_uv()
    ljs.set_tmin(temp=2.0)

    plot_phase_envelope([ljs], labels=["UV"], title="LJS-UV phase diagram")
    plot_JT_inversion([ljs], labels=["UV"], title="LJS-UV Joule-Thompson inversion curves")

    ####################################################################
    # Barker-Henderson perturbation theory
    ####################################################################

    LJS_BH, labels_BH = get_BH_models()
    plot_phase_envelope(LJS_BH, labels=labels_BH, title="LJS-BH phase diagram")
    plot_JT_inversion(LJS_BH, labels=labels_BH, title="LJS-BH Joule-Thompson inversion curves")
    plot_BH_preturbation_terms(LJS_BH[0])
    plot_BH_diameter(LJS_BH[0])

    ####################################################################
    # Weeks–Chandler–Anderson  perturbation theory
    ####################################################################

    LJS_WCA, labels_WCA = get_WCA_models()
    plot_phase_envelope(LJS_WCA, labels=labels_WCA, title="LJS-WCA phase diagram")
    plot_JT_inversion(LJS_WCA, labels=labels_WCA, title="LJS-WCA Joule-Thompson inversion curves")
    plot_WCA_preturbation_terms(LJS_WCA[0])
