#!/usr/bin/python3
#Modify system path
import sys
sys.path.append('../../pycThermopack/')
sys.path.append('../')
# Importing pyThermopack
from pyctp import ljs_wca, ljs_bh
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import MaxNLocator
from pyctp_example_utils import calc_reduced_T, calc_reduced_rho, \
    calc_reduced_P, calc_reduced_heat_capacity, calc_real_T, \
    calc_real_rho
from lennard_jones_spline import get_BH_models, get_WCA_models, plot_JT_inversion

SMALL_SIZE=10
MEDIUM_SIZE=12
LARGE_SIZE=14
mpl.rc('font', size=MEDIUM_SIZE)
mpl.rcParams["figure.titlesize"] = MEDIUM_SIZE
mpl.rcParams['lines.linewidth'] = 2
mpl.rcParams['axes.labelsize'] = MEDIUM_SIZE
mpl.rcParams['axes.titlesize'] = MEDIUM_SIZE
mpl.rcParams['lines.markersize'] = 6
mpl.rcParams['xtick.labelsize'] = SMALL_SIZE
mpl.rcParams['ytick.labelsize'] = SMALL_SIZE
mpl.rcParams['legend.fontsize'] = MEDIUM_SIZE


def plot_figure_2(LJS_BH, LJS_WCA):
    z = np.array([1.0])
    colors = ["k", "grey", "darkgrey", "lightgrey"]

    fig, (ax1, ax2) = plt.subplots(1, 2, sharey="all")
    plt.subplots_adjust(wspace=0, hspace=0)

    # Plot phase envelope
    for i, ljs in enumerate(LJS_BH):
        sigma, eps = ljs.get_sigma_eps()
        T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
        T_s = calc_reduced_T(T, eps)
        rho_s = calc_reduced_rho(1.0/v, sigma)
        Tc, vc, Pc = ljs.critical(z)
        Tc_s = calc_reduced_T(np.array([Tc]), eps)
        rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
        ax1.plot(rho_s, T_s, color=colors[i])
        ax1.plot(rhoc_s, Tc_s, color=colors[i], marker="o")

    custom_legends = [Line2D([0], [0], color='k', linestyle="None", label='BH')]
    leg = ax1.legend(handles=custom_legends, loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)

    # Plot phase envelope
    for i, ljs in enumerate(LJS_WCA):
        sigma, eps = ljs.get_sigma_eps()
        T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
        T_s = calc_reduced_T(T, eps)
        rho_s = calc_reduced_rho(1.0/v, sigma)
        Tc, vc, Pc = ljs.critical(z)
        Tc_s = calc_reduced_T(np.array([Tc]), eps)
        rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
        ax2.plot(rho_s, T_s, color=colors[i])
        ax2.plot(rhoc_s, Tc_s, color=colors[i], marker="o")

    custom_legends = [Line2D([0], [0], color='k', linestyle="None", label='WCA')]
    leg = ax2.legend(handles=custom_legends, loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)

    ax1.set_ylabel(r"$T^*$")
    ax1.set_ylim((0.55,1.1))
    fig.text(0.5, 0.03, r"$\rho^*$", ha='center', va='center')
    ax1.set_xlim((0.0,0.9))
    ax2.set_xlim((0.0,0.9))

    plt.suptitle("Figure 2: Phase diagrams BH and WCA")


def plot_figure_3(LJS_BH, LJS_WCA):
    z = np.array([1.0])
    colors = ["k", "grey", "darkgrey", "lightgrey"]

    rho_c_GCMC = 0.333
    T_c_GCMC = 0.8796
    P_c_GCMC = 0.07451

    # Get critical points
    BH_crit_T = []
    BH_crit_P = []
    BH_crit_rho = []
    BH_order = [3, 2, 1]
    for i, ljs in enumerate(LJS_BH):
        sigma, eps = ljs.get_sigma_eps()
        Tc, vc, Pc = ljs.critical(z)
        Tc_s = calc_reduced_T(np.array([Tc]), eps)
        rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
        Pc_s = calc_reduced_P(np.array([Pc]), eps, sigma)
        BH_crit_T.append(Tc_s[0])
        BH_crit_P.append(Pc_s[0])
        BH_crit_rho.append(rhoc_s[0])

    BH_crit_T = np.array(BH_crit_T)/T_c_GCMC
    BH_crit_P = np.array(BH_crit_P)/P_c_GCMC
    BH_crit_rho = np.array(BH_crit_rho)/rho_c_GCMC

    WCA_crit_T = []
    WCA_crit_P = []
    WCA_crit_rho = []
    WCA_order = [4, 3, 2, 1]
    for i, ljs in enumerate(LJS_WCA):
        sigma, eps = ljs.get_sigma_eps()
        Tc, vc, Pc = ljs.critical(z)
        Tc_s = calc_reduced_T(np.array([Tc]), eps)
        rhoc_s = calc_reduced_rho(np.array([1.0/vc]), sigma)
        Pc_s = calc_reduced_P(np.array([Pc]), eps, sigma)
        WCA_crit_T.append(Tc_s[0])
        WCA_crit_P.append(Pc_s[0])
        WCA_crit_rho.append(rhoc_s[0])

    WCA_crit_T = np.array(WCA_crit_T)/T_c_GCMC
    WCA_crit_P = np.array(WCA_crit_P)/P_c_GCMC
    WCA_crit_rho = np.array(WCA_crit_rho)/rho_c_GCMC

    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, sharex="all")
    plt.subplots_adjust(hspace=0)

    ax1.plot(BH_order, BH_crit_T, color="k", label="BH", marker="o")
    ax1.plot(WCA_order, WCA_crit_T, color="grey", label="WCA", marker="s")
    ax1.set_ylabel(r"$T^*_{\rm{c}}/T^*_{\rm{c,MC}}$")
    ax1.set_ylim((0.95,1.3))
    ax1.axhline(1.0, ls="--", color="k", lw=1.0)
    custom_legends = [Line2D([0], [0], marker='o', color='k', linestyle="None", label='BH'),
                      Line2D([0], [0], marker='s', color='grey', linestyle="None", label='WCA')]
    leg = ax1.legend(handles=custom_legends, loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)

    ax2.plot(BH_order, BH_crit_P, color="k", label="BH", marker="o")
    ax2.plot(WCA_order, WCA_crit_P, color="grey", label="WCA", marker="s")
    ax2.set_ylabel(r"$P^*_{\rm{c}}/P^*_{\rm{c,MC}}$")
    ax2.set_ylim((0.8,2.6))
    ax2.axhline(1.0, ls="--", color="k", lw=1.0)

    ax3.plot(BH_order, BH_crit_rho, color="k", label="BH", marker="o")
    ax3.plot(WCA_order, WCA_crit_rho, color="grey", label="WCA", marker="s")
    ax3.set_ylabel(r"$\rho^*_{\rm{c}}/\rho^*_{\rm{c,MC}}$")
    ax3.set_ylim((0.93,1.13))
    ax3.axhline(1.0, ls="--", color="k", lw=1.0)

    ax3.xaxis.set_major_locator(MaxNLocator(integer=True))
    fig.text(0.5, 0.03, "order of perturbation theory", ha='center', va='center')
    plt.suptitle("Figure 3: BH and WCA critical properties")


def plot_figure_4(LJS, labels):
    z = np.array([1.0])
    colors = ["orange", "b", "g", "k", "r"]

    assert len(LJS) == 3
    plt.figure()
    ax1 = plt.subplot(1, 3, 1)
    ax2 = plt.subplot(1, 3, 2)
    ax3 = plt.subplot(1, 3, 3)

    # Plot phase envelope
    for i, ljs in enumerate(LJS):
        sigma, eps = ljs.get_sigma_eps()
        T, P, v = ljs.get_envelope_twophase(5.0e3, z, maximum_pressure=1.0e7, calc_v=True)
        T_s = calc_reduced_T(T, eps)
        rho_s = calc_reduced_rho(1.0/v, sigma)
        P_s = calc_reduced_P(P, eps, sigma)
        ax1.plot(T_s, P_s, color=colors[i], label=labels[i])
        ax2.plot(rho_s, P_s, color=colors[i], label=labels[i])
        ax3.plot(rho_s, T_s, color=colors[i], label=labels[i])

    ax1.set_ylabel(r"$P^*$")
    ax1.set_xlabel(r"$T^*$")
    ax1.set_ylim((0.0,0.12))
    ax1.set_xlim((0.55,1.0))
    ax2.set_ylabel(r"$P^*$")
    ax2.set_xlabel(r"$\rho^*$")
    ax2.set_ylim((0.0,0.12))
    ax3.set_ylabel(r"$T^*$")
    ax3.set_xlabel(r"$\rho^*$")
    ax3.set_ylim((0.55,1.0))
    leg = ax1.legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)
    plt.suptitle("Figure 4: Phase diagrams")


def plot_figure_7(LJS, labels):
    # Plot Joule-Thompson inversion curves
    z = np.array([1.0])
    colors = ["orange", "b", "g"]
    plt.figure()
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
    plt.title("Figure 7: Joule-Thompson inversion curves")


def plot_figure_8(LJS, labels):
    z = np.array([1.0])
    colors = ["orange", "b", "g", "k", "r"]
    n = 100
    Ts = [0.7, 1.0, 1.46]
    rhos = [np.linspace(1.0e-3, 0.85, n),
            np.linspace(1.0e-3, 0.9, n),
            np.linspace(1.0e-3, 1.0, n)]

    assert len(LJS) == 3

    fig, ax_all = plt.subplots(2, 3, sharey="row", sharex="col")
    plt.subplots_adjust(wspace=0, hspace=0.1)
    ax = ax_all[0]

    # Plot reduced isochoric heat-capcity
    for iT, Tsi in enumerate(Ts):
        for i, ljs in enumerate(LJS):
            sigma, eps = ljs.get_sigma_eps()
            Ti = calc_real_T(np.array([Tsi]), eps)
            rho = calc_real_rho(rhos[iT], sigma)
            Cv = np.zeros_like(rho)
            for ir, r in enumerate(rho):
                u, Cv[ir] = ljs.internal_energy_tv(Ti, 1/r, z,
                                                   dedt=True,
                                                   property_flag="R")
            Cv_s = calc_reduced_heat_capacity(Cv)
            ax[iT].plot(rhos[iT], Cv_s, color=colors[i], label=labels[i])

    ax[0].set_ylim((0.0,2.4))
    ax[0].set_ylabel(r"$C_V^{\rm{res}}/Nk_{\rm{B}}$")
    leg = ax[0].legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)

    ax = ax_all[1]
    rhos = [np.concatenate((np.linspace(1.0e-3, 0.095, int(n/2)),
                            np.linspace(0.61, 0.85, int(n/2)))),
            np.linspace(1.0e-3, 0.9, n),
            np.linspace(1.0e-3, 1.0, n)]

    # Plot reduced isobaric heat-capcity
    for iT, Tsi in enumerate(Ts):
        for i, ljs in enumerate(LJS):
            sigma, eps = ljs.get_sigma_eps()
            Ti = calc_real_T(np.array([Tsi]), eps)
            rho = calc_real_rho(rhos[iT], sigma)
            Cp = np.zeros_like(rho)
            for ir, r in enumerate(rho):
                h, Cp[ir] = ljs.enthalpy_tvp(Ti, 1/r, z,
                                             dhdt=True,
                                             property_flag="R")
            Cp_s = calc_reduced_heat_capacity(Cp)
            ax[iT].plot(rhos[iT], Cp_s, color=colors[i], label=labels[i])

    ax[0].set_ylim((0.0,15.0))
    ax[0].set_ylabel(r"$C_P^{\rm{res}}/Nk_{\rm{B}}$")
    for i, axi in enumerate(ax):
        axi.set_xlim((0.0,rhos[i][-1]))

    leg = ax[0].legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)
    fig.text(0.5, 0.03, r"$\rho^*$", ha='center', va='center')
    plt.suptitle("Figure 8: Isochoric and isobaric heat capacity")


def plot_figure_9(LJS, labels):
    z = np.array([1.0])
    colors = ["orange", "b", "g"]
    linestyle = ["-", "--", ":"]
    n = 100
    Ts = np.linspace(0.65, 0.85, 5)
    rhos = np.linspace(1.0e-3, 0.8, n)
    assert len(LJS) == 3

    fig, ax = plt.subplots(1, 2)

    p_s_array = []
    # Plot reduced isotherms
    for iT, Tsi in enumerate(Ts):
        for i, ljs in enumerate(reversed(LJS)):
            j = 2 - i
            sigma, eps = ljs.get_sigma_eps()
            Ti = calc_real_T(np.array([Tsi]), eps)
            rho = calc_real_rho(rhos, sigma)
            p = np.zeros_like(rho)
            for ir, r in enumerate(rho):
                p[ir], = ljs.pressure_tv(Ti, 1/r, z)
            p_s = calc_reduced_P(p, eps, sigma)
            p_s_array.append(p_s)
            if iT == 0:
                label = labels[j]
            else:
                label = None
            ax[0].plot(rhos, p_s, color=colors[j], linestyle=linestyle[j], label=label)

    ax[0].set_ylabel(r"$p^*$")
    ax[0].set_xlabel(r"$\rho^*$")
    ax[0].set_ylim((-0.3,0.4))
    ax[0].set_xlim((0.0,0.8))
    leg = ax[0].legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)


    for ip, psi in enumerate(p_s_array):
        j = 2 - ip % 3
        ax[1].plot(rhos, psi, color=colors[j], linestyle=linestyle[j])

    ax[1].set_ylabel(r"$p^*$")
    ax[1].set_xlabel(r"$\rho^*$")
    ax[1].set_ylim((0.0,0.08))
    ax[1].set_xlim((0.0,0.2))

    plt.suptitle("Figure 9: Isotherms")


if __name__ == '__main__':
    # Generate plots from article:
    # Perturbation theories for fluids with short-ranged attractive forces: A case study of the Lennard-Jones spline fluid
    # doi:
    #
    # Instanciate and init LJS objects
    uv = ljs_wca.ljs_uv()
    uv.init()
    uv.set_tmin(temp=2.0)
    LJS_BH, labels_BH = get_BH_models()
    LJS_WCA, labels_WCA = get_WCA_models()

    # Define model set of UV, WCA4 and BH3
    LJS = [uv, LJS_WCA[0], LJS_BH[0]]
    labels_LJS = ["UV-theory", "WCA4", "BH3"]

    # Figure 2
    plot_figure_2(LJS_BH, LJS_WCA)

    # Figure 3
    plot_figure_3(LJS_BH, LJS_WCA)

    # Figure 4
    plot_figure_4(LJS, labels=labels_LJS)

    # Figure 7
    plot_figure_7(LJS, labels=labels_LJS)

    # Figure 8
    plot_figure_8(LJS, labels=labels_LJS)

    # Figure 9
    plot_figure_9(LJS, labels=labels_LJS)

    # Show figures
    plt.show()
