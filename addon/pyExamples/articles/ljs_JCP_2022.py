#!/usr/bin/python3
#
# Plots from:
# Thijs van Westen,Morten Hammer,Bjørn Hafskjold,Ailo Aasen,Joachim Gross, Øivind Wilhelmsen
# Perturbation theories for fluids with short-ranged attractive forces: A case study of the Lennard-Jones spline fluid
# doi: 10.1063/5.0082690
#
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


# def get_GCMC_sat_data():
#     """ GCMC-histogram-reweighting simulations:
#     Perturbation theories for fluids with short-ranged attractive forces: A case
#     study of the Lennard-Jones spline fluid
#     Thijs van Westen, Morten Hammer, Bjørn Hafskjold, Ailo Aasen, Joachim Gross, and 
#     Øivind Wilhelmsen
#     doi: 10.1063/5.0082690
#     """
#     T = np.array([])
#     rhol = np.array([])
#     rhog = np.array([])
#     p = np.array([])
#     data = {}
#     data["T"] = T
#     data["P"] = P
#     data["rhol"] = rhol
#     data["rhog"] = rhog
#     return data


def get_MD_isotherms():
    """ MD results for P-V isotherms:
    Perturbation theories for fluids with short-ranged attractive forces: A case
    study of the Lennard-Jones spline fluid
    Thijs van Westen, Morten Hammer, Bjørn Hafskjold, Ailo Aasen, Joachim Gross, and 
    Øivind Wilhelmsen
    doi: 10.1063/5.0082690
    """
    data = {}
    data["T=0.65"] = {}
    data["T=0.65"]["rho"] = np.array([0.000, 0.005, 0.010, 0.015, 0.020,
                                      0.025, 0.030, 0.035, 0.680, 0.690,
                                      0.700, 0.710, 0.720, 0.730, 0.740,
                                      0.750, 0.760, 0.770, 0.780, 0.790,
                                      0.800])
    data["T=0.65"]["p"] = np.array([0.0000, 0.0031, 0.0060, 0.0086, 0.0110,
                                    0.0131, 0.0149, 0.0165, -0.2699, -0.2589,
                                    -0.2331, -0.2028, -0.1627, -0.1124, -0.0540,
                                    0.0159, 0.0972, 0.1906, 0.2955, 0.4120,
                                    0.5409])
    data["T=0.70"] = {}
    data["T=0.70"]["rho"] = np.array([0.000, 0.005, 0.010, 0.015, 0.020,
                                      0.025, 0.030, 0.035, 0.040, 0.045,
                                      0.050, 0.650, 0.660, 0.670, 0.680,
                                      0.690, 0.700, 0.710, 0.720, 0.730,
                                      0.740, 0.750, 0.760, 0.770, 0.780,
                                      0.790, 0.800])
    data["T=0.70"]["p"] = np.array([0.000, 0.003, 0.007, 0.009, 0.012,
                                    0.015, 0.017, 0.019, 0.021, 0.023,
                                    0.024, -0.172, -0.162, -0.146, -0.125,
                                    -0.095, -0.059, -0.016, 0.035, 0.098,
                                    0.167, 0.247, 0.337, 0.443, 0.555,
                                    0.683, 0.826])
    data["T=0.75"] = {}
    data["T=0.75"]["rho"] = np.array([0.000, 0.005, 0.010, 0.015, 0.020,
                                      0.025, 0.030, 0.035, 0.040, 0.045,
                                      0.050, 0.055, 0.060, 0.065, 0.070,
                                      0.075, 0.080, 0.610, 0.620, 0.630,
                                      0.640, 0.650, 0.660, 0.670, 0.680,
                                      0.690, 0.700, 0.710, 0.720, 0.730,
                                      0.740, 0.750, 0.760, 0.770, 0.780,
                                      0.790, 0.800])
    data["T=0.75"]["p"] = np.array([0.000, 0.004, 0.007, 0.010, 0.013,
                                    0.016, 0.019, 0.021, 0.024, 0.026,
                                    0.027, 0.029, 0.031, 0.032, 0.033,
                                    0.034, 0.035, -0.088, -0.083, -0.076,
                                    -0.063, -0.047, -0.026, 0.000, 0.033,
                                    0.072, 0.117, 0.173, 0.231, 0.303,
                                    0.384, 0.473, 0.574, 0.686, 0.812,
                                    0.949, 1.098])
    data["T=0.80"] = {}
    data["T=0.80"]["rho"] = np.array([0.000, 0.005, 0.010, 0.015, 0.020,
                                      0.025, 0.030, 0.035, 0.040, 0.045,
                                      0.050, 0.055, 0.060, 0.065, 0.070,
                                      0.075, 0.080, 0.085, 0.090, 0.095,
                                      0.100, 0.105, 0.110, 0.115, 0.570,
                                      0.580, 0.590, 0.600, 0.610, 0.620,
                                      0.630, 0.640, 0.650, 0.660, 0.670,
                                      0.680, 0.690, 0.700, 0.710, 0.720,
                                      0.730, 0.740, 0.750, 0.760, 0.770,
                                      0.780, 0.790, 0.800])
    data["T=0.80"]["p"] = np.array([0.000, 0.004, 0.008, 0.011, 0.015,
                                    0.018, 0.021, 0.023, 0.026, 0.029,
                                    0.031, 0.033, 0.035, 0.037, 0.039,
                                    0.040, 0.041, 0.043, 0.044, 0.045,
                                    0.046, 0.046, 0.047, 0.047, -0.016,
                                    -0.011, -0.005, 0.001, 0.012, 0.024,
                                    0.041, 0.062, 0.087, 0.117, 0.152,
                                    0.194, 0.240, 0.296, 0.357, 0.429,
                                    0.508, 0.599, 0.696, 0.806, 0.927,
                                    1.060, 1.207, 1.365])
    data["T=0.85"] = {}
    data["T=0.85"]["rho"] = np.array([0.000, 0.005, 0.010, 0.015, 0.020,
                                      0.025, 0.030, 0.035, 0.040, 0.045,
                                      0.050, 0.055, 0.060, 0.065, 0.070,
                                      0.075, 0.080, 0.085, 0.090, 0.095,
                                      0.100, 0.105, 0.110, 0.115, 0.120,
                                      0.125, 0.130, 0.135, 0.140, 0.145,
                                      0.150, 0.160, 0.170, 0.180, 0.190,
                                      0.510, 0.520, 0.530, 0.540, 0.550,
                                      0.560, 0.570, 0.580, 0.590, 0.600,
                                      0.610, 0.620, 0.630, 0.640, 0.650,
                                      0.660, 0.670, 0.680, 0.690, 0.700,
                                      0.710, 0.720, 0.730, 0.740, 0.750,
                                      0.760, 0.770, 0.780, 0.790, 0.800])
    data["T=0.85"]["p"] = np.array([0.000, 0.004, 0.008, 0.012, 0.016,
                                    0.019, 0.022, 0.026, 0.029, 0.031,
                                    0.034, 0.037, 0.039, 0.041, 0.044,
                                    0.045, 0.047, 0.049, 0.051, 0.052,
                                    0.054, 0.055, 0.056, 0.057, 0.058,
                                    0.059, 0.060, 0.060, 0.061, 0.062,
                                    0.062, 0.063, 0.063, 0.064, 0.064,
                                    0.043, 0.045, 0.048, 0.051, 0.056,
                                    0.060, 0.068, 0.076, 0.089, 0.104,
                                    0.121, 0.140, 0.164, 0.193, 0.224,
                                    0.261, 0.304, 0.352, 0.409, 0.474,
                                    0.543, 0.622, 0.708, 0.806, 0.914,
                                    1.031, 1.162, 1.304, 1.462, 1.630])
    return data

# def get_GCMC_heat_cap_data():
#     """ GCMC-histogram-reweighting simulations:
#     Perturbation theories for fluids with short-ranged attractive forces: A case
#     study of the Lennard-Jones spline fluid
#     Thijs van Westen, Morten Hammer, Bjørn Hafskjold, Ailo Aasen, Joachim Gross, and 
#     Øivind Wilhelmsen
#     doi: 10.1063/5.0082690
#     """
#     T = np.array([])
#     Cv = np.array([])
#     Cp = np.array([])
#     data = {}
#     data["T"] = T
#     data["Cv"] = Cv
#     data["Cp"] = Cp
#     return data

def get_MD_heat_cap_data():
    """ MD data for saturation densities:
    Thermodynamic properties of the 3D Lennard-Jones/spline model
    Bjørn Hafskjold and Karl Patrick Travis and Amanda Bailey Hass and
    Morten Hammer and Ailo Aasen and Øivind Wilhelmsen
    doi: 10.1080/00268976.2019.1664780
    """

    data = {}
    data["T=0.7"] = {"rho": np.array([0.02, 0.80]),
                     "Cp": np.array([3.65-2.5, 4.88-2.5]),
                     "Cv": np.array([1.93-1.5, 2.59-1.5])}
    # Actually T=0.991
    data["T=1.0"] = {"rho": np.array([0.40]),
                     "Cp": np.array([14.22-2.5]),
                     "Cv": np.array([2.56-1.5])}

    data["T=1.46"] = {"rho": np.array([]),
                     "Cp": np.array([]),
                     "Cv": np.array([])}
    return data


def get_GEMC_sat_data():
    """ GEMC data for saturation curve:
    Thermodynamic properties of the 3D Lennard-Jones/spline model
    Bjørn Hafskjold and Karl Patrick Travis and Amanda Bailey Hass and
    Morten Hammer and Ailo Aasen and Øivind Wilhelmsen
    doi: 10.1080/00268976.2019.1664780
    """
    T = np.array([0.55, 0.6, 0.625, 0.65, 0.675, 0.7, 0.725,
                  0.75, 0.775, 0.8, 0.825, 0.85])
    rhol = np.array([0.80408, 0.778, 0.763955, 0.749004, 0.733098,
                     0.715797, 0.697191, 0.676001, 0.652946, 0.625045,
                     0.591004, 0.544877])
    rhog = np.array([0.004001, 0.008285, 0.01149, 0.015399, 0.02054,
                     0.026996, 0.035112, 0.045018, 0.058921, 0.07528,
                     0.099615, 0.135605])
    p = np.array([0.002106, 0.004607, 0.006513, 0.008856, 0.011899,
                  0.015656, 0.020214, 0.025602, 0.032361, 0.039875,
                  0.049038, 0.059711])

    data = {}
    data["T"] = T
    data["P"] = p
    data["rhol"] = rhol
    data["rhog"] = rhog
    return data


def get_MD_sat_data():
    """ MD data for saturation densities:
    Thermodynamic properties of the 3D Lennard-Jones/spline model
    Bjørn Hafskjold and Karl Patrick Travis and Amanda Bailey Hass and
    Morten Hammer and Ailo Aasen and Øivind Wilhelmsen
    doi: 10.1080/00268976.2019.1664780
    """
    T = np.array([0.5507, 0.5529, 0.5976, 0.6028, 0.6465, 0.6513,
                  0.7000, 0.7004, 0.7022, 0.7500, 0.7501, 0.7533,
                  0.8000, 0.8000, 0.8168, 0.8500])
    rhol = np.array([0.8038, 0.8033, 0.7780, 0.7772, 0.7510, 0.7479,
                     0.7143, 0.7148, 0.7140, 0.6738, 0.6733, 0.6729,
                     0.6225, 0.6218, 0.6023, 0.5324])
    rhog = np.array([0.0041, 0.0036, 0.0082, 0.0084, 0.0150, 0.0142,
                     0.0265, 0.0280, 0.0271, 0.0501, 0.0481, 0.0467,
                     0.0828, 0.0769, 0.0963, 0.1435])
    data = {}
    data["T"] = T
    data["rhol"] = rhol
    data["rhog"] = rhog
    return data

def get_MD_psat():
    """ MD data for saturation densities:
    Thermodynamic properties of the 3D Lennard-Jones/spline model
    Bjørn Hafskjold and Karl Patrick Travis and Amanda Bailey Hass and
    Morten Hammer and Ailo Aasen and Øivind Wilhelmsen
    doi: 10.1080/00268976.2019.1664780
    """
    T = np.array([0.5501, 0.5499, 0.5496, 0.5997, 0.6500, 0.7000, 0.7504,
                  0.8000, 0.8202, 0.8407, 0.8596, 0.8688, 0.8771, 0.8775,
                  0.6898, 0.7723, 0.8070, 0.8407, 0.8437, 0.8570, 0.8687,
                  0.8723, 0.8762, 0.8770])
    p = np.array([0.002158, 0.002084, 0.002123, 0.004656, 0.008804, 0.015332,
                  0.025052, 0.038927, 0.045588, 0.054326, 0.063949, 0.069529,
                  0.075501, 0.075752, 0.014112, 0.031532, 0.042154, 0.055300,
                  0.056660, 0.062675, 0.070558, 0.070944, 0.072616, 0.073748])
    data = {}
    data["T"] = T
    data["P"] = P
    return data


def get_crit_point():
    """Critical data from MD/GEMC/GCMC:
    doi: 10.1080/00268976.2019.1664780
    doi: 10.1063/5.0082690
    """
    data = {}
    data["MD"] = {"rho": 0.332, "T": 0.882, "P": 0.074}
    data["GEMC"] = {"rho": 0.333, "T": 0.884, "P": 0.076}
    data["GCMC"] = {"rho": 0.333, "T": 0.8796, "P": 0.07451}
    return data


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

    custom_legends = [Line2D([0], [0], color='k', linestyle="None", label='BH'),
                      Line2D([0], [0], color='b', linestyle="None", marker="v", label='GEMC', mfc="None"),
                      Line2D([0], [0], color='r', linestyle="None", marker="x", label='MD')]
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

    MD_data = get_MD_sat_data()
    ax1.plot(MD_data["rhol"], MD_data["T"], color="r", marker="x", linestyle="None")
    ax1.plot(MD_data["rhog"], MD_data["T"], color="r", marker="x", linestyle="None")
    ax2.plot(MD_data["rhol"], MD_data["T"], color="r", marker="x", linestyle="None")
    ax2.plot(MD_data["rhog"], MD_data["T"], color="r", marker="x", linestyle="None")

    GEMC_data = get_GEMC_sat_data()
    ax1.plot(GEMC_data["rhol"], GEMC_data["T"], color="b", marker="v", linestyle="None", mfc="None")
    ax1.plot(GEMC_data["rhog"], GEMC_data["T"], color="b", marker="v", linestyle="None", mfc="None")
    ax2.plot(GEMC_data["rhol"], GEMC_data["T"], color="b", marker="v", linestyle="None", mfc="None")
    ax2.plot(GEMC_data["rhog"], GEMC_data["T"], color="b", marker="v", linestyle="None", mfc="None")

    crit_data = get_crit_point()
    ax1.plot(crit_data["MD"]["rho"], crit_data["MD"]["T"], color="r", marker="x", linestyle="None")
    ax2.plot(crit_data["MD"]["rho"], crit_data["MD"]["T"], color="r", marker="x", linestyle="None")
    ax1.plot(crit_data["GCMC"]["rho"], crit_data["GCMC"]["T"], color="b", marker="v", linestyle="None")
    ax2.plot(crit_data["GCMC"]["rho"], crit_data["GCMC"]["T"], color="b", marker="v", linestyle="None")

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

    crit_data = get_crit_point()
    ax1.plot(crit_data["MD"]["T"], crit_data["MD"]["P"], color="r", marker="v", linestyle="None", label="MD")
    ax2.plot(crit_data["MD"]["rho"], crit_data["MD"]["P"], color="r", marker="v", linestyle="None")
    ax3.plot(crit_data["MD"]["rho"], crit_data["MD"]["T"], color="r", marker="v", linestyle="None")
    ax1.plot(crit_data["GCMC"]["T"], crit_data["GCMC"]["P"], color="k", marker="s", linestyle="None", label="GCMC")
    ax2.plot(crit_data["GCMC"]["rho"], crit_data["GCMC"]["P"], color="k", marker="s", linestyle="None")
    ax3.plot(crit_data["GCMC"]["rho"], crit_data["GCMC"]["T"], color="k", marker="s", linestyle="None")

    GEMC_data = get_GEMC_sat_data()
    ax1.plot(GEMC_data["T"], GEMC_data["P"], color="r", marker="d", linestyle="None", mfc="None", label="GEMC")
    ax2.plot(GEMC_data["rhog"], GEMC_data["P"], color="r", marker="d", linestyle="None", mfc="None")
    ax2.plot(GEMC_data["rhol"], GEMC_data["P"], color="r", marker="d", linestyle="None", mfc="None")
    ax3.plot(GEMC_data["rhog"], GEMC_data["T"], color="r", marker="d", linestyle="None", mfc="None")
    ax3.plot(GEMC_data["rhol"], GEMC_data["T"], color="r", marker="d", linestyle="None", mfc="None")

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

    Ps_NEMD = np.array([0.0000, 0.1942, 0.3752, 0.3748, 0.5394, 0.6832, 0.6848,
                        0.8017, 0.8992, 0.9073, 0.9616, 0.9843, 0.9959, 0.9886,
                        0.9914, 0.9219, 0.7832, 0.7794, 0.6716, 0.6751, 0.5592,
                        0.5035, 0.4686, 0.4615, 0.3480, 0.3425, 0.2744, 0.2744,
                        0.2059, 0.1246, 0.1269])

    Ts_NEMD = np.array([4.0153, 3.6905, 3.4845, 3.4590, 3.2859, 2.9665, 2.9638,
                    2.6978, 2.4962, 2.5056, 2.2587, 2.1030, 1.9156, 1.7352,
                    1.6945, 1.5182, 1.2927, 1.2876, 1.1644, 1.1646, 1.0606,
                    1.0100, 0.9945, 0.9868, 0.9120, 0.9078, 0.8710, 0.8650,
                    0.8307, 0.7885, 0.7833])

    plt.plot(Ps_NEMD, Ts_NEMD, color="k", label="NEMD", linestyle="None",
             marker="o", mfc="None")

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

    MD_heat_data = get_MD_heat_cap_data()
    ax[0].plot(MD_heat_data["T=0.7"]["rho"], MD_heat_data["T=0.7"]["Cv"],
               color="k", label="MD", marker="o", mfc="None", linestyle="None")
    ax[1].plot(MD_heat_data["T=1.0"]["rho"], MD_heat_data["T=1.0"]["Cv"],
               color="k", marker="o", mfc="None", linestyle="None")

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

    ax[0].plot(MD_heat_data["T=0.7"]["rho"], MD_heat_data["T=0.7"]["Cp"],
               color="k", marker="o", mfc="None", linestyle="None")
    ax[1].plot(MD_heat_data["T=1.0"]["rho"], MD_heat_data["T=1.0"]["Cp"],
               color="k", marker="o", mfc="None", linestyle="None")

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
    markers = ["o", "o", "p", "p", "v"]
    markerfill = ["full", "none", "full", "none", "full"]
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

    md_data = get_MD_isotherms()
    for iT, T in enumerate(Ts):
        label = "T={:.2f}".format(T)
        data = md_data[label]
        ax[0].plot(data["rho"], data["p"], marker=markers[iT],
                   fillstyle=markerfill[iT], label="{:.2f}".format(T),
                   color="k", linestyle="None")

    print("Update")
    data = md_data["T=0.70"]
    ax[0].plot(data["rho"], data["p"], marker="o",
               fillstyle="none", label=r"0.7 ($1/N \rightarrow 0$)",
               color="magenta", linestyle="None")

    leg = ax[0].legend(loc="best", numpoints=1)
    leg.get_frame().set_linewidth(0.0)

    for ip, psi in enumerate(p_s_array):
        j = 2 - ip % 3
        ax[1].plot(rhos, psi, color=colors[j], linestyle=linestyle[j])

    for iT, T in enumerate(Ts):
        label = "T={:.2f}".format(T)
        data = md_data[label]
        ax[1].plot(data["rho"], data["p"], marker=markers[iT],
                   fillstyle=markerfill[iT], color="k",
                   linestyle="None")

    print("Update")
    data = md_data["T=0.70"]
    ax[1].plot(data["rho"], data["p"], marker="o",
               fillstyle="none", label=r"0.7 ($1/N \rightarrow 0$)",
               color="magenta", linestyle="None")

    ax[1].set_ylabel(r"$p^*$")
    ax[1].set_xlabel(r"$\rho^*$")
    ax[1].set_ylim((0.0,0.08))
    ax[1].set_xlim((0.0,0.2))

    plt.suptitle("Figure 9: Isotherms")


if __name__ == '__main__':
    # Generate plots from article:
    # Perturbation theories for fluids with short-ranged attractive forces: A case study of the Lennard-Jones spline fluid
    # doi: 10.1063/5.0082690
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
    #plot_figure_2(LJS_BH, LJS_WCA)

    # Figure 3
    #plot_figure_3(LJS_BH, LJS_WCA)

    # Figure 4
    #plot_figure_4(LJS, labels=labels_LJS)

    # Figure 7
    #plot_figure_7(LJS, labels=labels_LJS)

    # Figure 8
    plot_figure_8(LJS, labels=labels_LJS)

    # Figure 9
    plot_figure_9(LJS, labels=labels_LJS)

    # Show figures
    plt.show()
