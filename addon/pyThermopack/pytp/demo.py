"""Simple examples on how to use pytp."""

from __future__ import print_function

import numpy as np
import matplotlib.pyplot as plt

from pytp import tp
from pytp import const
from pytp import utils


SHOWPLOTS = True
SAVEPLOTS = True


def simple_tp_init(complist=("CO2", "N2")):
    nc = len(complist)
    compstr = ",".join(complist)
    nph = nc+1
    tp.init("Thermopack", "SRK", "Classic", "Classic", nc, compstr, nph, 1, 1)


def test_tpflash_twophase(ngrid=100, Tmin=200.0, Tmax=350.0, Pmin=10e5,
                          Pmax=100e5, complist=("CO2", "N2"), z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Do TP-flashes
    print("---Doing %s TP-flashes" % (ngrid**2))
    phasegrid = np.zeros((ngrid, ngrid))
    betagrid = np.zeros((ngrid, ngrid))
    beta_guess = 0.5

    for i in range(ngrid):
        for j in range(ngrid):
            flashresult = tp.tpflash_twophase(Tvals[i], pvals[j], z,
                                              beta_guess)
            beta, dummy_betal, phase, dummy_x, dummy_y = flashresult
            if phase > 4:
                print("Invalid phase flag at:", Tvals[i], pvals[j])
                phase = -1
                phasegrid[j, i] = phase
                betagrid[j, i] = beta
    print("Done. Making plot...")

    # Example plot: Get a grid which is beta in the two-phase area, and -1 outside
    betatwophase_grid = np.ones((ngrid, ngrid))*(-1)
    betatwophase_grid[phasegrid == 0] = betagrid[phasegrid == 0]

    # Make T,P grid
    Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # Make beta plot
    plt.figure()
    plt.contourf(Tgrid, pgrid*1e-5, betatwophase_grid,
                 levels=np.linspace(0.0, 1.0, 9))
    plt.colorbar()
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(r"$\beta$")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_tpflash_twophase_beta.pdf")

    # Make phase flag plot
    plt.figure()
    plt.contourf(Tgrid, pgrid*1e-5, phasegrid,
                 levels=(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5))
    plt.colorbar()
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_tpflash_twophase_phase_flag.pdf")

    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_density(ngrid=200, Tmin=200.0, Tmax=350.0, Pmin=10e5, Pmax=100e5,
                 complist=("CO2", "N2"), z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Get liquid and vapor densities
    print("---Calling specific_volume %s times" % (2*ngrid**2))
    rholiq_grid = np.zeros((ngrid, ngrid))
    rhovap_grid = np.zeros((ngrid, ngrid))

    for i in range(ngrid):
        for j in range(ngrid):
            results_liq = tp.specific_volume(Tvals[i], pvals[j], z,
                                             const.LIQPH)
            results_vap = tp.specific_volume(Tvals[i], pvals[j], z,
                                             const.VAPPH)
            rholiq_grid[j, i] = 1.0/results_liq[0]
            rhovap_grid[j, i] = 1.0/results_vap[0]
    print("Done. Making plot...")

    # Make T,P grid
    Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # Making nice contour levels
    maxlevel_ithousand = int(np.max(rholiq_grid)/1000.0) + 1
    maxlevel_rho = 1000.0*maxlevel_ithousand
    levels = np.linspace(0.0, maxlevel_rho, maxlevel_ithousand+1)

    # Make plots
    plt.subplot(1, 2, 1)
    plt.contourf(Tgrid, pgrid*1e-5, rholiq_grid, levels=levels)
    plt.colorbar()
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(r"$\rho_\mathrm{l}$ ($\mathrm{mol}/\mathrm{m}^3$)")

    plt.subplot(1, 2, 2)
    plt.contourf(Tgrid, pgrid*1e-5, rhovap_grid, levels=levels)
    plt.colorbar()
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(r"$\rho_\mathrm{v}$ ($\mathrm{mol}/\mathrm{m}^3$)")

    # Saving pdf
    if SAVEPLOTS:
        plt.gcf().set_size_inches(18, 6)
        plt.savefig("test_density.pdf")
    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_phase_env(T_init=220.0, p_init=10e5, complist=("CO2", "N2"),
                   z=(0.98, 0.02)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    # Mixture:
    z = np.array(z)

    Tvals, pvals, Kvals, beta_vals = utils.get_envelope_twophase(T_init, p_init, z)

    plt.figure()
    plt.subplot(311)
    plt.plot(Kvals[:, 0])
    plt.subplot(312)
    plt.plot(Kvals[:, 1])
    plt.subplot(313)
    plt.plot(beta_vals)

    plt.figure()

    pvals *= 1e-5

    plt.plot(Tvals[beta_vals == 1.0], pvals[beta_vals == 1.0], "b.-",
             label="Tiny amount of liquid")
    plt.plot(Tvals[beta_vals == 0.0], pvals[beta_vals == 0.0], "r.-",
             label="Tiny amount of vapor")
    plt.legend(loc="upper left")

    plt.xlim(T_init, np.max(Tvals)+20.0)

    # Configuring plot:
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.grid()

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_phase_env.pdf")
    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_print_cubic_params(T=280.0, P=20e5, complist=("CO2", "N2", "C1"),
                            z=(0.90, 0.05, 0.05)):
    simple_tp_init(complist)
    tp.print_cubic_params(T, P, z)


def test_phflash_twophase(ngrid=60, Pmin=2e5, Pmax=100e5, hmin=-10000.0,
                          hmax=10000.0, complist=("CO2", "N2"),
                          z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    pvals = np.linspace(Pmin, Pmax, ngrid)
    hvals = np.linspace(hmin, hmax, ngrid)
    phasegrid = np.zeros((ngrid, ngrid))
    Tgrid = np.zeros((ngrid, ngrid))

    print("---Doing %s ph-flashes" % (ngrid**2))
    beta_guess = 0.5
    T_guess = 300.0
    for i in range(ngrid):
        for j in range(ngrid):
            flashresult = tp.phflash_twophase(pvals[j], hvals[i], z, T_guess,
                                              beta_guess)
            Tgrid[j, i] = flashresult[0]
            phasegrid[j, i] = flashresult[3]

    print("Done. Making plot...")

    # Make T,P grid
    hgrid, pgrid = np.meshgrid(hvals, pvals)
    pgrid = pgrid*1e-5

    # Making n-phases field
    nphasegrid = np.ones((ngrid, ngrid))
    nphasegrid[phasegrid == 0] = 2.0

    # Plotting colored T-contours
    plt.contourf(hgrid, pgrid, Tgrid)

    # Plotting labelled isotherms
    cont = plt.contour(hgrid, pgrid, Tgrid, colors="k")
    plt.clabel(cont, inline=True)

    # Plotting phase envelope
    plt.contour(hgrid, pgrid, nphasegrid, colors="k", levels=[0.5, 1.5], linewidths=3)

    # Making labels
    plt.xlabel(r"$h-h_0$ (J/mol)")
    plt.ylabel(r"$P$ (bar)")
    plt.title("Isotherms and phase envelope")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_phflash.pdf")
    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def _plot_envelope_kij(z, i, j, kij):
    tp.set_kij(i, j, kij)
    tp.set_kij(j, i, kij)
    Tmin = 250.0
    Pinit = 60e5
    Tvals, pvals, dummy_Kvals, dummy_beta_vals \
            = utils.get_envelope_twophase(Tmin, Pinit, z)
    pvals *= 1e-5
    plt.plot(Tvals, pvals, ".-", label=("kij=%.4f" % kij))


def _plot_envelope_fromflash_kij(z, i, j, kij):
    tp.set_kij(i, j, kij)
    tp.set_kij(j, i, kij)
    Tmin = 250.0
    Tmax = 310.0
    Pmin = 10e5
    Pmax = 100e5
    ngrid = 100
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Do TP-flashes
    phasegrid = np.zeros((ngrid, ngrid))
    beta_guess = 0.5
    for n in range(ngrid):
        for m in range(ngrid):
            flashresult = tp.tpflash_twophase(Tvals[n], pvals[m], z, beta_guess)
            dummy_beta, dummy_betal, phase, dummy_x, dummy_y = flashresult
            phasegrid[m, n] = phase
    print("Done. Making plot...")

    # Make nphases grid
    nph = np.ones_like(phasegrid)
    nph[phasegrid == 0.0] = 2.0

    # Make T,P grid
    Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # Make plot
    c = plt.contour(Tgrid, pgrid*1e-5, nph, levels=[1.5, 2.5])
    c.collections[0].set_label('kij=%.4f' % kij)


def test_parameter_change(complist=("CO2", "N2"), z=np.array((0.95, 0.05))):
    # Checking input
    assert len(complist) == len(z)
    assert len(complist) == 2

    # Initializing Thermopack
    simple_tp_init(complist)

    # Standard kij
    kij = tp.get_kij(1, 2)

    # Custom kij
    _plot_envelope_kij(z, 1, 2, -0.5)
    _plot_envelope_kij(z, 1, 2, -0.4)
    _plot_envelope_kij(z, 1, 2, -0.3)
    _plot_envelope_kij(z, 1, 2, -0.2)
    _plot_envelope_kij(z, 1, 2, -0.1)
    _plot_envelope_kij(z, 1, 2, 0.0)

    _plot_envelope_kij(z, 1, 2, 0.001)
    _plot_envelope_kij(z, 1, 2, 0.005)
    _plot_envelope_kij(z, 1, 2, 0.02)
    _plot_envelope_kij(z, 1, 2, 0.04)
    _plot_envelope_kij(z, 1, 2, 0.05)

    # Configuring plot:
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.grid()
    plt.legend(loc="upper left")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_parameter_change.pdf")
    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_z_factor(ngrid=200, Tmin=200.0, Tmax=350.0, Pmin=10e5, Pmax=100e5,
                  complist=("CO2", "N2"), z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Get liquid and vapor densities
    print("---Calling z_factor %s times" % (2*ngrid**2))
    zfac_liq_grid = np.zeros((ngrid, ngrid))
    zfac_vap_grid = np.zeros((ngrid, ngrid))

    for i in range(ngrid):
        for j in range(ngrid):
            zfac_liq_grid[j, i] = tp.z_factor(Tvals[i], pvals[j], z,
                                              const.LIQPH)
            zfac_vap_grid[j, i] = tp.z_factor(Tvals[i], pvals[j], z,
                                              const.VAPPH)

    print("Done. Making plot...")

    # Make T,P grid
    Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # Making nice contour levels
    levels = np.linspace(0.0, np.max(zfac_vap_grid), 20)

    # Make plots
    plt.subplot(1, 2, 1)
    plt.contourf(Tgrid, pgrid*1e-5, zfac_liq_grid, levels=levels)
    plt.colorbar()
    plt.gca().contour(Tgrid, pgrid*1e-5, zfac_liq_grid, levels=[1.0/3.0],
                      linewidths=3, colors="k", linestyles="dashed")
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(r"$Z_\mathrm{liq}$")

    plt.subplot(1, 2, 2)
    plt.contourf(Tgrid, pgrid*1e-5, zfac_vap_grid, levels=levels)
    plt.colorbar()
    plt.gca().contour(Tgrid, pgrid*1e-5, zfac_vap_grid, levels=[1.0/3.0],
                      linewidths=3, colors="k", linestyles="dashed")
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")
    plt.title(r"$Z_\mathrm{vap}$")

    # Saving pdf
    if SAVEPLOTS:
        plt.gcf().set_size_inches(18, 6) # Widen figure
        plt.savefig("test_z_factor.pdf")
    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_guess_phase(ngrid=200, Tmin=200.0, Tmax=350.0, Pmin=10e5, Pmax=100e5,
                     complist=("CO2", "N2"), z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Get liquid and vapor densities
    print("---Calling guess_phase %s times" % (ngrid**2))
    guessphase_grid = np.zeros((ngrid, ngrid))

    for i in range(ngrid):
        for j in range(ngrid):
            guessphase_grid[j, i] = tp.guess_phase(Tvals[i], pvals[j], z)

    # print("Done. Making plot...")

    # # Make T, P grid
    # Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # # Saving pdf
    # if SAVEPLOTS:
    #     plt.savefig("test_guess_phase.pdf")
    # # Showing plot on screen, if possible
    # if SHOWPLOTS:
    #     plt.show()
    # plt.close()


def test_uvflash_twophase(ngrid=50, umin=-4000.0, umax=6000.0, vmin=6e-5,
                          vmax=1e-3, T_init=260.0, p_init=5e5,
                          complist=("CO2", "N2"), z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)
    z = np.array(z)

    T_guess = T_init
    p_guess = p_init
    beta_guess = 0.5

    uvals = np.linspace(umin, umax, ngrid)
    vvals = np.linspace(vmin, vmax, ngrid)

    # Do TP-flashes
    print("---Doing %s uv-flashes" % (ngrid**2))
    phasegrid = np.zeros((ngrid, ngrid))
    betagrid = np.zeros((ngrid, ngrid))
    beta_guess = 0.5
    x_guess = np.ones_like(z)*0.5
    y_guess = (z-(1.0-beta_guess)*x_guess)/beta_guess

    for i in range(ngrid):
        for j in range(ngrid):
            print(uvals[i], vvals[j])
            flashresult = tp.uvflash_twophase(uvals[i], vvals[j], z, T_guess,
                                              p_guess, beta_guess, x_guess,
                                              y_guess)

            T, p, beta, dummy_betal, phase, x, y = flashresult

            phasegrid[j, i] = phase
            betagrid[j, i] = beta

            T_guess = T
            p_guess = p
            beta_guess = beta
            x_guess = x
            y_guess = y

    print("Done. Making plot...")

    # Example plot: Get a grid which is beta in the two-phase area, and -1 outside
    betatwophase_grid = np.ones((ngrid, ngrid))*(-1)
    betatwophase_grid[phasegrid == 0] = betagrid[phasegrid == 0]

    # Make beta plot
    plt.figure()
    plt.contourf(uvals, vvals, betatwophase_grid,
                 levels=np.linspace(0.0, 1.0, 20))
    plt.colorbar()
    plt.xlabel(r"$u$ (J/mol)")
    plt.ylabel(r"$v$ (m3/mol)")
    plt.title(r"$\beta$")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_uvflash_twophase.pdf")

    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()


def test_tpflash_multiphase(ngrid=100, Tmin=200.0, Tmax=350.0, Pmin=10e5,
                            Pmax=100e5, complist=("CO2", "N2"),
                            z=(0.95, 0.05)):
    # Checking input
    assert len(complist) == len(z)

    # Initializing Thermopack
    simple_tp_init(complist)

    z = np.array(z)
    nc = len(z)
    Tvals = np.linspace(Tmin, Tmax, ngrid)
    pvals = np.linspace(Pmin, Pmax, ngrid)

    # Do TP-flashes
    print("---Doing %s multi-phase TP-flashes" % (ngrid**2))
    nphasegrid = np.zeros((ngrid, ngrid))

    for i in range(ngrid):
        for j in range(ngrid):
            flashresult = tp.tpflash_multiphase(nc+1, Tvals[i], pvals[j], z)
            nphasegrid[j, i] = flashresult[0]
    print("Done. Making plot...")

    # Make T, P grid
    Tgrid, pgrid = np.meshgrid(Tvals, pvals)

    # Make nphase plot
    plt.figure()
    #plt.contourf(Tgrid, pgrid*1e-5, phasegrid)
    plt.contourf(Tgrid, pgrid*1e-5, nphasegrid, levels=(0.5, 1.5, 2.5, 3.5, 4.5))
    plt.colorbar()
    plt.xlabel(r"$T$ (K)")
    plt.ylabel(r"$P$ (bar)")

    # Saving pdf
    if SAVEPLOTS:
        plt.savefig("test_tpflash_multiphase.pdf")

    # Showing plot on screen, if possible
    if SHOWPLOTS:
        plt.show()
    plt.close()
