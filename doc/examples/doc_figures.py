import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import numpy as np
from thermopack.cubic import cubic

eos = cubic('C1,NC6,CO2', 'PR')

z = [0.3, 0.5, 0.2]

M = sum([z[i] * eos.compmoleweight(i + 1) for i in range(len(z))])

to_kg = lambda vm: (1 / vm) * (1e-3 * M)

Tc, vc, pc = eos.critical(z)
Teq, peq, veq = eos.get_envelope_twophase(1e4, z, calc_v=True, maximum_pressure=1.25e7)

isobar_T = []
isobar_rho_vap = []
isobar_rho_liq = []
isobars = np.linspace(0.1 * pc, 2 * pc, 10)
T_list = np.linspace(min(Teq), 1.5 * Tc)
for i, p in enumerate(isobars):
    vg = []
    vl = []
    for T in T_list:
        if p < pc and T < Tc:
            _, _, _, _, ph = eos.two_phase_tpflash(T, p, z)
        else:
            ph = 1

        if ph == eos.TWOPH:
            vg.append(np.nan)
            vl.append(np.nan)
        else:
            vg.append(to_kg(eos.specific_volume(T, p, z, 2)[0]))
            vl.append(to_kg(eos.specific_volume(T, p, z, 1)[0]))
    isobar_rho_vap.append(vg)
    isobar_rho_liq.append(vl)
    isobar_T.append(T_list)

isentrop_T = []
isentrop_rho = []
sc = eos.entropy_tv(Tc, vc, z)[0]
isentropes = [200, 220, 240, 260, 280, 300]
T_list = np.linspace(min(Teq), 1.5 * Tc)
for i, s in enumerate(isentropes):
    T, _, v, _ = eos.get_isentrope(s, z, minimum_temperature=min(Teq), maximum_temperature=1.5 * Tc,
                                   maximum_pressure=5 * pc)
    isentrop_rho.append(to_kg(v))
    isentrop_T.append(T)

isenthalp_T = []
isenthalp_rho = []
hc = eos.enthalpy_tv(Tc, vc, z)[0]
isenthalpes = np.linspace(0.5 * hc, 2 * hc, 10)
T_list = np.linspace(min(Teq), 1.5 * Tc)
for i, h in enumerate(isenthalpes):
    T, _, v, _ = eos.get_isenthalp(h, z, minimum_temperature=min(Teq), maximum_temperature=1.5 * Tc,
                                   maximum_pressure=5 * pc)
    isenthalp_rho.append(to_kg(v))
    isenthalp_T.append(T)

fig = plt.figure(figsize=(10, 5))
ax = fig.add_subplot(autoscale_on=False)
ln = ax.plot([], [])
legend = plt.legend()
lines = []
Nlines = 0

def init(proj='Trho'):
    global fig, ax
    if proj == 'Trho':
        ax.set_ylabel(r'$T$ [K]')
        ax.set_xlabel(r'$\rho$ [kg mol$^{-1}$]')
        ax.set_xlim(1, 1.25 * max(to_kg(veq)))
        ax.set_ylim(300, 1.5 * max(Teq))
        plt.xscale('log')
    elif proj == 'Tp':
        ax.set_ylabel(r'$T$ [K]')
        ax.set_xlabel(r'$p$ [bar]')
        ax.set_xlim(0.055, 1.25 * max(peq / 1e5))
        ax.set_ylim(225, 1.1 * max(Teq))
        plt.xscale('log')

def draw_isobars(idx, alpha, legend=False):
    if idx=='all':
        for i in range(len(isobars)):
            draw_isobars(i, alpha, legend)

    elif idx == 0 and legend is True:
        plt.plot(isobar_rho_vap[idx], isobar_T[idx], color='b', label='Isobar', alpha=alpha)
        plt.plot(isobar_rho_liq[idx], isobar_T[idx], color='b', alpha=alpha)
    else:
        plt.plot(isobar_rho_vap[idx], isobar_T[idx], color='b', alpha=alpha)
        plt.plot(isobar_rho_liq[idx], isobar_T[idx], color='b', alpha=alpha)

def draw_isentropes(idx, alpha, legend):
    if idx == 'all':
        for i in range(len(isentropes)):
            draw_isentropes(i, alpha, legend)
    elif idx == 0 and legend is True:
        plt.plot(isentrop_rho[idx], isentrop_T[idx], color='r', alpha=alpha, label='Isentrope')
    else:
        plt.plot(isentrop_rho[idx], isentrop_T[idx], color='r', alpha=alpha)

def draw_isenthalpes(idx, alpha, legend):
    if idx == 'all':
        for i in range(len(isenthalpes)):
            draw_isenthalpes(i, alpha, legend)

    elif idx == 0 and legend is True:
        plt.plot(isenthalp_rho[idx], isenthalp_T[idx], color='c', label='Isenthalp')
    else:
        plt.plot(isenthalp_rho[idx], isenthalp_T[idx], color='c')

def generate_isolines(frame):
    init('Trho')

    if frame >= 1:
        plt.plot(to_kg(veq), Teq, color='black', label='Envelope')

    if frame >= 2:
        plt.plot(to_kg(vc), Tc, color='r', marker='*', label='Critical Point', linestyle='', markersize=15)

    if 2 < frame < len(isobars) + 3:
        isobar_idx = frame - 3
        for i in range(isobar_idx):
            draw_isobars(i, alpha=1, legend=True)
        fig.suptitle(rf'Isobars : $p = {isobars[isobar_idx] / 1e5:.2f}$ bar')

    elif frame >= len(isobars) + 3:
        draw_isobars('all', alpha=0.2, legend=True)

    if len(isobars) + 3 <= frame < len(isentropes) + len(isobars) + 3:
        isentrop_idx = frame - 3 - len(isobars)
        for i in range(isentrop_idx):
            draw_isentropes(i, alpha=1, legend=True)
        fig.suptitle(rf'Isentropes : $s = {isentropes[isentrop_idx]:.2f}$ J / mol K')
    elif frame >= len(isentropes) + len(isobars) + 3:
        draw_isentropes('all', alpha=0.2, legend=True)


    if len(isentropes) + len(isobars) + 3 < frame < len(isobars) + len(isentropes) + len(isenthalpes) + 3:
        isenthalp_idx = frame - 3 - len(isobars) - len(isentropes)
        for i in range(isenthalp_idx):
            draw_isenthalpes(i, alpha=1, legend=True)
        fig.suptitle(rf'Isenthalpes : $h = {isenthalpes[isenthalp_idx]:.2f}$ J / mol')

def generate_fig(frame):
    global fig, ax, ln, legend

    ax.cla()

    if frame < len(isobars) + len(isentropes) + len(isenthalpes) + 3:
        generate_isolines(frame)
        plt.legend(loc='upper left')

    elif frame < len(isobars) + len(isentropes) + len(isenthalpes) + 3 + 13:
        frame -= len(isobars) + len(isentropes) + len(isenthalpes) + 3

        init('Tp')
        x_co2_list = [0.05, 0.05, 0.05, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5]
        x_co2 = x_co2_list[frame]
        z = [0.3 * (1 - x_co2), 0.7 * (1 - x_co2), x_co2]
        Teq, peq = eos.get_envelope_twophase(5.5e3, z, maximum_pressure=1.25e7)
        Tc, _, pc = eos.critical(z)
        Tlist = [260, 300, 350]
        plist = [2e5, 10e5, 30e5]
        for T, p in zip(Tlist[:min(frame, 3)], plist[:min(frame, 3)]):
            x, y, bv, bl, _ = eos.two_phase_tpflash(T, p, z)
            pb, _ = eos.bubble_pressure(T, z)
            pd, _ = eos.dew_pressure(T, z)
            plt.plot([pb / 1e5, p / 1e5, pd / 1e5], [T, T, T], linestyle='--', marker='o')
            plt.text(np.sqrt(p * pb) / 1.5e5 , T - 10, r'$\beta_{v}$ = ' + str(round(bv, 2)))
            plt.text(np.sqrt(p * pd) / 1.5e5, T - 10, r'$\beta_{l}$ = ' + str(round(bl, 2)))
        plt.plot(pc / 1e5, Tc, color='r', marker='*', label='Critical Point', linestyle='', markersize=10)
        plt.plot(peq / 1e5, Teq, color='black', label=r'$x_{CO_2}$ = ' + str(x_co2))
        plt.suptitle('Tp-Flash calculations')

        plt.legend(loc='upper left')

    else:
        pass

init()
nlines = 3 + len(isenthalpes) + len(isentropes) + len(isobars)

# for i in range(1, nlines):
    # generate_fig(i)
    # fig.legend()
    # plt.show()
ani = FuncAnimation(fig, generate_fig, frames=[i for i in range(nlines + 16)], blit=False, interval=500)
ani.save('animation.gif', writer='imagemagick', fps=2)
plt.show()
