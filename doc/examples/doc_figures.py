import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import numpy as np
from thermopack.cubic import cubic
from thermopack.cpa import cpa
from thermopack.saftvrmie import saftvrmie
from thermopack.pcsaft import pcsaft
from thermopack.saftvrqmie import saftvrqmie
from thermopack.quantum_cubic import qcubic
from matplotlib.cm import get_cmap, ScalarMappable
from matplotlib.colors import Normalize, LogNorm, SymLogNorm
import os

class NormedCmap:
    """
    Convenience class that holds both a normalizer, and a colormap, such that you don't have to call

    cmap = get_cmap('<name>')
    norm = Normalize(min(<my_list>), max(<my_list>))
    color = cmap(norm(<value>))

    but rather can use

    cmap = NormedCmap('<name>', <my_list>)
    color=cmap(value)

    for the same result.
    """
    def __init__(self, name, lst, norm=Normalize):
        """
        Args:
            name (str) : A matplotlib colormap name
            lst (iterable) : The Normalizer will take min and max values from this iterable
            norm (Normalizer, optional) : A callable class that behaves like a matplotlib Normalize class
                 (str) : 'log' => use matplotlib.colors.LogNorm, 'symlog' => use matplotlib.colors.SymLogNorm
        """
        if norm == 'log':
            norm = LogNorm
        elif norm == 'symlog':
            norm = SymLogNorm
        self.norm = norm(min(lst), max(lst))
        self.cmap = get_cmap(name)

    def get_ScalarMappable(self):
        return ScalarMappable(norm=self.norm, cmap=self.cmap)

    def colorbar(self, ax=None, cax=None, **kwargs):
        """
        Plot a colorbar using this instances colormap and normalizer. Pass kwargs to plt.colorbar.
        """
        plt.colorbar(self.get_ScalarMappable(), ax=ax, cax=cax, **kwargs)

    def __call__(self, val):
        """
        Get the color corresponding to the (non-normalized) value 'val'. 'val' will be normalized assuming it is from the
        list used to initialize this instance.
        """
        return self.cmap(self.norm(val))


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
T_list = np.linspace(min(Teq), 1.5 * Tc, 60)
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

eos2 = cpa('ACETONE,ETOH', 'SRK')

pxy_temps = [300, 325, 350, 375, 400, 425, 450]
LLE_p, L1VE_p, L2VE_p = {}, {}, {}

for T in pxy_temps:
    lle, l1ve, l2ve = eos2.get_binary_pxy(T, minimum_pressure=1e3)
    LLE_p[T] = lle
    L1VE_p[T] = l1ve
    L2VE_p[T] = l2ve


eos3 = cubic('C3,NC6', 'SRK')
Txy_pres = np.linspace(1, 4, 10) * 1e5
LLE_T, L1VE_T, L2VE_T = {}, {}, {}

for p in Txy_pres:
    lle, l1ve, l2ve = eos3.get_binary_txy(p, minimum_temperature=200)
    LLE_T[p] = lle
    L1VE_T[p] = l1ve
    L2VE_T[p] = l2ve

fig = plt.figure(figsize=(10, 5))
ax = fig.add_subplot(autoscale_on=False)
ln = ax.plot([], [])
legend = plt.legend()

def init(proj='Trho'):
    global fig, ax
    if proj == 'Trho':
        ax.set_ylabel(r'$T$ [K]')
        ax.set_xlabel(r'$\rho$ [kg m$^{-3}$]')
        ax.set_xlim(1, 1.25 * max(to_kg(veq)))
        ax.set_ylim(300, 1.5 * max(Teq))
        plt.xscale('log')

    elif proj == 'Tp':
        ax.set_ylabel(r'$T$ [K]')
        ax.set_xlabel(r'$p$ [bar]')
        ax.set_xlim(0.055, 1.25 * max(peq / 1e5))
        ax.set_ylim(225, 1.1 * max(Teq))
        plt.xscale('log')

    elif proj == 'pxy':
        ax.set_xlabel(r'$x, y$')
        ax.set_ylabel(r'$p$ [bar]')
        ax.set_xlim(0, 1)
        ax.set_ylim(8e-2, 20)
        ax.set_yscale('log')

    elif proj == 'pxy_flash':
        ax.set_xlabel(r'$x, y$')
        ax.set_ylabel(r'$p$ [bar]')
        ax.set_xlim(0, 1)
        ax.set_ylim(0.8, 2.3)

    elif proj == 'Txy':
        ax.set_xlabel(r'$x, y$')
        ax.set_ylabel(r'$T$ [K]')
        ax.set_xlim(0, 1)
        ax.set_ylim(225, 400)

    elif proj == 'eos_h2':
        ax.set_xlabel(r'$T$ [K]')
        ax.set_ylabel(r'$C_p$ [J mol$^{-1}$ K$^{-1}$]')
        ax.set_xlim(30, 36)
        ax.set_ylim(35, 250)

    elif proj == 'eos_nh3':
        ax.set_xlabel(r'$T$ [K]')
        ax.set_ylabel(r'Compressibility')
        ax.set_xlim(350, 500)
        ax.set_ylim(0.15, 0.85)

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

def flashes(frame):
    if frame < len(pxy_temps):
        init('pxy')
        cmap = NormedCmap('cool', pxy_temps)
        for i in range(frame):
            T = pxy_temps[i]
            plt.plot(L1VE_p[T][0], L1VE_p[T][2] / 1e5, label=T, color=cmap(T))
            plt.plot(L1VE_p[T][1], L1VE_p[T][2] / 1e5, color=cmap(T))
        plt.legend(title=r'$T$ [K]', loc='upper left')
        plt.suptitle('pxy-diagrams')

    elif frame < len(pxy_temps) + 20:
        init('pxy_flash')
        cmap = NormedCmap('cool', pxy_temps)
        T = pxy_temps[2]
        plt.plot(L1VE_p[T][0], L1VE_p[T][2] / 1e5, label=T, color=cmap(T))
        plt.plot(L1VE_p[T][1], L1VE_p[T][2] / 1e5, color=cmap(T))
        pxy_flash_p = np.linspace(1.3, 2.1, 20) * 1e5
        pxy_flash_z = np.ones(20) * 0.4
        flash_idx = frame - (len(pxy_temps))

        p, z = pxy_flash_p[flash_idx], pxy_flash_z[flash_idx]
        flsh = eos2.two_phase_tpflash(T, p, [z, 1 - z])
        if flsh.phase != eos.TWOPH:
            x, y = z, z
        else:
            x, y = flsh.x[0], flsh.y[0]
        plt.plot([x, z, y], [p / 1e5, p / 1e5, p / 1e5], color='black', marker='o', linestyle='--')
        plt.plot(pxy_flash_z, pxy_flash_p / 1e5, color='r', linestyle=':', alpha=0.3)

        if flsh.phase == eos.TWOPH:
            plt.text(0.5 * (flsh.x[0] + z) - 0.05, (p / 1e5) - 0.1, r'$\beta_v$ = ' + str(round(flsh.betaV, 2)))
            plt.text(0.5 * (flsh.y[0] + z) - 0.05, (p / 1e5) - 0.1, r'$\beta_l$ = ' + str(round(flsh.betaL, 2)))

            plt.text(flsh.x[0] - 0.1, (p / 1e5) + 0.05, r'$x_{Ac}$ = ' + str(round(flsh.x[0], 2)))
            plt.text(flsh.y[0] + 0.01, (p / 1e5) - 0.05, r'$y_{Ac}$ = ' + str(round(flsh.y[0], 2)))

        plt.suptitle('Flash calculations\n' + r'$T$ = ' + str(round(T, 2)) + ' K')

    elif frame < len(pxy_temps) + 20 + len(Txy_pres) + 1:
        init('Txy')
        cmap = NormedCmap('cividis', Txy_pres)
        for i in range(frame - (len(pxy_temps) + 21)):
            p = Txy_pres[i]
            plt.plot(L1VE_T[p][0], L1VE_T[p][2], label=round(p / 1e5, 2), color=cmap(p))
            plt.plot(L1VE_T[p][1], L1VE_T[p][2], color=cmap(p))
        plt.legend(title=r'$p$ [bar]', loc='lower left', ncol=2)
        plt.suptitle('Txy-diagrams')

def manyEoS(frame):

    if frame < 7:
        init('eos_h2')
        eos_list = [cubic, cubic, qcubic, saftvrmie, saftvrqmie]
        extra_init = ['SRK', 'PR', None, None, None]
        name = ['SRK', 'PR', 'Quantum Cubic',
                'SAFT-VR Mie', 'SAFT-VRQ Mie']

        for EoS, extra, name in zip(eos_list[:frame], extra_init[:frame], name[:frame]):
            if extra is None:
                e = EoS('H2')
            else:
                e = EoS('H2', extra)

            e.set_tmin(20)
            T_list = np.linspace(30, 36, 100)
            p = 16.5e5  # 1296400.0
            z_list = np.empty_like(T_list)
            for i, T in enumerate(T_list):
                _, z_list[i] = e.enthalpy(T, p, [1], 2, dhdt=True)

            plt.plot(T_list, z_list, label=name)
        plt.legend(title='EoS', ncol=2, loc='upper left')
        plt.suptitle('Many classic and modern Equations of State')

    elif frame < 7 + 9:
        init('eos_nh3')
        eos_list = [cubic, cubic, cubic, cubic, cpa, saftvrmie, saftvrqmie, pcsaft]
        extra_init = ['SRK', 'PR', 'PT', 'SW', 'SRK', None, None, None]
        name = ['SRK', 'PR', 'PT', 'SW', 'SRK-CPA',
                'SAFT-VR Mie', 'SAFT-VRQ Mie', 'PC-SAFT']

        eos_idx = frame - 7
        for EoS, extra, name in zip(eos_list[:eos_idx], extra_init[:eos_idx], name[:eos_idx]):
            if extra is None:
                e = EoS('NH3')
            else:
                e = EoS('NH3', extra)


            T_list = np.linspace(350, 500) # np.linspace(650, 750, 100)
            p = 170e5 # 350e5  # 22048300.0 # 11470000.0
            z_list = np.empty_like(T_list)
            for i, T in enumerate(T_list):
                z_list[i], = e.zfac(T, p, [1], 1)

            plt.plot(T_list, z_list, label=name)
        plt.legend(title='EoS', ncol=2, loc='upper left')
        plt.suptitle('Many classic and modern Equations of State')

def generate_fig(frame):
    global fig, ax, ln, legend
    ax.cla()

    if frame < nisolines:
        generate_isolines(frame)
        plt.legend(loc='upper left')

    elif frame < nTxy:
        frame -= nisolines
        flashes(frame)

    else:
        frame -= nTxy
        manyEoS(frame)


init()
nisolines = 3 + len(isenthalpes) + len(isentropes) + len(isobars)
nflashes = nisolines
npxy = nflashes + len(pxy_temps)
npxy_flash = npxy + 20
nTxy = npxy_flash + len(Txy_pres) + 1
nEoS = nTxy + 16
nframes = nEoS

# generate_fig(nframes - 1)
# plt.show()
# exit(0)
ani = FuncAnimation(fig, generate_fig, frames=[i for i in range(nframes)], blit=False, interval=750)
ani.save(os.path.dirname(__file__) + '/../markdown/figures/readme_intro.gif', writer='imagemagick')
plt.show()
