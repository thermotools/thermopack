import sys
import numpy as np


class state(object):
    """
    Thermodynamic state point
    """

    def __init__(self, eos, T, V, n, p=None, h=None, ph=None, n_tot=None, init_specific=False):
        """
        Create state

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            V (float): Volume (m^3 or m^3/mol)
            n (np.ndarray): Mol numbers (mol or mol/mol)
            p (float, optional): Pressure (Pa). Defaults to None.
            h (float, optional): Enthalpy (J/mol). Defaults to None.
            ph (int, optional): Phase identifyer (eos.LIQPH or eos.VAPPH). Defaults to None.
            n_tot (float, optional): Overall mol numbers. Defaults to None.
            init_specific (bool, optional): Treat inpus as specific variables. Defaults to False.
        """
        self.reset(eos=eos, T=T, V=V, n=n, p=p, h=h, ph=ph,
                   n_tot=n_tot, init_specific=init_specific)

    def reset(self, eos, T, V, n, p=None, h=None, ph=None, n_tot=None, init_specific=False):
        """
        Reset state for new calculation

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            V (float): Volume (m^3 or m^3/mol)
            n (np.ndarray): Mol numbers (mol or mol/mol)
            p (float, optional): Pressure (Pa). Defaults to None.
            h (float, optional): Enthalpy (J/mol). Defaults to None.
            ph (int, optional): Phase identifyer (eos.LIQPH or eos.VAPPH). Defaults to None.
            n_tot (float, optional): Overall mol numbers. Defaults to None.
            init_specific (bool, optional): Treat inpus as specific variables. Defaults to False.
        """
        self.eos = eos
        self.T = T
        if init_specific:  # Treat V, n and h as specific properties
            assert n_tot is not None
            self.v = V
            self.x = n
            self.V = V*n_tot if V is not None else None
            self.n = n*n_tot
        else:
            self.V = V
            self.n = n
            self.v = V/np.sum(n) if V is not None else None
            self.x = n/np.sum(n)
        if self.v is not None:
            self.rho = np.zeros_like(self.n)
            self.rho[:] = self.x[:] / self.v
        else:
            self.rho = None
        self.p = p
        self.h = h
        self.ph = ph

    def __repr__(self):
        return "temperature 	density 	molefracs\n" + \
            "------------------------------------------\n" + \
            f"{self.T:.5f} K   {np.sum(self.rho)*1e-3:.5f} kmol/m3    {self.x}"

    def __str__(self):
        return "temperature 	density 	molefracs\n" + \
            "------------------------------------------\n" + \
            f"{self.T:.5f} K   {np.sum(self.rho)*1e-3:.5f} kmol/m3    {self.x}"

    @staticmethod
    def new_nvt(eos, T, V, n):
        """ Tvn state

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            V (float): Volume (m^3)
            n (np.ndarray): Mol numbers (mol)

        Returns:
            state: State constructed form TVn
        """
        return state(eos=eos, T=T, V=V, n=n)

    @staticmethod
    def new_tpx(eos, T, p, x):
        """Create Tpx state

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            p (float): Pressure (Pa).
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            state: State constructed form Tpx
        """
        return state(eos=eos, T=T, V=None, n=x, p=p, n_tot=1.0, init_specific=True)

    @staticmethod
    def new_phx(eos, h, p, x):
        """Create hpx state

        Args:
            eos (thermo.thermo): Equation of state object
            h (float): Enthalpy (J/mol)
            p (float): Pressure (Pa).
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            state: State constructed form hpx
        """
        return state(eos=eos, T=None, V=None, n=x, p=p, h=h, n_tot=1.0, init_specific=True)

    @staticmethod
    def critical(eos, x):
        """Create critical state

        Args:
            eos (thermo.thermo): Equation of state object
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            state: State constructed form critical point
        """
        T, v, p = eos.critical(n=x)
        return state(eos=eos, T=T, V=v, n=x, p=p, n_tot=1.0, init_specific=True)

    def pressure(self):
        if self.p is not None:
            p = self.p
        elif self.T is not None and \
                self.v is not None and self.x is not None:
            p, = self.eos.pressure_tv(self.T, self.v, self.x)
            self.p = p
        return p

    def specific_volume(self):
        if self.v is not None:
            v = self.v
        elif self.T is not None and \
                self.P is not None and \
                self.x is not None and \
                self.ph is not None:
            v, = self.eos.specific_volume(self.T, self.p, self.x, self.ph)
            self.v = v
            self.V = v*np.sum(self.n)
        return v

    def volume(self):
        _ = self.specific_volume()
        return self.V

    def partial_density(self):
        rho = np.zeros_like(self.n)
        if self.rho is None:
            self.rho = np.zeros_like(self.n)
            self.rho[:] = self.x[:]/self.specific_volume()
        rho[:] = self.rho[:]
        return self.rho

    def specific_enthalpy(self):
        if self.h is not None:
            h = self.h
        elif self.v is not None:
            _ = self.volume()
            h, = self.eos.enthalpy_tv(self.T, self.v, self.x)
            self.h = h
        return h

    def enthalpy(self):
        _ = self.specific_volume()
        return self.h*np.sum(self.n)


class equilibrium(object):
    """
    VLLSE phase equilibrium
    """

    def __init__(self, vapor, liquid, liquid2=None, solid=None):
        """
        Create VLLE state
        """
        self.vapor = vapor
        self.liquid = liquid
        self.liquid1 = self.liquid
        self.liquid2 = liquid2
        self.solid = solid

    @staticmethod
    def tp_flash(self, eos, T, p, z):
        """Construct class from tpz flash

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            P (float): Pressure (Pa)
            z (np.ndarray): Molar fractions (mol/mol)

        Returns:
            equilibrium: Equilibrium state
        """
        x, y, betaV, betaL, phase = eos.two_phase_tpflash(temp=T, press=p, z=z)
        vg, = eos.specific_volume(T, p, y, eos.VAPPH) \
            if phase in [eos.VAPPH, eos.TWOPH] else (None,)
        vl, = eos.specific_volume(T, p, x, eos.LIQPH) \
            if phase in [eos.LIQPH, eos.TWOPH] else (None,)
        vapor = state(eos=eos, T=T, v=vg, n=y, p=p,
                      ph=eos.VAPPH, n_tot=betaV,
                      init_specific=True) \
            if phase in [eos.VAPPH, eos.TWOPH] else None
        liquid = state(eos=eos, T=T, V=vl, n=x, p=p,
                       ph=eos.LIQPH, n_tot=betaL,
                       init_specific=True) \
            if phase in [eos.LIQPH, eos.TWOPH] else None
        return equilibrium(vapor, liquid)

    @staticmethod
    def bubble_pressure(self, eos, T, z):
        """Construct class from bubble pressure calculation

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            z (np.ndarray): Molar fractions (mol/mol)

        Returns:
            equilibrium: Equilibrium state
        """
        p, y = eos.bubble_pressure(temp=T, z=z)
        vg, = eos.specific_volume(T, p, y, eos.VAPPH)
        vl, = eos.specific_volume(T, p, z, eos.LIQPH)
        vapor = state(eos=eos, T=T, V=vg, n=y, p=p, ph=eos.VAPPH, n_tot=0.0,
                      init_specific=True)
        liquid = state(eos=eos, T=T, V=vl, n=z, p=p, ph=eos.LIQPH, n_tot=1.0,
                       init_specific=True)
        return equilibrium(vapor, liquid)

    @staticmethod
    def bubble_temperature(self, eos, p, z):
        """Construct class from bubble temperature calculation

        Args:
            eos (thermo.thermo): Equation of state object
            p (float): Pressure (Pa)
            z (np.ndarray): Molar fractions (mol/mol)

        Returns:
            equilibrium: Equilibrium state
        """
        T, y = eos.bubble_temperature(press=p, z=z)
        vg, = eos.specific_volume(T, p, y, eos.VAPPH)
        vl, = eos.specific_volume(T, p, z, eos.LIQPH)
        vapor = state(eos=eos, T=T, V=vg, n=y, p=p, ph=eos.VAPPH, n_tot=0.0,
                      init_specific=True)
        liquid = state(eos=eos, T=T, V=vl, n=z, p=p, ph=eos.LIQPH, n_tot=1.0,
                       init_specific=True)
        return equilibrium(vapor, liquid)


class phase_state_list(object):
    """
    List of phase states. Utility class.
    """

    def __init__(self, states):
        """
        Create list
        """
        self.states = states

    def __getitem__(self, i):
        return self.states[i]

    @property
    def temperatures(self):
        return np.array([state.T for state in self.states])

    @property
    def pressures(self):
        return np.array([state.p for state in self.states])

    @property
    def molefracs(self):
        return np.array([state.x for state in self.states])


class phase_diagram(object):
    """
    List of states
    """

    def __init__(self, vle_states):
        """
        Create empty list
        """
        self.vle_states = vle_states

    @staticmethod
    def binary_isotherm_vle(eos, T, maximum_pressure=1.5e7):
        """Construct phase_diagram from binary vle isotherm

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            maximum_pressure (float, optional): Pressure (Pa). Defaults to 1.5e7.

        Returns:
            phase_diagram: Phase diagram
        """
        _, L1VE, _ = eos.get_binary_pxy(temp=T,
                                        maximum_pressure=maximum_pressure,
                                        minimum_pressure=1.0e5,
                                        maximum_dz=0.003,
                                        maximum_dlns=0.01)
        vle_states = []
        x, y, p = L1VE
        for i in range(len(x)):
            yy = np.array([y[i], 1-y[i]])
            vg, = eos.specific_volume(T, p[i], yy, eos.VAPPH)
            xx = np.array([x[i], 1-x[i]])
            vl, = eos.specific_volume(T, p[i], xx, eos.LIQPH)
            vapor = state(eos=eos, T=T, V=vg, n=yy, p=p[i], n_tot=1.0,
                          init_specific=True)
            liquid = state(eos=eos, T=T, V=vl, n=xx, p=p[i], n_tot=1.0,
                           init_specific=True)
            vle_states.append(equilibrium(vapor, liquid))
        return phase_diagram(vle_states)

    @property
    def liquid(self):
        return phase_state_list([vle.liquid for vle in self.vle_states])

    @property
    def vapour(self):
        return phase_state_list([vle.vapor for vle in self.vle_states])
