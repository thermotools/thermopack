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
        self.hE = None
        self.s = None
        self.sE = None
        self.a = None
        self.aE = None
        self.u = None
        self.uE = None
        self.mu = None
        self.muE = None
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
        _ = self.specific_enthalpy()
        return self.h*np.sum(self.n)

    def enthalpy_density(self):
        return self.specific_enthalpy()/self.v

    def specific_excess_enthalpy(self):
        if self.hE is not None:
            hE = self.hE
        elif self.v is not None:
            _ = self.volume()
            hE, = self.eos.enthalpy_tv(self.T, self.v, self.x, property_flag="R")
            self.hE = hE
        return hE

    def excess_enthalpy_density(self):
        return self.specific_excess_enthalpy()/self.v

    def excess_enthalpy(self):
        _ = self.specific_excess_enthalpy()
        return self.hE*np.sum(self.n)

    def specific_entropy(self):
        if self.s is not None:
            s = self.s
        elif self.v is not None:
            _ = self.volume()
            s, = self.eos.entropy_tv(self.T, self.v, self.x)
            self.s = s
        return s

    def entropy(self):
        _ = self.specific_entropy()
        return self.s*np.sum(self.n)

    def entropy_density(self):
        return self.specific_entropy()/self.v

    def specific_excess_entropy(self):
        if self.sE is not None:
            sE = self.sE
        elif self.v is not None:
            _ = self.volume()
            sE, = self.eos.entropy_tv(self.T, self.v, self.x, property_flag="R")
            self.sE = sE
        return sE

    def excess_entropy_density(self):
        if self.sE is not None:
            sE = self.sE
        elif self.v is not None:
            _ = self.volume()
            sE, = self.eos.entropy_tv(self.T, self.v, self.x, property_flag="R")
            self.sE = sE
        return sE/self.v

    def excess_entropy(self):
        _ = self.specific_excess_entropy()
        return self.sE*np.sum(self.n)

    def specific_free_energy(self):
        if self.a is not None:
            a = self.a
        elif self.v is not None:
            _ = self.volume()
            a, = self.eos.helmholtz_tv(self.T, self.v, self.x)
            self.a = a
        return a

    def free_energy(self):
        _ = self.specific_free_energy()
        return self.a*np.sum(self.n)

    def free_energy_density(self):
        return self.specific_free_energy()/self.v

    def specific_excess_free_energy(self):
        if self.aE is not None:
            aE = self.aE
        elif self.v is not None:
            _ = self.volume()
            aE, = self.eos.helmholtz_tv(self.T, self.v, self.x, property_flag="R")
            self.aE = aE
        return aE

    def excess_free_energy_density(self):
        return self.specific_excess_free_energy()/self.v

    def excess_free_energy(self):
        _ = self.specific_excess_free_energy()
        return self.aE*np.sum(self.n)

    def specific_energy(self):
        if self.u is not None:
            u = self.u
        elif self.v is not None:
            _ = self.volume()
            u, = self.eos.internal_energy_tv(self.T, self.v, self.x)
            self.u = u
        return u

    def energy(self):
        _ = self.specific_energy()
        return self.u*np.sum(self.n)

    def energy_density(self):
        return self.specific_energy()/self.v

    def specific_excess_energy(self):
        if self.uE is not None:
            uE = self.uE
        elif self.v is not None:
            _ = self.volume()
            uE, = self.eos.internal_energy_tv(self.T, self.v, self.x, property_flag="R")
            self.uE = uE
        return uE

    def excess_energy(self):
        _ = self.specific_excess_energy()
        return self.uE*np.sum(self.n)

    def excess_energy_density(self):
        return self.specific_excess_energy()/self.v

    def chemical_potential(self):
        if self.mu is not None:
            mu = self.mu
        elif self.v is not None:
            _ = self.volume()
            mu, = self.eos.chemical_potential_tv(self.T, self.v, self.x)
            self.mu = mu
        return mu

    def excess_chemical_potential(self):
        if self.muE is not None:
            muE = self.muE
        elif self.v is not None:
            _ = self.volume()
            muE, = self.eos.chemical_potential_tv(self.T, self.v, self.x, property_flag="R")
            self.muE = muE
        return muE

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
        self.present_phase_list = [ {"state": vapor, "phase": "V"}, {"state": liquid, "phase": "L"}]
        if liquid2 is not None:
            self.present_phase_list.append({"state": liquid2, "phase": "L"})
        if solid is not None:
            self.present_phase_list.append({"state": solid, "phase": "S"})

    def __repr__(self):
        output = "-----------------------------------------------------"
        for p in self.present_phase_list:
            output += "\n"
            state = p["state"]
            phase = p["phase"]
            output += f"{phase}: {state.T:.5f} K   {np.sum(state.partial_density())*1e-3:.5f} kmol/m3    {state.x}"
        return output

    def __str__(self):
        output = "-----------------------------------------------------"
        for p in self.present_phase_list:
            output += "\n"
            state = p["state"]
            phase = p["phase"]
            output += f"{phase}: {state.T:.5f} K   {np.sum(state.partial_density())*1e-3:.5f} kmol/m3    {state.x}"
        return output

    @staticmethod
    def tp_flash(eos, T, p, z):
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
    def bubble_pressure(eos, T, z):
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
    def bubble_temperature(eos, p, z):
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

    @property
    def temperature(self):
        return self.vapor.T if self.vapor else self.liquid.T

    @property
    def pressure(self):
        return self.vapor.pressure if self.vapor else self.liquid.pressure

    @property
    def eos(self):
        return self.vapor.eos if self.vapor else self.liquid.eos

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
    def pure_saturation_curve(eos, T, n):
        """Map pure fluid saturation curve

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature to start mapping curve (K)
            n (int): Number of equidistant temperature points
        Returns:
            phase_diagram: Phase diagram
        """
        assert eos.nc == 1
        z = np.ones(1)
        t_vals, p_vals, vl_vals, vg_vals = eos.get_pure_fluid_saturation_curve(initial_pressure=None,
                                                                               initial_temperature=T,
                                                                               z=None,
                                                                               max_delta_press=0.2e5,
                                                                               nmax=n,
                                                                               log_linear_grid=False)
        vle_states = []
        for i in range(len(t_vals)):
            vapor = state(eos=eos, T=t_vals[i], V=vg_vals[i], n=z, p=p_vals[i], n_tot=1.0,
                          init_specific=True)
            liquid = state(eos=eos, T=t_vals[i], V=vl_vals[i], n=z, p=p_vals[i], n_tot=1.0,
                           init_specific=True)
            vle_states.append(equilibrium(vapor, liquid))
        return phase_diagram(vle_states)

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

    @property
    def temperatures(self):
        return phase_state_list([vle.vapor for vle in self.vle_states]).temperatures

    @property
    def pressures(self):
        return phase_state_list([vle.vapor for vle in self.vle_states]).pressures

class meta_curve(object):
    """
    List of meta-stable states
    """

    def __init__(self, meta_states):
        """
        Assign list of states
        """
        self.meta_states = meta_states

    @staticmethod
    def isothermal(eos, T, z, n, phase):
        """Map meta-stable states from saturation curve to spinodal

        Args:
            eos (thermo): Equation of state object
            T (float): Temperature to start mapping curve (K)
            z (float): Composition (-)
            n (int): Number of points equidistant in specific volume
            phase (int): Phase indicator. thermo.LIQPH or thermo.VAPPH
        Returns:
            meta_curve: List of states from saturation curve to spinodal
        """
        vz, rho = eos.map_meta_isotherm(T=T,
                                        z=z,
                                        phase=phase,
                                        n=n)
        meta_states = []
        for i in range(len(t_vals)):
            if phase == eos.LIQPH:
                vl = vz
                vg = 1.0/np.sum(rho[i,:])
                x = z
                y = rho[i,:]*vg
            else:
                vg = vz
                vl = 1.0/np.sum(rho[i,:])
                y = z
                x = rho[i,:]*vl
            vapor = state(eos=eos, T=T, V=vg, n=y, n_tot=1.0,
                          init_specific=True)
            liquid = state(eos=eos, T=T, V=vl, n=x, n_tot=1.0,
                           init_specific=True)
            meta_states.append(equilibrium(vapor, liquid))
        return meta_curve(meta_states)

    @property
    def liquid(self):
        return phase_state_list([meta.liquid for meta in self.meta_states])

    @property
    def vapour(self):
        return phase_state_list([meta.vapor for meta in self.meta_states])

    @property
    def temperatures(self):
        return phase_state_list([meta.vapor for meta in self.meta_states]).temperatures

    @property
    def pressures(self):
        return phase_state_list([meta.vapor for meta in self.meta_states]).pressures
