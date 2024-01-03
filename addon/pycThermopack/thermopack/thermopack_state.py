import sys
import numpy as np
import inspect
from types import MethodType
from . import utils

class State(object):
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
        self.eos = eos
        self._T = T
        if init_specific:  # Treat V, n and h as specific properties
            assert n_tot is not None
            self._v = V
            self._x = n
            self._V = V*n_tot if V is not None else None
            self._n = n*n_tot
            self._n_tot = n_tot
        else:
            self._V = V
            self._n = n
            self._n_tot = np.sum(n)
            self._v = V/self._n_tot if V is not None else None
            self._x = n/self._n_tot
        if self._v is not None:
            self._rho = np.zeros_like(self._n)
            self._rho[:] = self._x[:] / self._v
        else:
            self._rho = None
        self._p = p
        self._h = h
        self._h_res = None
        self._s = None
        self._s_res = None
        self._a = None
        self._a_res = None
        self._u = None
        self._u_res = None
        self._mu = None
        self._mu_res = None
        self._ph = ph

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
        self.__init__(eos=eos, T=T, V=V, n=n, p=p, h=h, ph=ph,
                        n_tot=n_tot, init_specific=init_specific)


    def __repr__(self):
        return "temperature 	density 	molefracs\n" + \
            "------------------------------------------\n" + \
            f"{self._T:.5f} K   {np.sum(self._rho)*1e-3:.5f} kmol/m3    {self._x}"

    def __str__(self):
        return "temperature 	density 	molefracs\n" + \
            "------------------------------------------\n" + \
            f"{self._T:.5f} K   {np.sum(self._rho)*1e-3:.5f} kmol/m3    {self._x}"

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
        return State(eos=eos, T=T, V=V, n=n)

    @staticmethod
    def new_tpx(eos, T, p, x):
        """Create Tpx state

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            p (float): Pressure (Pa).
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            State: State constructed form Tpx
        """
        return State(eos=eos, T=T, V=None, n=x, p=p, n_tot=1.0, init_specific=True)

    @staticmethod
    def new_phx(eos, h, p, x):
        """Create hpx state

        Args:
            eos (thermo.thermo): Equation of state object
            h (float): Enthalpy (J/mol)
            p (float): Pressure (Pa).
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            State: State constructed form hpx
        """
        return State(eos=eos, T=None, V=None, n=x, p=p, h=h, n_tot=1.0, init_specific=True)

    @staticmethod
    def new_mut(eos, mu, T, rho0):
        """Create mu-T state

        Args:
            eos (thermo.thermo): Equation of state object
            mu (float): Chemical potential (J/mol)
            T (float): Temperature (K).
            rho0 (np.ndarray): Initial densities (mol/m3)

        Returns:
            State: State constructed form mu-T
        """
        rho = eos.density_mu_t(T, mu, rho_initial=rho0)
        v = 1.0/np.sum(rho)
        x = rho*v
        return State(eos=eos, T=T, V=v, n=x, p=None, h=None, n_tot=1.0, init_specific=True)

    @staticmethod
    def critical(eos, x):
        """Create critical state

        Args:
            eos (thermo.thermo): Equation of state object
            x (np.ndarray): Mol numbers (mol/mol)

        Returns:
            State: State constructed form critical point
        """
        T, v, p = eos.critical(n=x)
        return State(eos=eos, T=T, V=v, n=x, p=p, n_tot=1.0, init_specific=True)

    @property
    def temperature(self):
        """ Temperature (K)
        """
        return self._T

    @property
    def T(self):
        """ Temperature (K)
        """
        return self._T

    @property
    def molefrac(self):
        """ Mole fraction (mol/mol)
        """
        return self._x

    @property
    def pressure(self):
        """ Pressure (Pa)
        """
        if self._p is None and (self._T is not None and
                self._v is not None and self._x is not None):
            self._p, = self.eos.pressure_tv(self._T, self._v, self._x)
        return self._p

    @property
    def p(self):
        """ Pressure (Pa)
        """
        return self.pressure

    @property
    def specific_volume(self):
        """ Specific volume (m3/mol)
        """
        if self._v is None and (self._T is not None and
                self._p is not None and
                self._x is not None and
                self._ph is not None):
            self._v, = self.eos.specific_volume(self._T, self._p, self._x, self._ph)
        return self._v

    @property
    def v(self):
        """ Specific volume (m3/mol)
        """
        return self.specific_volume

    @property
    def volume(self):
        """ Volume (m3)
        """
        if self._V is None:
            self._V = self.specific_volume * self._n_tot
        return self._V

    @property
    def V(self):
        """ Volume (m3)
        """
        return self.volume

    @property
    def partial_density(self):
        """ Partial densities (mol/m3)
        """
        if self._rho is None:
            self._rho = np.zeros_like(self._n)
            self._rho[:] = self._x[:]/self.specific_volume
        return self._rho

    @property
    def rho(self):
        """ Partial densities (mol/m3)
        """
        return self.partial_density

    @property
    def specific_enthalpy(self):
        """ Specific entalpy (J/mol)
        """
        if self._h is None:
            self._h = utils.back_compatible_unpack(self.eos.enthalpy_tv(self._T, self.specific_volume, self._x))
        return self._h

    @property
    def h(self):
        """ Specific entalpy (J/mol)
        """
        return self.specific_enthalpy

    @property
    def enthalpy(self):
        """ Entalpy (J)
        """
        return self.specific_enthalpy * self._n_tot

    @property
    def H(self):
        """ Entalpy (J)
        """
        return self.enthalpy

    @property
    def enthalpy_density(self):
        """ Entalpy density (J/m3)
        """
        return self.specific_enthalpy / self._v

    @property
    def h_dens(self):
        """ Entalpy density (J/m3)
        """
        return self.enthalpy_density

    @property
    def specific_residual_enthalpy(self):
        """ Specific residual entalpy (J/mol)
        """
        if self._h_res is None:
            self._h_res = utils.back_compatible_unpack(self.eos.enthalpy_tv(self._T, self.specific_volume, self._x, property_flag="R"))
        return self._h_res

    @property
    def h_res(self):
        """ Specific residual entalpy (J/mol)
        """
        return self.specific_residual_enthalpy

    @property
    def residual_enthalpy_density(self):
        """ Residual entalpy (J/m3)
        """
        return self.specific_residual_enthalpy / self._v

    @property
    def h_res_dens(self):
        """ Residual entalpy (J/m3)
        """
        return self.residual_enthalpy_density

    @property
    def residual_enthalpy(self):
        """ Residual entalpy (J)
        """
        return self.specific_residual_enthalpy * self._n_tot

    @property
    def H_res(self):
        """ Residual entalpy (J)
        """
        return self.residual_enthalpy

    @property
    def specific_entropy(self):
        """ Specific entropy (J/mol/K)
        """
        if self._s is None:
            self._s = utils.back_compatible_unpack(self.eos.entropy_tv(self._T, self.specific_volume, self._x))
        return self._s

    @property
    def s(self):
        """ Specific entropy (J/mol/K)
        """
        return self.specific_entropy

    @property
    def entropy(self):
        """ Entropy (J/K)
        """
        return self.specific_entropy * self._n_tot

    @property
    def S(self):
        """ Entropy (J/K)
        """
        return self.entropy

    @property
    def entropy_density(self):
        """ Entropy density (J/K/m3)
        """
        return self.specific_entropy / self._v

    @property
    def s_dens(self):
        """ Entropy density (J/K/m3)
        """
        return self.entropy_density

    @property
    def specific_residual_entropy(self):
        """ Specific residual entropy (J/mol/K)
        """
        if self._s_res is None:
            self._s_res = utils.back_compatible_unpack(self.eos.entropy_tv(self._T, self.specific_volume, self._x, property_flag="R"))
        return self._s_res

    @property
    def s_res(self):
        """ Specific residual entropy (J/mol/K)
        """
        return self.specific_residual_entropy

    @property
    def residual_entropy_density(self):
        """ Residual entropy density (J/K/m3)
        """
        return self.specific_residual_entropy / self._v

    @property
    def s_res_dens(self):
        """ Residual entropy density (J/K/m3)
        """
        return self.residual_entropy_density

    @property
    def residual_entropy(self):
        """ Residual entropy (J/K)
        """
        return self.specific_residual_entropy * self._n_tot

    @property
    def S_res(self):
        """ Residual entropy (J/K)
        """
        return self.residual_entropy

    @property
    def specific_helmholtz_energy(self):
        """ Specific Helmholtz energy (J/mol)
        """
        if self._a is None:
            self._a = utils.back_compatible_unpack(self.eos.helmholtz_tv(self._T, self.specific_volume, self._x))
        return self._a

    @property
    def a(self):
        """ Specific Helmholtz energy (J/mol)
        """
        return self.specific_helmholtz_energy

    @property
    def helmholtz_energy(self):
        """ Helmholtz energy (J)
        """
        return self.specific_helmholtz_energy*self._n_tot

    @property
    def A(self):
        """ Helmholtz energy (J)
        """
        return self.helmholtz_energy

    @property
    def helmholtz_energy_density(self):
        """ Helmholtz energy densty (J/m3)
        """
        return self.specific_helmholtz_energy / self._v

    @property
    def a_dens(self):
        """ Helmholtz energy densty (J/m3)
        """
        return self.helmholtz_energy_density

    @property
    def specific_residual_helmholtz_energy(self):
        """ Specific residual Helmholtz energy (J/mol)
        """
        if self._a_res is None:
            self._a_res = utils.back_compatible_unpack(self.eos.helmholtz_tv(self._T, self.specific_volume, self._x, property_flag="R"))
        return self._a_res

    @property
    def a_res(self):
        """ Specific residual Helmholtz energy (J/mol)
        """
        return self.specific_residual_helmholtz_energy

    @property
    def residual_helmholtz_energy_density(self):
        """ Residual Helmholtz energy density (J/m3)
        """
        return self.specific_residual_helmholtz_energy / self._v

    @property
    def a_res_dens(self):
        """ Residual Helmholtz energy density (J/m3)
        """
        return self.residual_helmholtz_energy_density

    @property
    def residual_helmholtz_energy(self):
        """ Residual Helmholtz energy (J/mol)
        """
        return self.specific_residual_helmholtz_energy * self._n_tot

    @property
    def A_res(self):
        """ Residual Helmholtz energy (J/mol)
        """
        return self.residual_helmholtz_energy

    @property
    def specific_energy(self):
        """ Specific energy (J/mol)
        """
        if self._u is None:
            self._u = utils.back_compatible_unpack(self.eos.internal_energy_tv(self._T, self.specific_volume, self._x))
        return self._u

    @property
    def u(self):
        """ Specific energy (J/mol)
        """
        return self.specific_energy

    @property
    def energy(self):
        """ Energy (J)
        """
        return self.specific_energy * self._n_tot

    @property
    def U(self):
        """ Energy (J)
        """
        return self.energy

    @property
    def energy_density(self):
        """ Energy density (J/m3)
        """
        return self.specific_energy / self._v

    @property
    def u_dens(self):
        """ Energy density (J/m3)
        """
        return self.energy_density

    @property
    def specific_residual_energy(self):
        """ Specific residual energy (J/mol)
        """
        if self._u_res is None:
            self._u_res = utils.back_compatible_unpack(self.eos.internal_energy_tv(self._T, self.specific_volume, self._x, property_flag="R"))
        return self._u_res

    @property
    def u_res(self):
        """ Specific residual energy (J/mol)
        """
        return self.specific_residual_energy

    @property
    def residual_energy(self):
        """ Residual energy (J)
        """
        return self.specific_residual_energy * self._n_tot

    @property
    def U_res(self):
        """ Residual energy (J)
        """
        return self.residual_energy

    @property
    def residual_energy_density(self):
        """ Residual energy density (J/m3)
        """
        return self.specific_residual_energy / self._v

    @property
    def u_res_dens(self):
        """ Residual energy density (J/m3)
        """
        return self.residual_energy_density

    @property
    def chemical_potential(self):
        """ Chemical potential (J/mol)
        """
        if self._mu is None:
            self._mu = utils.back_compatible_unpack(self.eos.chemical_potential_tv(self._T, self.specific_volume, self._x))
        return self._mu

    @property
    def mu(self):
        """ Chemical potential (J/mol)
        """
        return self.chemical_potential

    @property
    def residual_chemical_potential(self):
        """ Residual chemical potential (J/mol)
        """
        if self._mu_res is None:
            self._mu_res = utils.back_compatible_unpack(self.eos.chemical_potential_tv(self._T, self.specific_volume, self._x, property_flag="R"))
        return self._mu_res

    @property
    def mu_res(self):
        """ Residual chemical potential (J/mol)
        """
        return self.residual_chemical_potential

class Equilibrium(object):
    """
    VLLSE phase equilibrium
    """

    def __init__(self, vapour, liquid, liquid2=None, solid=None):
        """
        Create VLLE state
        """
        self.vapour = vapour
        self.liquid = liquid
        self.liquid1 = self.liquid
        self.liquid2 = liquid2
        self.solid = solid
        self.present_phase_list = [ {"state": vapour, "phase": "V"}, {"state": liquid, "phase": "L"}]
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
            output += f"{phase}: {state.temperature:.5f} K   {np.sum(state.partial_density)*1e-3:.5f} kmol/m3    {state.molefrac}"
        return output

    def __str__(self):
        output = "-----------------------------------------------------"
        for p in self.present_phase_list:
            output += "\n"
            state = p["state"]
            phase = p["phase"]
            output += f"{phase}: {state.temperature:.5f} K   {np.sum(state.partial_density)*1e-3:.5f} kmol/m3    {state.molefrac}"
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
        vg = utils.back_compatible_unpack(eos.specific_volume(T, p, y, eos.VAPPH)) \
            if phase in [eos.VAPPH, eos.TWOPH] else (None,)
        vl = utils.back_compatible_unpack(eos.specific_volume(T, p, x, eos.LIQPH)) \
            if phase in [eos.LIQPH, eos.TWOPH] else (None,)
        vapour = State(eos=eos, T=T, v=vg, n=y, p=p,
                      ph=eos.VAPPH, n_tot=betaV,
                      init_specific=True) \
            if phase in [eos.VAPPH, eos.TWOPH] else None
        liquid = State(eos=eos, T=T, V=vl, n=x, p=p,
                       ph=eos.LIQPH, n_tot=betaL,
                       init_specific=True) \
            if phase in [eos.LIQPH, eos.TWOPH] else None
        return Equilibrium(vapour, liquid)

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
        vg = utils.back_compatible_unpack(eos.specific_volume(T, p, y, eos.VAPPH))
        vl = utils.back_compatible_unpack(eos.specific_volume(T, p, z, eos.LIQPH))
        vapour = State(eos=eos, T=T, V=vg, n=y, p=p, ph=eos.VAPPH, n_tot=0.0,
                      init_specific=True)
        liquid = State(eos=eos, T=T, V=vl, n=z, p=p, ph=eos.LIQPH, n_tot=1.0,
                       init_specific=True)
        return Equilibrium(vapour, liquid)

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
        vg = utils.back_compatible_unpack(eos.specific_volume(T, p, y, eos.VAPPH))
        vl = utils.back_compatible_unpack(eos.specific_volume(T, p, z, eos.LIQPH))
        vapour = State(eos=eos, T=T, V=vg, n=y, p=p, ph=eos.VAPPH, n_tot=0.0,
                      init_specific=True)
        liquid = State(eos=eos, T=T, V=vl, n=z, p=p, ph=eos.LIQPH, n_tot=1.0,
                       init_specific=True)
        return Equilibrium(vapour, liquid)

    @property
    def temperature(self):
        return self.vapour.temperature if self.vapour else self.liquid.temperature

    @property
    def pressure(self):
        return self.vapour.pressure if self.vapour else self.liquid.pressure

    @property
    def eos(self):
        return self.vapour.eos if self.vapour else self.liquid.eos

class phase_state_list(State, list):
    """
    List of phase states. Utility class.

    This class inherits from State and list, such that it can be treated as both, with the exception that calling
    static methods inherited from State will raise an AttributeError.

    For example, after initializing a phase_state_list:
        psl = phase_state_list(<my_list>)

        psl.append(state)
        [s for s in psl]
        etc.
        work as if phase_state_list were a plain list.

        psl.chemical_potential()
        psl.specific_volume()
        etc.
        will return a numpy array generated by calling the expression on each element in the phase_state_list

        Similarly, properties work "as expected", such that
        psl.p
        psl.mu_res
        etc.
        will return numpy arrays generated by iterating over all states.
    """

    def __init__(self, states):
        """
        Create list
        """
        list.__init__(self, states)

        # Create the methods that ensure that a phase_state_list can be treated *as if* it were a state.
        # Iterate over the methods in State, excluding dunder methods and static methods.
        # For each method: Generate a lambda that creates a list by iterating over all states and calling that method.
        #                   Bind the lambda to the corresponding method in phase_state_list.
        # Properties are treated separately, as they must be set on the class, not on the instance.

        methods = inspect.getmembers(State, predicate=inspect.isfunction)
        properties = inspect.getmembers(State, lambda o: isinstance(o, property))
        for name, val in methods:
            if name[:2] != '__' and name[-2:] != '__' and not isinstance(State.__dict__[name], staticmethod):
                setattr(self, name, MethodType(
                    lambda *args, mname=name, **kwargs: np.array([state.__getattribute__(mname)(*(args[1:]), **kwargs) for
                                                         state in args[0]]), self))
            elif isinstance(State.__dict__[name], staticmethod):
                setattr(self, name, self.no_inherit_static)

        for name, val in properties:
            setattr(phase_state_list, name, property(lambda *args, mname=name: np.array([state.__getattribute__(mname)
                                                                                for state in self])))

    def no_inherit_static(self, *args, **kwargs):
        raise AttributeError('phase_state_list does not inherit static methods from State.')

    def __repr__(self):
        return [str(state) for state in self]

    def __str__(self):
        return '\n'.join(self.__repr__())

    @staticmethod
    def new_nvt(eos, T, V, n):
        return phase_state_list([State.new_nvt(eos, Ti, Vi, ni) for Ti, Vi, ni in zip(T, V, n)])

    @staticmethod
    def new_tpx(eos, T, p, x):
        return phase_state_list([State.new_tpx(eos, Ti, pi, xi) for Ti, pi, xi in zip(T, p, x)])

    @staticmethod
    def new_phx(eos, h, p, x):
        return phase_state_list([State.new_tpx(eos, hi, pi, xi) for hi, pi, xi in zip(h, p, x)])

    @staticmethod
    def new_mut(eos, mu, T, rho0):
        return phase_state_list([State.new_mut(eos, mui, Ti, rho0i) for mui, Ti, rho0i in zip(mu, T, rho0)])
    @property
    def temperatures(self):
        return np.array([state.temperature for state in self])

    @property
    def pressures(self):
        return np.array([state.pressure for state in self])

    @property
    def molefracs(self):
        return np.array([state.molefrac for state in self])

    @property
    def specific_volumes(self):
        return np.array([state.specific_volume for state in self])


class PhaseDiagram(object):
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
            PhaseDiagram: Phase diagram
        """
        assert eos.nc == 1
        z = np.ones(1)
        t_vals, p_vals, vl_vals, vg_vals = eos.get_pure_fluid_saturation_curve(initial_pressure=None,
                                                                               initial_temperature=T,
                                                                               i=None,
                                                                               max_delta_press=0.2e5,
                                                                               nmax=n,
                                                                               log_linear_grid=False)
        vle_states = []
        for i in range(len(t_vals)):
            vapour = State(eos=eos, T=t_vals[i], V=vg_vals[i], n=z, p=p_vals[i], n_tot=1.0,
                          init_specific=True)
            liquid = State(eos=eos, T=t_vals[i], V=vl_vals[i], n=z, p=p_vals[i], n_tot=1.0,
                           init_specific=True)
            vle_states.append(Equilibrium(vapour, liquid))
        return PhaseDiagram(vle_states)

    @staticmethod
    def binary_isotherm_vle(eos, T, maximum_pressure=1.5e7):
        """Construct PhaseDiagram from binary vle isotherm

        Args:
            eos (thermo.thermo): Equation of state object
            T (float): Temperature (K)
            maximum_pressure (float, optional): Pressure (Pa). Defaults to 1.5e7.

        Returns:
            PhaseDiagram: Phase diagram
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
            vg = utils.back_compatible_unpack(eos.specific_volume(T, p[i], yy, eos.VAPPH))
            xx = np.array([x[i], 1-x[i]])
            vl = utils.back_compatible_unpack(eos.specific_volume(T, p[i], xx, eos.LIQPH))
            vapour = State(eos=eos, T=T, V=vg, n=yy, p=p[i], n_tot=1.0,
                          init_specific=True)
            liquid = State(eos=eos, T=T, V=vl, n=xx, p=p[i], n_tot=1.0,
                           init_specific=True)
            vle_states.append(Equilibrium(vapour, liquid))
        return PhaseDiagram(vle_states)

    @staticmethod
    def binary_isobar_vle(eos, p, minimum_temperature=0.0):
        """Construct PhaseDiagram from binary vle isobar

        Args:
            eos (thermo.thermo): Equation of state object
            p (float): Pressure (Pa)
            minimum_temperaturte (float, optional): Temperature (K). Defaults to 0.0.

        Returns:
            PhaseDiagram: Phase diagram
        """
        _, L1VE, _ = eos.get_binary_txy(pressure=p,
                                        minimum_temperature=minimum_temperature,
                                        maximum_dz=0.003,
                                        maximum_dlns=0.01)
        vle_states = []
        x, y, T = L1VE
        for i in range(len(x)):
            yy = np.array([y[i], 1-y[i]])
            vg = utils.back_compatible_unpack(eos.specific_volume(T[i], p, yy, eos.VAPPH))
            xx = np.array([x[i], 1-x[i]])
            vl = utils.back_compatible_unpack(eos.specific_volume(T[i], p, xx, eos.LIQPH))
            vapour = State(eos=eos, T=T[i], V=vg, n=yy, p=p, n_tot=1.0,
                          init_specific=True)
            liquid = State(eos=eos, T=T[i], V=vl, n=xx, p=p, n_tot=1.0,
                           init_specific=True)
            vle_states.append(Equilibrium(vapour, liquid))
        return PhaseDiagram(vle_states)

    @property
    def liquid(self):
        return phase_state_list([vle.liquid for vle in self.vle_states])

    @property
    def vapour(self):
        return phase_state_list([vle.vapour for vle in self.vle_states])

    @property
    def temperatures(self):
        return phase_state_list([vle.vapour for vle in self.vle_states]).temperatures

    @property
    def pressures(self):
        return phase_state_list([vle.vapour for vle in self.vle_states]).pressures

class MetaCurve(object):
    """
    List of meta-stable states
    """

    def __init__(self, meta_states):
        """
        Assign list of States
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
        vz, rho = eos.map_meta_isotherm(temperature=T,
                                        z=z,
                                        phase=phase,
                                        n=n)
        meta_states = []
        for i in range(len(vz)):
            if phase == eos.LIQPH:
                vl = vz[i]
                vg = 1.0 / np.sum(rho[i,:])
                x = z
                y = rho[i,:] * vg
            else:
                vg = vz[i]
                vl = 1.0 / np.sum(rho[i,:])
                y = z
                x = rho[i,:]*vl
            vapour = State(eos=eos, T=T, V=vg, n=y, n_tot=1.0,
                          init_specific=True)
            liquid = State(eos=eos, T=T, V=vl, n=x, n_tot=1.0,
                           init_specific=True)
            meta_states.append(Equilibrium(vapour, liquid))
        return MetaCurve(meta_states)

    @property
    def liquid(self):
        return phase_state_list([meta.liquid for meta in self.meta_states])

    @property
    def vapour(self):
        return phase_state_list([meta.vapour for meta in self.meta_states])

    @property
    def temperatures(self):
        return phase_state_list([meta.vapour for meta in self.meta_states]).temperatures

    @property
    def pressures(self):
        return phase_state_list([meta.vapour for meta in self.meta_states]).pressures


class SpinodalCurve(object):
    """
    List of states along spinodal line
    """

    def __init__(self, eos, z, initial_pressure=1.0e5, initial_liquid_temperature=None):
        """Map spinodal states

        Args:
            eos (thermo): Equation of state object
            z (float): Composition (-)
            initial_pressure (float): Initial pressure (Pa). Defaults to 1.0e5.
            initial_liquid_temperature (float, optional): Initial temperature on liquid spinodal (K).
        """
        T, v, P = eos.spinodal(z,
                             initial_pressure=1.0e5,
                             initial_liquid_temperature=None,
                             dlnv=None,
                             min_temperature_vapor=None)

        self.spinodal_states =  []
        for i in range(len(v)):
            state = State(eos=eos, T=T[i], V=v[i], n=z, n_tot=1.0,
                          init_specific=True)
            self.spinodal_states.append(state)

    @property
    def specific_volume(self):
        return phase_state_list([spin for spin in self.spinodal_states]).specific_volume

    @property
    def temperatures(self):
        return phase_state_list([spin for spin in self.spinodal_states]).temperatures

    @property
    def pressures(self):
        return phase_state_list([spin for spin in self.spinodal_states]).pressures
