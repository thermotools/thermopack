# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
import warnings
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from .thermo import thermo, c_len_type


class ideal(thermo):
    """
    Interface to ideal eos
    """
    def __init__(self, comps=None, parameter_reference="Default"):
        """Constructor
        Initialize ideal model in thermopack

        Unless 'comps' parameter is specified, model must be initialized for specific components
        later by direct call to 'init'.
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        super(ideal, self).__init__()

        # Init methods
        self.eoslibinit_init_ideal = getattr(self.tp, self.get_export_name("eoslibinit", "init_ideal_eos"))

        if comps is not None:
            self.init(comps, parameter_reference)
        else:
            missing_args = ['comps']
            warnings.warn('Ideal EoS not completely initialized, due to missing parameter(s) :'+str(missing_args)+'.\n'
                          'Complete initialisation by explicitly calling this classes "init" method.', Warning)

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Constructor
        Initialize ideal model in thermopack

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))
        ierr_c = c_int(0)

        self.eoslibinit_init_ideal.argtypes = [c_char_p,
                                               POINTER (c_int),
                                               c_char_p,
                                               c_len_type,
                                               c_len_type]

        self.eoslibinit_init_ideal.restype = None

        self.eoslibinit_init_ideal(comp_string_c,
                                   ierr_c,
                                   ref_string_c,
                                   comp_string_len,
                                   ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))


    def bubble_temperature(self, press, z):
        """Saturation interface
        Calculate bubble temperature given pressure and composition

        Args:
            press (float): Pressure (Pa)
            z (array_like): Composition (-)

        Raises:
            Exception: Faild to calculate

        Returns:
            float: Temperature (K)
            ndarray: Incipient phase composition
        """
        raise NotImplementedError("ideal does not support this method")

    def bubble_pressure(self, temp, z):
        """Saturation interface
        Calculate bubble pressure given temperature and composition

        Args:
            temp (float): Temperature (K)
            z (array_like): Composition (-)

        Raises:
            Exception: Faild to calculate

        Returns:
            float: Pressure (Pa)
            ndarray: Incipient phase composition
        """
        raise NotImplementedError("ideal does not support this method")

    def dew_temperature(self, press, z):
        """Saturation interface
        Calculate dew temperature given pressure and composition

        Args:
            press (float): Pressure (Pa)
            z (float): Composition (-)

        Raises:
            Exception: Not able to solve for dew point

        Returns:
            float : Temperature (K)
            ndarray : Incipient phase composition (-)
        """
        raise NotImplementedError("ideal does not support this method")

    def dew_pressure(self, temp, z):
        """Saturation interface
        Calculate dew pressure given temperature and composition

        Args:
            temp (float): Temperature (K)
            z (float): Composition (-)

        Raises:
            Exception: Not able to solve for dew point

        Returns:
            float : Pressure (Pa)
            ndarray : Incipient phase composition (-)
        """
        raise NotImplementedError("ideal does not support this method")

    def get_envelope_twophase(self, initial_pressure, z, maximum_pressure=1.5e7,
                              minimum_temperature=None, step_size_factor=1.0,
                              step_size=None, calc_v=False, initial_temperature=None,
                              calc_criconden=False):
        """Saturation interface
        Get the phase-envelope at a given composition

        Args:
            initial_pressure (float): Start mapping form dew point at initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_temperature (float , optional): Exit on minimum temperature (K). Defaults to None.
            step_size_factor (float , optional): Scale default step size for envelope trace. Defaults to 1.0. Reducing step_size_factor will give a denser grid.
            step_size (float , optional): Set maximum step size for envelope trace. Overrides step_size_factor. Defaults to None.
            calc_v (bool, optional): Calculate specific volume of saturated phase? Defaults to False
            initial_temperature (float, optional): Start mapping form dew point at initial temperature.
                                                   Overrides initial pressure. Defaults to None (K).
            calc_criconden (bool, optional): Calculate cricondenbar and cricondentherm?
        Returns:
            ndarray: Temperature values (K)
            ndarray: Pressure values (Pa)
            ndarray (optional, if `calc_v=True`): Specific volume (m3/mol)
            ndarray (optional, if `calc_criconden=True`): Cricondenbar followed by cricondentherm (T (K), P (Pa), T (K), P (Pa))
        """
        raise NotImplementedError("ideal does not support this method")

    def get_pure_fluid_saturation_curve(self,
                                        initial_pressure,
                                        initial_temperature=None,
                                        i=None,
                                        max_delta_press=0.2e5,
                                        nmax=100,
                                        log_linear_grid=False):
        """Saturation interface
        Get the pure fluid saturation line

        To start mapping from and initial temperature, use:
        get_pure_fluid_saturation_curve(None, initial_temperature=<my_temp>)

        Args:
            initial_pressure (float): Start mapping form dew point at initial pressure (Pa).
            initial_temperature (float, optional): Start mapping form dew point at initial temperature (K). Default None.
            i (int, optional): FORTRAN component index. Default None. Must be given if self.nc > 1.
            max_delta_press (float , optional): Maximum delta pressure between points (Pa). Defaults to 0.2e5.
            nmax (int, optional): Maximum number of points on envelope. Defaults to 100.
            log_linear_grid (logical, optional): Use log-linear grid?. Defaults to False.

        Returns:
            ndarray: Temperature values (K)
            ndarray: Pressure values (Pa)
            ndarray: Specific liquid volume (m3/mol)
            ndarray: Specific gas volume (m3/mol)
        """
        raise NotImplementedError("ideal does not support this method")

    def get_binary_pxy(self,
                       temp,
                       maximum_pressure=1.5e7,
                       minimum_pressure=1.0,
                       maximum_dz=0.003,
                       maximum_dlns=0.01):
        """Saturation interface
        Calculate binary three phase envelope

        Args:
            temp (float): Temperature (K)
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0.
            maximum_dz (float, optional): Maximum composition step. Defaults to 0.003.
            maximum_dlns (float, optional): Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

        Returns:
            (XYDiagram) : Structure with the attributes

            lle : Liquid 1 - Liquid 2 Equilibrium (PxyEquilibrium) with the attributes
                x1 -> Liquid 1 composition (mole fraction of component 1)
                x2 -> Liquid 2 composition (mole fraction of component 1)
                p -> Pressure [Pa]
            l1ve : Liquid 1 - Vapour Equilibrium (PxyEquilibrium) with the attributes
                x -> Bubble line composition (mole fraction of component 1)
                y -> Dew line composition (mole fraction of component 1)
                p -> Pressure [Pa]
            l2ve : Liquid 2 - Vapour Equilibrium (PxyEquilibrium) with the attributes
                x -> Bubble line composition (mole fraction of component 1)
                y -> Dew line composition (mole fraction of component 1)
                p -> Pressure [Pa]

            If one or more of the equilibria are not found the corresponding arrays are empty
        """
        raise NotImplementedError("ideal does not support this method")

    def get_binary_txy(self,
                       pressure,
                       minimum_temperature=0.0,
                       maximum_dz=0.003,
                       maximum_dlns=0.005):
        """Saturation interface
        Calculate binary isobaric three phase envelope

        Args:
            pressure (float): Pressure (Pa)
            minimum_temperature (float, optional): Exit on minimum temperature (K).
            maximum_dz (float, optional): Maximum composition step. Defaults to 0.003.
            maximum_dlns (float, optional): Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

        Returns:
            (XYDiagram) : Structure with the attributes

            lle : Liquid 1 - Liquid 2 Equilibrium (TxyEquilibrium) with the attributes
                x1 -> Liquid 1 composition (mole fraction of component 1)
                x2 -> Liquid 2 composition (mole fraction of component 1)
                T -> Temperature [K]
            l1ve : Liquid 1 - Vapour Equilibrium (TxyEquilibrium) with the attributes
                x -> Bubble line composition (mole fraction of component 1)
                y -> Dew line composition (mole fraction of component 1)
                T -> Temperature [K]
            l2ve : Liquid 2 - Vapour Equilibrium (TxyEquilibrium) with the attributes
                x -> Bubble line composition (mole fraction of component 1)
                y -> Dew line composition (mole fraction of component 1)
                T -> Temperature [K]

            If one or more of the equilibria are not found the corresponding arrays are empty
        """
        raise NotImplementedError("ideal does not support this method")

    def get_bp_term(self,
                    i_term):
        """Saturation interface
        Get error description for binary plot error

        Args:
            i_term (int): binary plot error identifier

        Returns:
            str: Error message
        """
        raise NotImplementedError("ideal does not support this method")

    def binary_triple_point_pressure(self,
                                     temp,
                                     maximum_pressure=1.5e7,
                                     minimum_pressure=1.0e4):
        """Saturation interface
        Calculate triple point for binary mixture at specified temperature

        Args:
            temp (float): Temperature (K)
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0e4.

        Returns:
            has_triple_point (boolean): Does the mixture have a triple point?
            x (np.ndarray): Liquid 1 composition
            y (np.ndarray): Gas composition
            w (np.ndarray): Liquid 2 composition
            P (float): Pressure (Pa)
        """
        raise NotImplementedError("ideal does not support this method")

    def global_binary_plot(self,
                           maximum_pressure=1.5e7,
                           minimum_pressure=1.0e5,
                           minimum_temperature=150.0,
                           maximum_temperature=500.0,
                           include_azeotropes=False):
        """Saturation interface
        Calculate global binary phase envelope

        Args:
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0e5.
            minimum_temperature (float, optional): Terminate phase line traceing at minimum temperature. Defaults to 150.0 K.
            maximum_temperature (float, optional): Terminate phase line traceing at maximum temperature. Defaults to 500.0 K.
            include_azeotropes (bool, optional): Include azeotropic lines. Defaults to False.

        Returns:
            tuple of arrays
        """
        raise NotImplementedError("ideal does not support this method")

    def solid_envelope_plot(self, initial_pressure, z, maximum_pressure=1.5e7,
                            minimum_temperature=170.0, calc_esv=False):
        """Saturation interface
        Calculate phase envelope including solid lines

        Args:
            initial_pressure (float): Start mapping from initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            calc_esv (bool, optional): Calculate specific volume of saturated phase? Defaults to False

        Returns:
            tuple of arrays
        """
        raise NotImplementedError("ideal does not support this method")

    def envelope_isentrope_cross(self, entropy, initial_pressure, z, maximum_pressure=1.5e7,
                              minimum_temperature=None, step_size=None, initial_temperature=None):
        """Saturation interface
        Get saturated phase having given entropy. Searches the binodal by
        tracing it upwards in pressure from the dew point at initial_pressure.
        Args:
            entropy (float): Entropy (J/mol/K).
            initial_pressure (float): Start search from dew point at initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Stop envelope tracking at maximum pressure (Pa). Defaults to 1.5e7.
            minimum_temperature (float , optional): Exit envelope tracking minimumtemperature (K). Defaults to None.
            step_size (float , optional): Set maximum step size for envelope trace. Defaults to None.
            minimum_temperature (float, optional): Not in use
            initial_temperature (float, optional): Start search from dew point at initial temperature.
                                                  Overrides initial pressure. Defaults to None (K).
        Returns:
            float: Temperature values (K)
            foat: Pressure values (Pa)
            float: Specific volume (m3/mol)
            int: Phase flag for main phase
            ndarray: Incipient composition (mol/mol)
        """
        raise NotImplementedError("ideal does not support this method")

    def saturation_points_from_property(self,
                                        initial_pressure,
                                        z,
                                        prop_grid,
                                        prop,
                                        maximum_pressure=1.5e7,
                                        minimum_temperature=None,
                                        step_size=None):
        """Saturation interface
        Get saturated points intersecting with properties given as input.
        Args:
            initial_pressure (float): Start search from dew point at initial pressure (Pa).
            z (array_like): Composition (-)
            prop_grid (array like): Property values where intersect is needed
            prop (str): Property (Entropy, Enthalpy, Volume, Pressure, Temperature, Joule-Thompson)
            maximum_pressure (float , optional): Stop envelope tracking at maximum pressure (Pa). Defaults to 1.5e7.
            minimum_temperature (float , optional): Exit envelope tracking minimumtemperature (K). Defaults to None.
            step_size (float , optional): Set maximum step size for envelope trace. Defaults to None.

        Returns:
            float: Temperature values (K)
            foat: Pressure values (Pa)
            float: Specific volume (m3/mol)
            int: Phase flag for main phase
            ndarray: Incipient composition (mol/mol)
        """
        raise NotImplementedError("ideal does not support this method")

    def saturation_point_from_property_bracket_search(self,
                                                      z,
                                                      prop_val,
                                                      prop,
                                                      temp_1,
                                                      press_1,
                                                      x_1,
                                                      y_1,
                                                      temp_2,
                                                      press_2,
                                                      x_2,
                                                      y_2):
        """Saturation interface
        Get saturated point intersecting with property given as input. Point 1 and 2 are saturation
        states bracketing the solution.

        Args:
            z (array_like): Composition (-)
            prop_val (float): Property value where intersect is needed
            prop (str): Property (Entropy, Enthalpy, Volume, Pressure, Temperature, Joule-Thompson)
            temp_1 (float): Temperature of point 1 (K).
            press_1 (float): Pressure of point 1 (Pa).
            x_1 (float): Liquid compozition of point 1 (mol/mol).
            y_1 (float): Vapour compozition of point 1 (mol/mol).
            temp_2 (float): Temperature of point 2 (K).
            press_2 (float): Pressure of point 2 (Pa).
            x_2 (float): Liquid compozition of point 2 (mol/mol).
            y_2 (float): Vapour compozition of point 2 (mol/mol).

        Raises:
            Exception: Not able to solve for property

        Returns:
            float: Temperature values (K)
            float: Pressure values (Pa)
            ndarray: Liquid composition (mol/mol)
            ndarray: Vapour composition (mol/mol)
        """
        raise NotImplementedError("ideal does not support this method")

    def critical(self, n, temp=0.0, v=0.0, tol=1.0e-7, v_min=None):
        """Stability interface
        Calculate critical point in variables T and V

        Args:
            n (array_like): Mol numbers (mol)
            temp (float, optional): Initial guess for temperature (K). Defaults to 0.0.
            v (float, optional): Initial guess for volume (m3/mol). Defaults to 0.0.
            tol (float, optional): Error tolerance (-). Defaults to 1.0e-8.
            v_min (float, optional): Minimum volume for search (m3/mol). Defaults to None.

        Raises:
            Exception: Failure to solve for critical point

        Returns:
            float: Temperature (K)
            float: Volume (m3/mol)
            float: Pressure (Pa)
        """
        raise NotImplementedError("ideal does not support this method")

    def spinodal(self,
                 z,
                 initial_pressure=1.0e5,
                 initial_liquid_temperature=None,
                 dlnv=None,
                 min_temperature_vapor=None):
        """Stability interface
        Trace spinodal curve

        Args:
            z (array_like): Composition (-)
            initial_pressure (float): Initial pressure (Pa). Defaults to 1.0e5.
            initial_liquid_temperature (float, optional): Initial temperature on liquid spinodal (K).
            dlnv (float, optional): Override step size (-).
            min_vapor_temperature (float, optional): Minimum temperature on vapor spinodal (K).

        Raises:
            Exception: Failure to trace spinodal

        Returns:
            np.ndarray: Temperature (K)
            np.ndarray: Volume (m3/mol)
            np.ndarray: Pressure (Pa)
        """
        raise NotImplementedError("ideal does not support this method")

    def spinodal_point(self,
                       z,
                       pressure,
                       phase,
                       temperature=None):
        """Stability interface
        Solve for spinodal curve point. Not able to solve for points close to critical point.
        Solve for temperature if given, otherwise solve for pressure.

        Args:
            z (array_like): Composition (-)
            pressure (float): Pressure (Pa)
            phase (int): Phase flag (VAPPH/LIQPH)
            temperature (float, optional): Temperature (K). Solve for temperature if given.

        Raises:
            Exception: Failure to solve for spinodal curve point

        Returns:
            float: Temperature (K)
            float: Volume (m3/mol)
        """
        raise NotImplementedError("ideal does not support this method")

    def map_meta_isentrope(self,
                           z,
                           initial_pressure,
                           entropy,
                           minimum_pressure,
                           n_max=50):
        """Stability interface & Isoline
        Trace isentrope into meta-stable region. Trace from pressure to minimum_pressure

        Args:
            z (array_like): Composition (-)
            initial_pressure (float): Initial pressure (Pa)
            entropy (float): Entropy (J/mol/K).
            minimum_pressure (float): Minimum pressure (Pa).
            n_max (int): Number of points on curve. Default 50.

        Raises:
            Exception: Failure to map isentrope

        Returns:
            np.ndarray: Temperature (K)
            np.ndarray: Volume (m3/mol)
            np.ndarray: Pressure (Pa)
        """
        raise NotImplementedError("ideal does not support this method")

    def map_meta_isotherm(self,
                          temperature,
                          z,
                          phase,
                          n=50):
        """Stability interface & Isoline
        Trace isotherm from saturation line to spinodal. Solve for phase in
        chemical and thermal equilibrium with a phase defined by z anf phase flag..

        Args:
            temperature (float): Temperature (K)
            z (array_like): Composition (-)
            phase (float): Phase with composition z (LIQPH or VAPPH)
            n (int): Number of points on curve. Default 50.

        Raises:
            Exception: Failure to map isotherm

        Returns:
            np.ndarray: Volume of meta-stable phase (m3/mol)
            np.ndarray: Density (mol/m3) of equilibrium phase in each point, dimension (n,nc).
        """
        raise NotImplementedError("ideal does not support this method")

    def tv_meta_ps(self, pressure, entropy, n, volume_initial, temp_initial):
        """Stability interface & Other property
        Solve for temperature and volume given pressure and entropy.
        A fair initial guess is required.
        No phase stability is tested, and stable/meta-stable states will be
        returned depending on input.

        Args:
            pressure (float): Pressure (Pa).
            entropy (float): Entropy (J/K).
            n (array_like): Mol numbers (mol)
            volume_initial (float): Initial guess for volume (m3).
            temp_initial (float): Initial guess for temperature (K)

        Returns:
            float: Temperature (K)
            float: Volume (m3).
        """
        raise NotImplementedError("ideal does not support this method")

