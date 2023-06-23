# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from .thermo import c_len_type
from .saft import saft


class saftvrmie(saft):
    """
    Interface to SAFT-VR Mie
    """

    def __init__(self, comps=None, parameter_reference="Default"):
        """Constructor
        Initialize SAFT-VR Mie model in thermopack.
        If no components are specified, model must be initialized for specific components later by direct call to 'init'
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        saft.__init__(self)

        # Options methods
        self.s_enable_hs = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "model_control_hs"))  # Option to enable/disable hard-sphere contribution
        self.s_enable_a1 = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "model_control_a1"))  # Option to enable/disable A1 contribution
        self.s_enable_a2 = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "model_control_a2"))  # Option to enable/disable A2 contribution
        self.s_enable_a3 = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "model_control_a3"))  # Option to enable/disable A1 contribution
        self.s_enable_chain = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "model_control_chain"))  # Option to enable/disable A1 contribution
        self.s_hs_reference = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "hard_sphere_reference"))  # Option to set HS model
        self.s_set_temperature_cache_flag = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "set_temperature_cache_flag"))  # Set flag controlling temperature cache

        # Init methods
        self.s_eoslibinit_init_saftvrmie = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_saftvrmie"))
        self.s_get_saftvrmie_pure_fluid_param = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "get_saftvrmie_pure_fluid_param"))
        self.s_set_saftvrmie_pure_fluid_param = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "set_saftvrmie_pure_fluid_param"))

        # Tuning methods
        self.s_get_eps_kij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "get_saftvrmie_eps_kij"))
        self.s_set_eps_kij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "set_saftvrmie_eps_kij"))
        self.s_get_sigma_lij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "get_saftvrmie_sigma_lij"))
        self.s_set_sigma_lij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "set_saftvrmie_sigma_lij"))
        self.s_get_lr_gammaij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "get_saftvrmie_lr_gammaij"))
        self.s_set_lr_gammaij = getattr(self.tp, self.get_export_name(
            "saftvrmie_containers", "set_saftvrmie_lr_gammaij"))

        # Model results
        self.s_calc_saftvrmie_term = getattr(self.tp, self.get_export_name(
            "saftvrmie_interface", "calc_saftvrmie_term"))
        self.s_calc_saftvrmie_term.argtypes = [POINTER(c_int),
                                               POINTER(c_double),
                                               POINTER(c_double),
                                               POINTER(c_double),
                                               POINTER(c_int)]
        self.s_calc_saftvrmie_term.restype = c_double


        # Define parameters to be set by init
        self.nc = None
        self.lambda_a = None
        self.lambda_r = None
        self.nc = None
        self.m = None
        self.sigma = None
        self.eps_div_kb = None
        self.lambda_a = None
        self.lambda_r = None

        if comps is not None:
            self.init(comps, parameter_reference=parameter_reference)

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Constructor
        Initialize SAFT-VR Mie model in thermopack

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_saftvrmie.argtypes = [c_char_p,
                                                     c_char_p,
                                                     c_len_type,
                                                     c_len_type]

        self.s_eoslibinit_init_saftvrmie.restype = None

        self.s_eoslibinit_init_saftvrmie(comp_string_c,
                                         ref_string_c,
                                         comp_string_len,
                                         ref_string_len)
        self.nc = max(len(comps.split(" ")), len(comps.split(",")))

        # Map pure fluid parameters
        self.m = np.zeros(self.nc)
        self.sigma = np.zeros(self.nc)
        self.eps_div_kb = np.zeros(self.nc)
        self.lambda_a = np.zeros(self.nc)
        self.lambda_r = np.zeros(self.nc)
        for i in range(self.nc):
            self.m[i], self.sigma[i], self.eps_div_kb[i], self.lambda_a[i], self.lambda_r[i] = \
                self.get_pure_fluid_param(i+1)

    def model_control_hard_sphere(self, active):
        """Model control
        Enable/disable hard-sphere term.

        Args:
            active (bool): Enable/disable hard-sphere dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_hs.argtypes = [POINTER(c_int)]
        self.s_enable_hs.restype = None
        self.s_enable_hs(byref(active_c))

    def set_hard_sphere_reference(self,
                                  reference,
                                  exact_binary_dhs=None,
                                  enable_hs_extra=None):
        """Model control
        Set hard-sphere reference.

        Args:
            reference (str): "LAFITTE", "ADDITIVE", "NON-ADDITIVE"
            exact_binary_dhs (bool): Calculate d_ij from sigma_ij and epsilon_ij
                                     or simply as d_ij = (d_ii + d_jj)/2
            enable_hs_extra (bool): Correction of A_HS due to non-additive d_ij
        """
        self.activate()
        if reference.upper() == "LAFITTE":
            hs_ref_c = c_int(1)
        else:
            is_non_additive = (reference.upper() == "NON-ADDITIVE" or
                               reference.upper() == "NONADDITIVE")
            hs_ref_c = c_int(4 if is_non_additive else 3)

        exact_binary_dhs_c = (POINTER(c_int)() if exact_binary_dhs is None
                              else POINTER(c_int)(c_int(exact_binary_dhs)))
        enable_hs_extra_c = (POINTER(c_int)() if enable_hs_extra is None
                             else POINTER(c_int)(c_int(enable_hs_extra)))

        self.s_hs_reference.argtypes = [POINTER(c_int),
                                        POINTER(c_int),
                                        POINTER(c_int)]
        self.s_hs_reference.restype = None
        self.s_hs_reference(byref(hs_ref_c),
                            exact_binary_dhs_c,
                            enable_hs_extra_c)

    def model_control_a1(self, active):
        """Model control
        Enable/disable first dispersion term.

        Args:
            active (bool): Enable/disable first dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a1.argtypes = [POINTER(c_int)]
        self.s_enable_a1.restype = None
        self.s_enable_a1(byref(active_c))

    def model_control_a2(self, active):
        """Model control
        Enable/disable second dispersion term.

        Args:
            active (bool): Enable/disable second dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a2.argtypes = [POINTER(c_int)]
        self.s_enable_a2.restype = None
        self.s_enable_a2(byref(active_c))

    def model_control_a3(self, active):
        """Model control
        Enable/disable third dispersion term.

        Args:
            active (bool): Enable/disable third dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a3.argtypes = [POINTER(c_int)]
        self.s_enable_a3.restype = None
        self.s_enable_a3(byref(active_c))

    def model_control_chain(self, active):
        """Model control
        Enable/disable chain term.

        Args:
            active (bool): Enable/disable chain term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_chain.argtypes = [POINTER(c_int)]
        self.s_enable_chain.restype = None
        self.s_enable_chain(byref(active_c))

    def enable_temperature_cache(self, enable=True):
        """Model performance
        Enable/disable temperature cache.

        Args:
            enable (bool): Enable/disable temperature cache
        """
        self.activate()
        enable_c = c_int(1 if enable else 0)
        self.s_set_temperature_cache_flag.argtypes = [POINTER(c_int)]
        self.s_set_temperature_cache_flag.restype = None
        self.s_set_temperature_cache_flag(byref(enable_c))

    def get_eps_kij(self, c1, c2):
        """Utility
        Get binary well depth interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            kij (float): Well depth interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = c_double(0.0)
        self.s_get_eps_kij.argtypes = [POINTER(c_int),
                                       POINTER(c_int),
                                       POINTER(c_double)]

        self.s_get_eps_kij.restype = None

        self.s_get_eps_kij(byref(c1_c),
                           byref(c2_c),
                           byref(kij_c))
        return kij_c.value

    def set_eps_kij(self, c1, c2, kij):
        """Utility
        Set binary well depth interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two
            kij (float): Well depth interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = c_double(kij)
        self.s_set_eps_kij.argtypes = [POINTER(c_int),
                                       POINTER(c_int),
                                       POINTER(c_double)]

        self.s_set_eps_kij.restype = None

        self.s_set_eps_kij(byref(c1_c),
                           byref(c2_c),
                           byref(kij_c))

    def get_sigma_lij(self, c1, c2):
        """Utility
        Get the interaction parameter lij for the sigma combining rule (controlling non-additivity)

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            lij (float): Sigma interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        lij_c = c_double(0.0)
        self.s_get_sigma_lij.argtypes = [POINTER(c_int),
                                         POINTER(c_int),
                                         POINTER(c_double)]

        self.s_get_sigma_lij.restype = None

        self.s_get_sigma_lij(byref(c1_c),
                             byref(c2_c),
                             byref(lij_c))
        return lij_c.value

    def set_sigma_lij(self, c1, c2, lij):
        """Utility
        Set the interaction parameter lij for the sigma combining rule (controlling non-additivity)

        Args:
            c1 (int): Component one
            c2 (int): Component two
            lij (float): Sigma interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        lij_c = c_double(lij)
        self.s_set_sigma_lij.argtypes = [POINTER(c_int),
                                         POINTER(c_int),
                                         POINTER(c_double)]

        self.s_set_sigma_lij.restype = None

        self.s_set_sigma_lij(byref(c1_c),
                             byref(c2_c),
                             byref(lij_c))

    def get_lr_gammaij(self, c1, c2):
        """Utility
        Get the interaction parameter gammaij for the lambda_r combining rule

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            gammaij (float): Repulsive exponent interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        gammaij_c = c_double(0.0)
        self.s_get_lr_gammaij.argtypes = [POINTER(c_int),
                                          POINTER(c_int),
                                          POINTER(c_double)]

        self.s_get_lr_gammaij.restype = None

        self.s_get_lr_gammaij(byref(c1_c),
                              byref(c2_c),
                              byref(gammaij_c))
        return gammaij_c.value

    def set_lr_gammaij(self, c1, c2, gammaij):
        """Utility
        Set the interaction parameter gammaij for the lambda_r combining rule

        Args:
            c1 (int): Component one
            c2 (int): Component two
            gammaij (float): Repulsive exponent interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        gammaij_c = c_double(gammaij)
        self.s_set_lr_gammaij.argtypes = [POINTER(c_int),
                                          POINTER(c_int),
                                          POINTER(c_double)]

        self.s_set_lr_gammaij.restype = None

        self.s_set_lr_gammaij(byref(c1_c),
                              byref(c2_c),
                              byref(gammaij_c))

    def get_pure_fluid_param(self, ic):
        """Utility
        Set pure fluid parameters

        Args:
            ic (int): Component index
        Returns;
            m (float): Mean number of segments.
            sigma (float): Temperature-independent segment diameter [m].
            eps_div_k (float): Well depth divided by Boltzmann's const. [K].
            lambda_a (float): Attractive exponent of the Mie potential
            lambda_r (float): Repulsive exponent of the Mie potential
        """
        self.activate()
        ic_c = c_int(ic)
        m_c = c_double(0.0)
        sigma_c = c_double(0.0)
        eps_c = c_double(0.0)
        lambda_a_c = c_double(0.0)
        lambda_r_c = c_double(0.0)
        self.s_get_saftvrmie_pure_fluid_param.argtypes = [POINTER(c_int),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double)]

        self.s_get_saftvrmie_pure_fluid_param.restype = None

        self.s_get_saftvrmie_pure_fluid_param(byref(ic_c),
                                              byref(m_c),
                                              byref(sigma_c),
                                              byref(eps_c),
                                              byref(lambda_a_c),
                                              byref(lambda_r_c))

        return m_c.value, sigma_c.value, eps_c.value, lambda_a_c.value, lambda_r_c.value

    def set_pure_fluid_param(self, ic, m, sigma, eps_div_kb, lambda_a, lambda_r):
        """Utility
        Set pure fluid parameters

        Args:
            ic (int): Component index
            m (float): Mean number of segments.
            sigma (float): Temperature-independent segment diameter [m].
            eps_div_kb (float): Well depth divided by Boltzmann's const. [K].
            lambda_a (float): Attractive exponent of the Mie potential
            lambda_r (float): Repulsive exponent of the Mie potential
        """
        self.activate()
        ic_c = c_int(ic)
        m_c = c_double(m)
        sigma_c = c_double(sigma)
        eps_c = c_double(eps_div_kb)
        lambda_a_c = c_double(lambda_a)
        lambda_r_c = c_double(lambda_r)
        self.s_set_saftvrmie_pure_fluid_param.argtypes = [POINTER(c_int),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double),
                                                          POINTER(c_double)]

        self.s_set_saftvrmie_pure_fluid_param.restype = None

        self.s_set_saftvrmie_pure_fluid_param(byref(ic_c),
                                              byref(m_c),
                                              byref(sigma_c),
                                              byref(eps_c),
                                              byref(lambda_a_c),
                                              byref(lambda_r_c))

        self.m[ic-1] = m
        self.sigma[ic-1] = sigma
        self.eps_div_kb[ic-1] = eps_div_kb
        self.lambda_a[ic-1] = lambda_a
        self.lambda_r[ic-1] = lambda_r

    def print_saft_parameters(self, c):
        """Utility
        Print saft parameters for component c

        Args:
            c (int): Component index (FORTRAN)

        """
        saft.print_saft_parameters(self, c)
        print(f"lambda_a: {self.lambda_a[c-1]}")
        print(f"lambda_r: {self.lambda_r[c-1]}")

    def a1(self, temp, volume, n):
        """Utility
        Get a1 term

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array like): Mol numbers (mol)
        Returns:
            a1 (float): First order perturbation (K/mol)
        """
        nc_c = c_int(self.nc)
        t_c = c_double(temp)
        v_c = c_double(volume)
        n_c = (c_double * self.nc)(*n)
        term_c = c_int(1)
        a = self.s_calc_saftvrmie_term(byref(nc_c),
                                       byref(t_c),
                                       byref(v_c),
                                       n_c,
                                       byref(term_c))

        return a

    def a2(self, temp, volume, n):
        """Utility
        Get a2 term

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array like): Mol numbers (mol)
        Returns:
            a2 (float): Second order perturbation (K^2/mol)
        """
        nc_c = c_int(self.nc)
        t_c = c_double(temp)
        v_c = c_double(volume)
        n_c = (c_double * self.nc)(*n)
        term_c = c_int(2)
        a = self.s_calc_saftvrmie_term(byref(nc_c),
                                       byref(t_c),
                                       byref(v_c),
                                       n_c,
                                       byref(term_c))

        return a

    def a3(self, temp, volume, n):
        """Utility
        Get a3 term

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array like): Mol numbers (mol)
        Returns:
            a3 (float): Second order perturbation (K^3/mol)
        """
        nc_c = c_int(self.nc)
        t_c = c_double(temp)
        v_c = c_double(volume)
        n_c = (c_double * self.nc)(*n)
        term_c = c_int(3)
        a = self.s_calc_saftvrmie_term(byref(nc_c),
                                       byref(t_c),
                                       byref(v_c),
                                       n_c,
                                       byref(term_c))

        return a

    def a_hs(self, temp, volume, n):
        """Utility
        Get hardsphere term

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array like): Mol numbers (mol)
        Returns:
            a_hs (float): Second order perturbation (1/mol)
        """
        nc_c = c_int(self.nc)
        t_c = c_double(temp)
        v_c = c_double(volume)
        n_c = (c_double * self.nc)(*n)
        term_c = c_int(0)
        a = self.s_calc_saftvrmie_term(byref(nc_c),
                                       byref(t_c),
                                       byref(v_c),
                                       n_c,
                                       byref(term_c))

        return a
