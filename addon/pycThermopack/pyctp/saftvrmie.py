# Support for python2
from __future__ import print_function
# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from . import thermo

c_len_type = thermo.c_len_type


class saftvrmie(thermo.thermopack):
    """
    Interface to SAFT-VR Mie
    """

    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(saftvrmie, self).__init__()

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

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Initialize SAFT-VR Mie model in thermopack

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

    def model_control_hard_sphere(self, active):
        """Model control. Enable/disable hard-sphere term.

        Args:
            active (bool): Enable/disable hard-sphere dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_hs.argtypes = [POINTER(c_int)]
        self.s_enable_hs.restype = None
        self.s_enable_hs(byref(active_c))

    def model_control_a1(self, active):
        """Model control. Enable/disable first dispersion term.

        Args:
            active (bool): Enable/disable first dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a1.argtypes = [POINTER(c_int)]
        self.s_enable_a1.restype = None
        self.s_enable_a1(byref(active_c))

    def model_control_a2(self, active):
        """Model control. Enable/disable second dispersion term.

        Args:
            active (bool): Enable/disable second dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a2.argtypes = [POINTER(c_int)]
        self.s_enable_a2.restype = None
        self.s_enable_a2(byref(active_c))

    def model_control_a3(self, active):
        """Model control. Enable/disable third dispersion term.

        Args:
            active (bool): Enable/disable third dispersion term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_a3.argtypes = [POINTER(c_int)]
        self.s_enable_a3.restype = None
        self.s_enable_a3(byref(active_c))

    def model_control_chain(self, active):
        """Model control. Enable/disable chain term.

        Args:
            active (bool): Enable/disable chain term
        """
        self.activate()
        active_c = c_int(1 if active else 0)
        self.s_enable_chain.argtypes = [POINTER(c_int)]
        self.s_enable_chain.restype = None
        self.s_enable_chain(byref(active_c))

    def get_eps_kij(self, c1, c2):
        """Get binary well depth interaction parameter

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
        """Set binary well depth interaction parameter

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
        """Get the interaction parameter lij for the sigma combining rule (controlling non-additivity)

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
        """Set the interaction parameter lij for the sigma combining rule (controlling non-additivity)

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
        """Get the interaction parameter gammaij for the lambda_r combining rule

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
        """Set the interaction parameter gammaij for the lambda_r combining rule

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
        """Set pure fluid parameters

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

    def set_pure_fluid_param(self, ic, m, sigma, eps_div_k, lambda_a, lambda_r):
        """Set pure fluid parameters

        Args:
            ic (int): Component index
            m (float): Mean number of segments.
            sigma (float): Temperature-independent segment diameter [m].
            eps_div_k (float): Well depth divided by Boltzmann's const. [K].
            lambda_a (float): Attractive exponent of the Mie potential
            lambda_r (float): Repulsive exponent of the Mie potential
        """
        self.activate()
        ic_c = c_int(ic)
        m_c = c_double(m)
        sigma_c = c_double(sigma)
        eps_c = c_double(eps_div_k)
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
