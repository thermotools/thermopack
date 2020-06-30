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
    Interface to cubic
    """
    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(saftvrmie, self).__init__()

        # Options methods
        self.enable_hs_c = c_int.in_dll(self.tp, self.get_export_name("saftvrmie_options", "enable_hs")) # Option to enable/disable hard-sphere contribution
        self.enable_a1_c = c_int.in_dll(self.tp, self.get_export_name("saftvrmie_options", "enable_a1")) # Option to enable/disable A1 contribution
        self.enable_a2_c = c_int.in_dll(self.tp, self.get_export_name("saftvrmie_options", "enable_a2")) # Option to enable/disable A2 contribution
        self.enable_a3_c = c_int.in_dll(self.tp, self.get_export_name("saftvrmie_options", "enable_a3")) # Option to enable/disable A1 contribution
        self.enable_chain_c = c_int.in_dll(self.tp, self.get_export_name("saftvrmie_options", "enable_chain")) # Option to enable/disable A1 contribution

        # Init methods
        self.eoslibinit_init_saftvrmie = getattr(self.tp, self.get_export_name("eoslibinit", "init_saftvrmie"))

        # Tuning methods
        self.s_get_eps_kij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "get_saftvrmie_eps_kij"))
        self.s_set_eps_kij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "set_saftvrmie_eps_kij"))
        self.s_get_sigma_lij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "get_saftvrmie_sigma_lij"))
        self.s_set_sigma_lij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "set_saftvrmie_sigma_lij"))
        self.s_get_lr_gammaij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "get_saftvrmie_lr_gammaij"))
        self.s_set_lr_gammaij = getattr(self.tp, self.get_export_name("saftvrmie_containers", "set_saftvrmie_lr_gammaij"))

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """
        Initialize SAFT-VR Mie model in thermopack
        """
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_saftvrmie.argtypes = [c_char_p,
                                                   c_char_p,
                                                   c_len_type,
                                                   c_len_type]

        self.eoslibinit_init_saftvrmie.restype = None

        self.eoslibinit_init_saftvrmie(comp_string_c,
                                       ref_string_c,
                                       comp_string_len,
                                       ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))

    def model_control_hard_sphere(self, active):
        """Model control. Enable/disable hard-sphere term.

        Args:
            active (bool): Enable/disable hard-sphere dispersion term
        """
        if active:
            self.enable_hs_c.value = 1
        else:
            self.enable_hs_c.value = 0

    def model_control_a1(self, active):
        """Model control. Enable/disable first dispersion term.

        Args:
            active (bool): Enable/disable first dispersion term
        """
        if active:
            self.enable_a1_c.value = 1
        else:
            print("zero")
            self.enable_a1_c.value = 0

    def model_control_a2(self, active):
        """Model control. Enable/disable second dispersion term.

        Args:
            active (bool): Enable/disable second dispersion term
        """
        if active:
            self.enable_a2_c.value = 1
        else:
            self.enable_a2_c.value = 0

    def model_control_a3(self, active):
        """Model control. Enable/disable third dispersion term.

        Args:
            active (bool): Enable/disable third dispersion term
        """
        if active:
            self.enable_a3_c.value = 1
        else:
            self.enable_a3_c.value = 0

    def model_control_chain(self, active):
        """Model control. Enable/disable chain term.

        Args:
            active (bool): Enable/disable chain term
        """
        if active:
            self.enable_chain_c.value = 1
        else:
            self.enable_chain_c.value = 0

    def get_eps_kij(self, c1, c2):
        """Get binary well depth interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            kij (float): Well depth interaction parameter
        """
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
