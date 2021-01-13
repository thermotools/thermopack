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

class ljs_bh(thermo.thermopack):
    """
    Interface to LJS-BH
    """
    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(ljs_bh, self).__init__()

        # Options methods
        self.s_ljs_bh_model_control = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_model_control"))

        # Init methods
        self.s_eoslibinit_init_ljs_bh = getattr(self.tp, self.get_export_name("eoslibinit", "init_ljs_bh"))
        self.s_ljs_bh_get_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_get_pure_params"))
        self.s_ljs_bh_set_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_set_pure_params"))


    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default"):
        """Initialize Lennard-Jomes splined model in thermopack

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_ljs_bh.argtypes = [c_char_p,
                                                  c_len_type]

        self.s_eoslibinit_init_ljs_bh.restype = None

        self.s_eoslibinit_init_ljs_bh(ref_string_c,
                                      ref_string_len)

        self.nc = 1


    def get_sigma_eps(self):
        """Get particle size and well depth

        Returns:
            sigma (float): Particle diameter (m)
            eps_depth_divk (float): Well depth divided by Boltzmann constant (K)
        """
        self.activate()
        sigma_c = c_double(0.0)
        eps_depth_divk_c = c_double(0.0)
        self.s_ljs_bh_get_pure_params.argtypes = [POINTER(c_double),
                                                  POINTER(c_double)]

        self.s_ljs_bh_get_pure_params.restype = None

        self.s_ljs_bh_get_pure_params(byref(sigma_c),
                                      byref(eps_depth_divk_c))
        return sigma_c.value, eps_depth_divk_c.value

    def set_sigma_eps(self, sigma, eps_depth_divk):
        """Set particle size and well depth

        Args:
            sigma (float): Particle diameter (m)
            eps_depth_divk (float): Well depth divided by Boltzmann constant (K)
        """
        self.activate()
        sigma_c = c_double(sigma)
        eps_depth_divk_c = c_double(eps_depth_divk)
        self.s_ljs_bh_set_pure_params.argtypes = [POINTER(c_double),
                                                  POINTER(c_double)]

        self.s_ljs_bh_set_pure_params.restype = None

        self.s_ljs_bh_set_pure_params(byref(sigma_c),
                                      byref(eps_depth_divk_c))


    #################################
    # Model options
    #################################
    def model_control(self,
                      use_Lafitte_a3=False,
                      enable_chi_correction=True,
                      enable_hs=True,
                      enable_a1=True,
                      enable_a2=True,
                      enable_a3=True):
        """Model control. Enable/disable model terms.

        Args:
            use_Lafitte_a3 (bool): Enable/disable use of Lafitte model for a3 dispersion term. Defaults to False.
            enable_chi_correction (bool): Enable/disable use of chi correction for a2 dispersion term. Defaults to True.
            enable_hs (bool): Enable/disable hard-sphere term. Defaults to True.
            enable_a1 (bool): Enable/disable use of a1 dispersion term. Defaults to True.
            enable_a2 (bool): Enable/disable use of a1 dispersion term. Defaults to True.
            enable_a3 (bool): Enable/disable use of a1 dispersion term. Defaults to True.
        """
        self.activate()
        use_Lafitte_a3_c = c_int(use_Lafitte_a3)
        enable_chi_correction_c = c_int(enable_chi_correction)
        enable_hs_c = c_int(enable_hs)
        enable_a1_c = c_int(enable_a1)
        enable_a2_c = c_int(enable_a2)
        enable_a3_c = c_int(enable_a3)

        self.s_ljs_bh_model_control.argtypes = [POINTER(c_int),
                                                POINTER(c_int),
                                                POINTER(c_int),
                                                POINTER(c_int),
                                                POINTER(c_int),
                                                POINTER(c_int)]

        self.s_ljs_bh_model_control.restype = None

        self.s_ljs_bh_model_control(byref(use_Lafitte_a3_c),
                                    byref(enable_chi_correction_c),
                                    byref(enable_hs_c),
                                    byref(enable_a1_c),
                                    byref(enable_a2_c),
                                    byref(enable_a3_c))
