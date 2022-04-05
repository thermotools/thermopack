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
from abc import abstractmethod

c_len_type = thermo.c_len_type


class ljs_wca_base(thermo.thermopack):
    """
    Interface to LJS-WCA
    """
    def __init__(self):
        """
        Initialize wca specific function pointers
        """
        # Load dll/so
        super(ljs_wca_base, self).__init__()

        # Init methods
        self.s_eoslibinit_init_ljs = getattr(self.tp, self.get_export_name("eoslibinit", "init_ljs"))
        self.s_ljs_wca_get_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_wca_get_pure_params"))
        self.s_ljs_wca_set_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_wca_set_pure_params"))

    @abstractmethod
    def init(self, parameter_reference="Default"):
        pass

    @abstractmethod
    def model_control(self):
        pass

    def _base_init(self, model, parameter_reference="Default"):
        """Initialize Lennard-Jomes splined model based on Weeks-Chandler-Anderson perturbation theory

        Args:
            model (str, optional): Which model to use?. ("WCA", "UV", UF)
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        model_string_c = c_char_p(model.encode('ascii'))
        model_string_len = c_len_type(len(model))

        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_ljs.argtypes = [c_char_p,
                                               c_char_p,
                                               c_len_type,
                                               c_len_type]

        self.s_eoslibinit_init_ljs.restype = None

        self.s_eoslibinit_init_ljs(model_string_c,
                                   ref_string_c,
                                   model_string_len,
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
        self.s_ljs_wca_get_pure_params.argtypes = [POINTER(c_double),
                                                   POINTER(c_double)]

        self.s_ljs_wca_get_pure_params.restype = None

        self.s_ljs_wca_get_pure_params(byref(sigma_c),
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
        self.s_ljs_wca_set_pure_params.argtypes = [POINTER(c_double),
                                                   POINTER(c_double)]

        self.s_ljs_wca_set_pure_params.restype = None

        self.s_ljs_wca_set_pure_params(byref(sigma_c),
                                       byref(eps_depth_divk_c))


class ljs_wca(ljs_wca_base):
    """
    Interface to LJS-WCA
    """
    def __init__(self):
        """
        Initialize wca specific function pointers
        """
        # Load dll/so
        super(ljs_wca, self).__init__()

        # Options methods
        self.s_ljs_wca_model_control = getattr(self.tp, self.get_export_name("lj_splined", "ljs_wca_model_control"))

        # LJS-WCA specific methods
        self.s_calc_ai_reduced_ljs_wca = getattr(self.tp, self.get_export_name("lj_splined", "calc_ljs_wca_ai_tr"))


    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default"):
        """Initialize Lennard-Jomes splined model based on Weeks-Chandler-Anderson perturbation theory

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self._base_init(model="WCA",parameter_reference=parameter_reference)

    #################################
    # Model options
    #################################
    def model_control(self,
                      enable_cavity=True,
                      enable_hs=True,
                      enable_a1=True,
                      enable_a2=True,
                      enable_a3=True,
                      enable_a4=True):
        """Model control. Enable/disable model terms.

        Args:
            enable_cavity (bool): Enable/disable use of cavity term. Defaults to True.
            enable_hs (bool): Enable/disable hard-sphere term. Defaults to True.
            enable_a1 (bool): Enable/disable use of a1 dispersion term. Defaults to True.
            enable_a2 (bool): Enable/disable use of a2 dispersion term. Defaults to True.
            enable_a3 (bool): Enable/disable use of a3 dispersion term. Defaults to True.
            enable_a4 (bool): Enable/disable use of a3 dispersion term. Defaults to True.
        """
        self.activate()
        enable_cavity_c = c_int(enable_cavity)
        enable_hs_c = c_int(enable_hs)
        enable_a1_c = c_int(enable_a1)
        enable_a2_c = c_int(enable_a2)
        enable_a3_c = c_int(enable_a3)
        enable_a4_c = c_int(enable_a4)

        self.s_ljs_wca_model_control.argtypes = [POINTER(c_int),
                                                 POINTER(c_int),
                                                 POINTER(c_int),
                                                 POINTER(c_int),
                                                 POINTER(c_int),
                                                 POINTER(c_int)]

        self.s_ljs_wca_model_control.restype = None

        self.s_ljs_wca_model_control(byref(enable_cavity_c),
                                     byref(enable_hs_c),
                                     byref(enable_a1_c),
                                     byref(enable_a2_c),
                                     byref(enable_a3_c),
                                     byref(enable_a4_c))

    #################################
    # LJS-WCA specific methods
    #################################
    def get_pert_a(self,
                   T_star,
                   rho_star):
        """Get perturbation terms.

        Args:
            T_star (float): Reduced temperature.
            rho_star (float): Reduced density.

        Returns:
            a1 (float): a1 dispersion term divided by epsilon.
            a2 (float): a2 dispersion term divided by epsilon squared.
            a3 (float): a3 dispersion term divided by epsilon cubed.
            a4 (float): a4 dispersion term divided by epsilon to the forth power .
        """
        T_c = c_double(T_star)
        rho_star_c = c_double(rho_star)

        self.s_calc_ai_reduced_ljs_wca.argtypes = [POINTER(c_int),
                                                   POINTER(c_double),
                                                   POINTER(c_double),
                                                   POINTER(c_double)]

        self.s_calc_ai_reduced_ljs_wca.restype = None

        ai = []
        for i in range(4):
            i_c = c_int(i+1)
            ai_c = c_double(0.0)
            self.s_calc_ai_reduced_ljs_wca(byref(i_c),
                                           byref(T_c),
                                           byref(rho_star_c),
                                           byref(ai_c))
            ai.append(ai_c.value)
        return ai


class ljs_uv(ljs_wca_base):
    """
    Interface to LJS-UV
    """
    def __init__(self):
        """
        Initialize UV specific function pointers
        """
        # Load dll/so
        super(ljs_uv, self).__init__()

        # Options methods
        self.s_ljs_uv_model_control = getattr(self.tp, self.get_export_name("lj_splined", "ljs_uv_model_control"))


    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default"):
        """Initialize Lennard-Jomes splined model based on Weeks-Chandler-Anderson perturbation theory

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self._base_init(model="UV",parameter_reference=parameter_reference)

    #################################
    # Model options
    #################################
    def model_control(self,
                      use_temperature_dependent_u_fraction=False):
        """Model control. Enable/disable model terms.

        Args:
            use_temperature_dependent_u_fraction (bool): Enable/disable use of temperature dependent u-fraction.
        """
        self.activate()
        use_temperature_dependent_u_fraction_c = c_int(use_temperature_dependent_u_fraction)

        self.s_ljs_uv_model_control.argtypes = [POINTER(c_int)]

        self.s_ljs_uv_model_control.restype = None

        self.s_ljs_uv_model_control(byref(use_temperature_dependent_u_fraction_c))
