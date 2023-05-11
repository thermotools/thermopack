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
from .saft import saft

c_len_type = thermo.c_len_type

class ljs_bh(saft):
    """
    Interface to LJS-BH
    """
    def __init__(self, parameter_reference="Default", minimum_temperature=2.0):
        """
        Initialize cubic specific function pointers

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
            minimum_temperature (float, optional): Minimum temperature considered by numerical solvers. Default value 2.0
        """
        # Load dll/so
        saft.__init__(self)

        # Options methods
        self.s_ljs_bh_model_control = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_model_control"))

        # Init methods
        self.s_eoslibinit_init_ljs_bh = getattr(self.tp, self.get_export_name("eoslibinit", "init_ljs"))
        self.s_ljs_bh_get_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_get_pure_params"))
        self.s_ljs_bh_set_pure_params = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_set_pure_params"))

        # LJS-BH specific methods
        self.s_calc_ai_reduced_ljs_ex = getattr(self.tp, self.get_export_name("lj_splined", "calc_ai_reduced_ljs_ex"))
        self.s_ljs_bh_get_bh_diameter_div_sigma = getattr(self.tp, self.get_export_name("lj_splined", "ljs_bh_get_bh_diameter_div_sigma"))

        self.init(parameter_reference, minimum_temperature)

    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default", minimum_temperature=2.0):
        """Initialize Lennard-Jomes splined model based on Barker-Henderson perturbation theory

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
            minimum_temperature (float, optional): Minimum temperature considered by numerical solvers. Default value 2.0
        """
        self.activate()
        model = "BH"
        model_string_c = c_char_p(model.encode('ascii'))
        model_string_len = c_len_type(len(model))

        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_ljs_bh.argtypes = [c_char_p,
                                                  c_char_p,
                                                  c_len_type,
                                                  c_len_type]

        self.s_eoslibinit_init_ljs_bh.restype = None

        self.s_eoslibinit_init_ljs_bh(model_string_c,
                                      ref_string_c,
                                      model_string_len,
                                      ref_string_len)

        self.nc = 1
        self.set_tmin(minimum_temperature)

        # Map pure fluid parameters
        self.m = np.zeros(self.nc)
        self.sigma = np.zeros(self.nc)
        self.eps_div_kb = np.zeros(self.nc)
        self.m[0] = 1.0
        self.sigma[0], self.eps_div_kb[0] = self.get_sigma_eps()

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
            enable_a2 (bool): Enable/disable use of a2 dispersion term. Defaults to True.
            enable_a3 (bool): Enable/disable use of a3 dispersion term. Defaults to True.
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

    #################################
    # LJS-BH specific methods
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
            a3 (float): a3 dispersion term divided by epsilon cube.
        """
        T_c = c_double(T_star)
        rho_star_c = c_double(rho_star)
        a1_c = c_double(0.0)
        a2_c = c_double(0.0)
        a3_c = c_double(0.0)


        self.s_calc_ai_reduced_ljs_ex.argtypes = [POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double)]

        self.s_calc_ai_reduced_ljs_ex.restype = None

        self.s_calc_ai_reduced_ljs_ex(byref(T_c),
                                      byref(rho_star_c),
                                      byref(a1_c),
                                      byref(a2_c),
                                      byref(a3_c))

        return a1_c.value, a2_c.value, a3_c.value


    def get_bh_diameter_div_sigma(self, T_star):
        """Get Barker-Henderson diameter.

        Args:
            T_star (float): Reduced temperature (-).

        Returns:
            d_bh (float): Barker-Henderson diameter divided by sigma (-).
        """
        T_star_c = c_double(T_star)
        d_bh_c = c_double(0.0)


        self.s_ljs_bh_get_bh_diameter_div_sigma.argtypes = [POINTER(c_double),
                                                            POINTER(c_double)]

        self.s_ljs_bh_get_bh_diameter_div_sigma.restype = None

        self.s_ljs_bh_get_bh_diameter_div_sigma(byref(T_star_c),
                                                byref(d_bh_c))

        return d_bh_c.value
