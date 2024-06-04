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

class pets(saft):
    """
    Interface to PETS
    """
    def __init__(self, parameter_reference="Default", minimum_temperature=2.0):
        """Constructor
        Initialize pets specific function pointers

        Args:
            parameter_reference (str, optional): What parameters to use. Defaults to "Default".
            minimum_temperature (float, optional): Minimum temperature considered by numerical solvers. Default value 2.0
        """
        # Load dll/so
        saft.__init__(self)

        # Init methods
        self.eoslibinit_init_pets = getattr(self.tp, self.get_export_name("eoslibinit", "init_pets"))

        self.s_get_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pets_get_pure_params"))
        self.s_set_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pets_get_pure_params"))

        self.init(parameter_reference, minimum_temperature)


    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default", minimum_temperature=2.0):
        """Constructor
        Initialize he PeTS equation of state for the LJ fluid
        truncated and shifted at 2.5 sigma. Reference:
        Heier et al. 2018 (10.1080/00268976.2018.1447153)

        Args:
            parameter_reference (str, optional): What parameters to use. Defaults to "Default".
            minimum_temperature (float, optional): Minimum temperature considered by numerical solvers. Default value 2.0
        """
        self.activate()
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_pets.argtypes = [c_char_p,
                                              c_len_type]

        self.eoslibinit_init_pets.restype = None

        self.eoslibinit_init_pets(ref_string_c,
                                  ref_string_len)

        self.nc = 1
        self.set_tmin(minimum_temperature)

        # Map pure fluid parameters
        self.m = np.ones(self.nc)
        self.sigma = np.zeros(self.nc)
        self.eps_div_kb = np.zeros(self.nc)
        self.sigma[0], self.eps_div_kb[0] = self.get_pure_params()

    def set_pure_params(self, c, sigma, eps_div_kb):
        """Utility
        Set pure fluid PeTS parameters

        Args:
            c (int) : Component index
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
        """
        self.sigma[c-1] = sigma
        self.eps_div_kb[c-1] = eps_div_kb

        self.activate()
        c_c = c_int(1)
        param_c = (c_double * 2)(sigma, eps_div_kb)

        self.s_set_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_set_pure_params.restype = None

        self.s_set_pure_params(byref(c_c),
                               param_c)

    def get_pure_params(self):
        """Utility
        Get pure fluid PeTS parameters

        Returns:
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
        """
        self.activate()
        c_c = c_int(1)
        param_c = (c_double * 2)(0.0)
        self.s_get_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_get_pure_params.restype = None

        self.s_get_pure_params(byref(c_c),
                               param_c)
        sigma, eps_div_kb = param_c
        return sigma, eps_div_kb
