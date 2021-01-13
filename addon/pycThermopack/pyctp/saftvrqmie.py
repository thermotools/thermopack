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
# Import saftvrmie
from . import saftvrmie

c_len_type = thermo.c_len_type

class saftvrqmie(saftvrmie.saftvrmie):
    """
    Interface to SAFT-VRQ Mie
    """
    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(saftvrqmie, self).__init__()

        # Init methods
        self.s_eoslibinit_init_quantum_saftvrmie = getattr(self.tp, self.get_export_name("eoslibinit", "init_quantum_saftvrmie"))

    #################################
    # Init
    #################################

    def init(self, comps, feynman_hibbs_order=1, parameter_reference="Default"):
        """Initialize SAFT-VRQ Mie model in thermopack

        Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium
        (doi.org/10.1063/1.5111364
        Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium
        (doi.org/10.1063/1.5136079)

        Args:
            comps (str): Comma separated list of component names
            feynman_hibbs_order (int): Order of Feynman-Hibbs quantum corrections (1 or 2 supported). Defaults to 1.
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        fh_c = c_int(feynman_hibbs_order)
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_quantum_saftvrmie.argtypes = [c_char_p,
                                                             POINTER(c_int),
                                                             c_char_p,
                                                             c_len_type,
                                                             c_len_type]

        self.s_eoslibinit_init_quantum_saftvrmie.restype = None

        self.s_eoslibinit_init_quantum_saftvrmie(comp_string_c,
                                                 byref(fh_c),
                                                 ref_string_c,
                                                 comp_string_len,
                                                 ref_string_len)
        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
