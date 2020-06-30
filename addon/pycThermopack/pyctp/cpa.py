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
from . import cubic

c_len_type = thermo.c_len_type

class cpa(cubic.cubic):
    """
    Interface to cubic
    """
    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(cpa, self).__init__()

        # Init methods
        self.eoslibinit_init_cpa = getattr(self.tp, self.get_export_name("eoslibinit", "init_cpa"))
        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name("saft_interface", "cpa_get_kij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name("saft_interface", "cpa_set_kij"))


    #################################
    # Init
    #################################

    def init(self, comps, eos="SRK", mixing="vdW", alpha="Classic",
             parameter_reference="Default"):
        """
        Initialize cubic plus association model in thermopack
        """
        eos_c = c_char_p(eos.encode('ascii'))
        eos_len = c_len_type(len(eos))
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_len_type(len(mixing))
        alpha_c = c_char_p(alpha.encode('ascii'))
        alpha_len = c_len_type(len(alpha))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_cpa.argtypes = [c_char_p,
                                             c_char_p,
                                             c_char_p,
                                             c_char_p,
                                             c_char_p,
                                             c_len_type,
                                             c_len_type,
                                             c_len_type,
                                             c_len_type,
                                             c_len_type]

        self.eoslibinit_init_cpa.restype = None

        self.eoslibinit_init_cpa(comp_string_c,
                                 eos_c,
                                 mixing_c,
                                 alpha_c,
                                 ref_string_c,
                                 comp_string_len,
                                 eos_len,
                                 mixing_len,
                                 alpha_len,
                                 ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))
