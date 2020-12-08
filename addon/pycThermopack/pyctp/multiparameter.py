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

class multiparam(thermo.thermopack):
    """
    Interface to multiparameter EOS
    """
    def __init__(self):
        """
        Initialize multiparameter EOS specific function pointers
        """
        # Load dll/so
        super(multiparam, self).__init__()

        # Init methods
        self.eoslibinit_init_multiparameter = getattr(self.tp, self.get_export_name("eoslibinit", "init_multiparameter"))

    #################################
    # Init
    #################################

    def init(self, comps, eos):
        """Initialize multiparameter EOS

        Args:
            comps (str): Comma separated list of component names
            eos (str): Equation of state. (NIST_MEOS, MBWR32, MBWR19)
        """
        self.activate()
        eos_c = c_char_p(eos.encode('ascii'))
        eos_len = c_len_type(len(eos))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))

        self.eoslibinit_init_multiparameter.argtypes = [c_char_p,
                                                        c_char_p,
                                                        c_len_type,
                                                        c_len_type]

        self.eoslibinit_init_multiparameter.restype = None

        self.eoslibinit_init_multiparameter(comp_string_c,
                                            eos_c,
                                            comp_string_len,
                                            eos_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
