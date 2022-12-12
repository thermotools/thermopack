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

class multiparam(thermo.thermo):
    """
    Interface to multiparameter EOS
    """
    def __init__(self, comps=None, eos=None):
        """Initialize multiparameter EOS

        Unless both parameters are specified, model must be initialized for specific components later by direct call
        to 'init'.  Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            eos (str, optional): Equation of state. (NIST_MEOS, MBWR32, MBWR19)
        """
        # Load dll/so
        super(multiparam, self).__init__()

        # Init methods
        self.eoslibinit_init_multiparameter = getattr(self.tp, self.get_export_name("eoslibinit", "init_multiparameter"))

        if None not in (comps, eos):
            self.init(comps, eos)

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
