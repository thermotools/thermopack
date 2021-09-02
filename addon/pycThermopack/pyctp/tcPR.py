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
# Import thermo
from . import cubic

c_len_type = thermo.c_len_type

class tcPR(cubic.cubic):
    """
    Interface to tc-PR
    """
    def __init__(self):
        """
        Initialize tcPR specific function pointers
        """
        # Load dll/so
        super(tcPR, self).__init__()

        # Init methods
        self.eoslibinit_init_tcpr = getattr(self.tp, self.get_export_name("eoslibinit", "init_tcpr"))


    #################################
    # Init
    #################################

    def init(self, comps, mixing="vdW", parameter_reference=None):
        """Initialize tc-PR model. Translated and consistent cubic EoS by le Guennec et al.
        (10.1016/j.fluid.2016.09.003)

        Args:
            comps (str): Comma separated list of component names
            mixing (str, optional): Mixture model. Defaults to "vdW".
            parameter_ref (str, optional): Parameter reference additional to "tcPR".
        """
        self.activate()
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_len_type(len(mixing))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        if parameter_reference is None:
            parameter_reference_c = c_char_p()
            parameter_reference_len = c_len_type(0)
        else:
            parameter_reference_c = c_char_p(parameter_reference.encode('ascii'))
            parameter_reference_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_tcpr.argtypes = [c_char_p,
                                              c_char_p,
                                              c_char_p,
                                              c_len_type,
                                              c_len_type,
                                              c_len_type]

        self.eoslibinit_init_tcpr.restype = None

        self.eoslibinit_init_tcpr(comp_string_c,
                                  mixing_c,
                                  parameter_reference_c,
                                  comp_string_len,
                                  mixing_len,
                                  parameter_reference_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
