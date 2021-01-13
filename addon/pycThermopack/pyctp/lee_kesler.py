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

class lee_kesler(thermo.thermopack):
    """
    Interface to Lee-Kesler model
    """
    def __init__(self):
        """
        Initialize Lee-Kesler specific function pointers
        """
        # Load dll/so
        super(lee_kesler, self).__init__()

        # Init methods
        self.eoslibinit_init_lee_kesler = getattr(self.tp, self.get_export_name("eoslibinit", "init_lee_kesler"))

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """ Init Lee-Kesler

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Identefier for parameters set. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_lee_kesler.argtypes = [c_char_p,
                                                    c_char_p,
                                                    c_len_type,
                                                    c_len_type]

        self.eoslibinit_init_lee_kesler.restype = None

        self.eoslibinit_init_lee_kesler(comp_string_c,
                                        ref_string_c,
                                        comp_string_len,
                                        ref_string_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
