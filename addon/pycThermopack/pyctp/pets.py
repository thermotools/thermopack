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

class pets(thermo.thermopack):
    """
    Interface to PETS
    """
    def __init__(self):
        """
        Initialize pets specific function pointers
        """
        # Load dll/so
        super(pets, self).__init__()

        # Init methods
        self.eoslibinit_init_pets = getattr(self.tp, self.get_export_name("eoslibinit", "init_pets"))


    #################################
    # Init
    #################################

    def init(self, parameter_reference="Default"):
        """Initialize he PeTS equation of state for the LJ fluid
        truncated and shifted at 2.5 sigma. Reference:
        Heier et al. 2018 (10.1080/00268976.2018.1447153)

        Args:
            parameter_reference (str, optional): Wath parameters to use. Defaults to "Default".
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
