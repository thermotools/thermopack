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

class pcsaft(thermo.thermopack):
    """
    Interface to cubic
    """
    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(pcsaft, self).__init__()

        # Init methods
        self.eoslibinit_init_pcsaft = getattr(self.tp, self.get_export_name("eoslibinit", "init_pcsaft"))
        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_get_kij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_set_kij_asym"))

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Initialize PC-SAFT model in thermopack

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_pcsaft.argtypes = [c_char_p,
                                                c_char_p,
                                                c_len_type,
                                                c_len_type]

        self.eoslibinit_init_pcsaft.restype = None

        self.eoslibinit_init_pcsaft(comp_string_c,
                                    ref_string_c,
                                    comp_string_len,
                                    ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))

    def get_kij(self, c1, c2):
        """Get binary well depth interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            kij (float): Well depth interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = c_double(0.0)
        self.s_get_kij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_get_kij.restype = None

        self.s_get_kij(byref(c1_c),
                       byref(c2_c),
                       byref(kij_c))
        return kij_c.value

    def set_kij(self, c1, c2, kij):
        """Set binary well depth interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two
            kij (float): Well depth interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = c_double(kij)
        self.s_set_kij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_set_kij.restype = None

        self.s_set_kij(byref(c1_c),
                       byref(c2_c),
                       byref(kij_c))
