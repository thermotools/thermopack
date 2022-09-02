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
    Interface to cubic plus association model
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
        # Options methods
        self.s_use_simplified_cpa = getattr(self.tp, self.get_export_name("saft_interface", "setcpaformulation"))


    #################################
    # Init
    #################################

    def init(self, comps, eos="SRK", mixing="vdW", alpha="Classic",
             parameter_reference="Default"):
        """Initialize cubic plus association model in thermopack

        Args:
            comps (str): Comma separated list of component names
            eos (str, optional): Cubic equation of state. Defaults to "SRK".
            mixing (str, optional): Mixture model. Defaults to "vdW".
            alpha (str, optional): Alpha model. Defaults to "Classic".
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
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

    def get_kij(self, c1, c2):
        """Get attractive energy interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            kij (array_like): i-j interaction parameter (2 parameters)
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = (c_double * 2)(0.0)
        self.s_get_kij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_get_kij.restype = None

        self.s_get_kij(byref(c1_c),
                       byref(c2_c),
                       kij_c)

        return np.array(kij_c)

    def set_kij(self, c1, c2, kij):
        """Set attractive energy interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two
            kij (array_like): i-j interaction parameter (2 parameters)
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_c = (c_double * 2)(*kij)
        self.s_set_kij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_set_kij.restype = None

        self.s_set_kij(byref(c1_c),
                       byref(c2_c),
                       kij_c)

    def use_simplified_cpa(self, simplified):
        """Use simplified form for rdf in CPA
        Args:
            simplified (bool): True if simplified
        """
        simplified_c = c_bool(simplified)
        self.s_use_simplified_cpa.argtypes = [POINTER(c_bool)]
        self.s_use_simplified_cpa.restype = None
        self.s_use_simplified_cpa(byref(simplified_c))