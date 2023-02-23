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

class qcubic(cubic.cubic):
    """
    Interface to cubic
    """
    def __init__(self, comps=None, mixing="vdW", minimum_temperature=2.0):
        """Initialize Quantum Cubic Peng-Robinson equation of state by Aasen et al.
        (10.1016/j.fluid.2020.112790)

        If no components are specified, model must be initialized for specific components later by direct call to 'init'
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            mixing (str, optional): Mixture model. Defaults to "vdW".
        """
        # Load dll/so
        super(qcubic, self).__init__()

        # Init methods
        self.eoslibinit_init_quantum_cubic = getattr(self.tp, self.get_export_name("eoslibinit", "init_quantum_cubic"))
        if comps is not None:
            self.init(comps, mixing=mixing, minimum_temperature=minimum_temperature)


    #################################
    # Init
    #################################

    def init(self, comps, mixing="vdW", minimum_temperature=2.0):
        """Initialize Quantum Cubic Peng-Robinson equation of state by Aasen et al.
        (10.1016/j.fluid.2020.112790)

        Args:
            comps (str): Comma separated list of component names
            mixing (str, optional): Mixture model. Defaults to "vdW".
        """
        self.activate()
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_len_type(len(mixing))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))

        self.eoslibinit_init_quantum_cubic.argtypes = [c_char_p,
                                                       c_char_p,
                                                       c_len_type,
                                                       c_len_type]

        self.eoslibinit_init_quantum_cubic.restype = None

        self.eoslibinit_init_quantum_cubic(comp_string_c,
                                           mixing_c,
                                           comp_string_len,
                                           mixing_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))

        self.set_tmin(minimum_temperature)
