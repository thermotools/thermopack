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
from .thermo import c_len_type
from .cubic import cubic


class tcPR(cubic):
    """
    Interface to tc-PR
    """
    def __init__(self, comps=None, mixing="vdW", parameter_reference=None):
        """Constructor
        Initialize tc-PR model. Translated and consistent cubic EoS by le Guennec et al.
        (10.1016/j.fluid.2016.09.003)

        If no components are specified, model must be initialized for specific components later by direct call to 'init'
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            mixing (str, optional): Mixture model. Defaults to "vdW".
            parameter_ref (str, optional): Parameter reference additional to "tcPR".

        """
        # Load dll/so
        super(tcPR, self).__init__()

        # Init methods
        self.eoslibinit_init_tcpr = getattr(self.tp, self.get_export_name("eoslibinit", "init_tcpr"))
        if comps is not None:
            self.init(comps, mixing=mixing, parameter_reference=parameter_reference)


    #################################
    # Init
    #################################

    def init(self, comps, mixing="vdW", parameter_reference=None):
        """Constructor
        Initialize tc-PR model. Translated and consistent cubic EoS by le Guennec et al.
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
