# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from .thermo import thermo, c_len_type


class multiparam(thermo):
    """
    Interface to multiparameter EOS
    """
    def __init__(self, comps=None, eos=None, reference_state="DEFAULT"):
        """Constructor
        Initialize multiparameter EOS

        Unless both parameters are specified, model must be initialized for specific components later by direct call
        to 'init'.  Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            eos (str, optional): Equation of state. (NIST_MEOS, MBWR32, MBWR19)
            reference_state (str): Reference state. ("DEFAULT", "IIR", "NBP", "ASHRAE", "IDGAS", "TRIPLE_POINT")
        """
        # Load dll/so
        super(multiparam, self).__init__()

        # Init methods
        self.eoslibinit_init_multiparameter = getattr(self.tp, self.get_export_name("eoslibinit", "init_multiparameter"))

        if None not in (comps, eos):
            self.init(comps, eos, reference_state)

    #################################
    # Init
    #################################

    def init(self, comps, eos, reference_state="DEFAULT"):
        """Constructor
        Initialize multiparameter EOS

        Args:
            comps (str): Comma separated list of component names
            eos (str): Equation of state. (NIST_MEOS, MBWR32, MBWR19, MEOS, GERG2008)
            reference_state (str): Reference state. ("DEFAULT", "IIR", "NBP", "ASHRAE", "IDGAS", "TRIPLE_POINT")
        """
        self.activate()
        eos_c = c_char_p(eos.encode('ascii'))
        eos_len = c_len_type(len(eos))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_state_c = c_char_p(reference_state.encode('ascii'))
        ref_state_len = c_len_type(len(reference_state))
        self.eoslibinit_init_multiparameter.argtypes = [c_char_p,
                                                        c_char_p,
                                                        c_char_p,
                                                        c_len_type,
                                                        c_len_type,
                                                        c_len_type]

        self.eoslibinit_init_multiparameter.restype = None

        self.eoslibinit_init_multiparameter(comp_string_c,
                                            eos_c,
                                            ref_state_c,
                                            comp_string_len,
                                            eos_len,
                                            ref_state_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))

        if "MEOS" == eos.upper():
            self.set_numerical_robustness_level(level=1)
