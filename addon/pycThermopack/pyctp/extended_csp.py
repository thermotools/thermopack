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

class ext_csp(thermo.thermopack):
    """
    Interface to tc-PR
    """
    def __init__(self):
        """
        Initialize extended corredsponding state model
        """
        # Load dll/so
        super(ext_csp, self).__init__()

        # Init methods
        self.eoslibinit_init_extcsp = getattr(self.tp, self.get_export_name("eoslibinit", "init_extcsp"))


    #################################
    # Init
    #################################

    def init(self,
             comps,
             sh_eos,
             sh_alpha,
             sh_mixing,
             ref_eos,
             ref_comp,
             ref_alpha="Classic",
             parameter_reference="Default"):
        """Initialize extended corredsponding state model model.

        Args:
            comps (str): Comma separated list of component names
            sh_eos (str): Shape factor equation of state
            sh_alpha (str): Shape factor alpha
            sh_mixing (str): Shape factor mixing rules
            ref_eos (str): Reference equation of state
            ref_comp (str): Reference component
            ref_alpha (str): Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos. Defaults to "Classic"
            parameter_reference (str, optional): Identefier for parameters set. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        sh_eos_c = c_char_p(sh_eos.encode('ascii'))
        sh_eos_len = c_len_type(len(sh_eos))
        sh_alpha_c = c_char_p(sh_alpha.encode('ascii'))
        sh_alpha_len = c_len_type(len(sh_alpha))
        sh_mixing_c = c_char_p(sh_mixing.encode('ascii'))
        sh_mixing_len = c_len_type(len(sh_mixing))
        ref_eos_c = c_char_p(ref_eos.encode('ascii'))
        ref_eos_len = c_len_type(len(ref_eos))
        ref_comp_c = c_char_p(ref_comp.encode('ascii'))
        ref_comp_len = c_len_type(len(ref_comp))
        ref_alpha_c = c_char_p(ref_alpha.encode('ascii'))
        ref_alpha_len = c_len_type(len(ref_alpha))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_extcsp.argtypes = [c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type]

        self.eoslibinit_init_extcsp.restype = None

        self.eoslibinit_init_extcsp(comp_string_c,
                                    sh_eos_c,
                                    sh_alpha_c,
                                    sh_mixing_c,
                                    ref_eos_c,
                                    ref_comp_c,
                                    ref_alpha_c,
                                    ref_string_c,
                                    comp_string_len,
                                    sh_eos_len,
                                    sh_alpha_len,
                                    sh_mixing_len,
                                    ref_eos_len,
                                    ref_comp_len,
                                    ref_alpha_len,
                                    ref_string_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
