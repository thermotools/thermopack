from ctypes import *
import numpy as np
from . import thermo

c_len_type = thermo.c_len_type


class uv_theory(thermo.thermo):
    """
    Interface to UV-theory
    """
    def __init__(self, comps=None, model="BH", parameter_reference="Default"):
        """
        Initialize UV-Mie therory EoS
        Args:
            comps (str): Comma separated list of component names
            model (str, optional): "BH" (default), "WCA"
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        thermo.thermo.__init__(self)

        # Init methods
        self.s_eoslibinit_init_uv = getattr(self.tp, self.get_export_name("eoslibinit", "init_uv"))


        if comps is not None:
            self.init(comps, model, parameter_reference)


    #################################
    # Init
    #################################

    def init(self, comps, model="BH", parameter_reference="Default"):
        """Initialize UV theory for Mie fluids

        Args:
            comps (str): Comma separated list of component names
            model (str, optional): "BH" (default), "WCA"
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        model_string_c = c_char_p(model.encode('ascii'))
        model_string_len = c_len_type(len(model))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.s_eoslibinit_init_uv.argtypes = [c_char_p,
                                              c_char_p,
                                              c_char_p,
                                              c_len_type,
                                              c_len_type,
                                              c_len_type]

        self.s_eoslibinit_init_uv.restype = None

        self.s_eoslibinit_init_uv(comp_string_c,
                                  model_string_c,
                                  ref_string_c,
                                  comp_string_len,
                                  model_string_len,
                                  ref_string_len)

        self.nc = max(len(comps.split(" ")),len(comps.split(",")))
