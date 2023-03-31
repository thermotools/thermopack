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
        Initialize UV-Mie theory EoS
        Args:
            comps (str): Comma separated list of component names
            model (str, optional): "BH" (default), "WCA"
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        thermo.thermo.__init__(self)

        # Init methods
        self.s_eoslibinit_init_uv = getattr(self.tp, self.get_export_name("eoslibinit", "init_uv"))
        self.s_set_sutsum_params = getattr(self.tp, self.get_export_name(
            "uv_theory", "reset_sutsum_external_ij"))

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


    #def set_params(self, ic, m, sigma, eps_div_k, lambda_a, lambda_r):
    def set_params(self,ic,jc,nt,c_vec,lam_vec,sigma,epsdivk,beta_expo=None):
        # """Set pure fluid parameters

        # Args:
        #     ic (int): Component index
        #     m (float): Mean number of segments.
        #     sigma (float): Temperature-independent segment diameter [m].
        #     eps_div_k (float): Well depth divided by Boltzmann's const. [K].
        #     lambda_a (float): Attractive exponent of the Mie potential
        #     lambda_r (float): Repulsive exponent of the Mie potential
        # """
        self.activate()

        ic_c = c_int(ic)
        jc_c = c_int(jc)
        nt_c = c_int(nt)

        cvec_c   = (c_double * nt)(*c_vec)
        lamvec_c = (c_double * nt)(*lam_vec)

        sigma_c = c_double(sigma)
        eps_c = c_double(epsdivk)

        null_pointer = POINTER(c_int)()
        if beta_expo is None:
            bexp_c = null_pointer
        else:
            bexp_c = (c_int * nt)(*beta_expo)

        self.s_set_sutsum_params.argtypes = [POINTER(c_int),
                                             POINTER(c_int),
                                             POINTER(c_int),
                                             POINTER(c_double),
                                             POINTER(c_double),
                                             POINTER(c_double),
                                             POINTER(c_double),
                                             POINTER(c_int)]

        self.s_set_sutsum_params.restype = None

        self.s_set_sutsum_params(byref(ic_c),
                                 byref(jc_c),
                                 byref(nt_c),
                                 cvec_c,
                                 lamvec_c,
                                 byref(sigma_c),
                                 byref(eps_c),
                                 bexp_c)
