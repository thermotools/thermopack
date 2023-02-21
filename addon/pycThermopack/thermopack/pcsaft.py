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
from . import thermo, saft

c_len_type = thermo.c_len_type


class pcsaft(saft.saft):
    """
    Interface to PC-SAFT model
    """
    def __init__(self, comps=None, parameter_reference="Default", simplified=False, polar=False):
        """Initialize PC-SAFT model in thermopack

        If no components are specified, model must be initialized for specific components later by direct call to 'init'.
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
            simplified (bool): Use simplified PC-SAFT (sPC-SAFT: 10.1021/ie020753p) (Default False)
            polar (bool): Use dipole and quadrupole contributions PCP-SAFT (10.1002/aic.10502, 10.1002/aic.10683 and 10.1021/jp072619u) (Default True)
        """
        # Load dll/so
        saft.saft.__init__(self)

        # Init methods
        self.eoslibinit_init_pcsaft = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_pcsaft"))
        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name(
            "saft_interface", "pc_saft_get_kij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name(
            "saft_interface", "pc_saft_set_kij_asym"))
        # SAFT specific methods
        self.s_get_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_get_pure_params"))
        self.s_set_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_set_pure_params"))
        self.s_lng_ii_pc_saft_tvn = getattr(self.tp, self.get_export_name("pc_saft_nonassoc", "lng_ii_pc_saft_tvn"))

        # Define parameters to be set by init
        self.nc = None

        if comps is not None:
            self.init(comps, parameter_reference=parameter_reference, simplified=simplified, polar=polar)

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default", simplified=False, polar=False):
        """Initialize PC-SAFT model in thermopack

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
            simplified (bool): Use simplified PC-SAFT (sPC-SAFT: 10.1021/ie020753p) (Default False)
            polar (bool): Use dipole and quadrupole contributions PCP-SAFT (10.1002/aic.10502, 10.1002/aic.10683 and 10.1021/jp072619u) (Default True)
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))
        simplified_c = c_int(1 if simplified else 0)
        polar_c = c_int(1 if polar else 0)

        self.eoslibinit_init_pcsaft.argtypes = [c_char_p,
                                                c_char_p,
                                                POINTER( c_int ),
                                                POINTER( c_int ),
                                                c_len_type,
                                                c_len_type]

        self.eoslibinit_init_pcsaft.restype = None

        self.eoslibinit_init_pcsaft(comp_string_c,
                                    ref_string_c,
                                    byref( simplified_c ),
                                    byref( polar_c ),
                                    comp_string_len,
                                    ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))

        # Map pure fluid parameters
        self.m = np.zeros(self.nc)
        self.sigma = np.zeros(self.nc)
        self.eps_div_kb = np.zeros(self.nc)
        for i in range(self.nc):
            self.m[i], self.sigma[i], self.eps_div_kb[i], eps, beta = \
                self.get_pure_params(i+1)


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


    def set_pure_params(self, c, m, sigma, eps_div_kb, eps=0.0, beta=0.0):
        """Set pure fluid PC-SAFT parameters

        Args:
            c (int): Component index (FORTRAN)
            m (float): Mean number of segments
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
            eps (float): Association energy (J/mol)
            beta (float): Association volume (-)
        """
        self.m[c-1] = m
        self.sigma[c-1] = sigma
        self.eps_div_kb[c-1] = eps_div_kb

        self.activate()
        c_c = c_int(c)
        param_c = (c_double * 5)(m, sigma, eps_div_kb, eps, beta)
        self.s_set_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_set_pure_params.restype = None

        self.s_set_pure_params(byref(c_c),
                               param_c)

    def get_pure_params(self, c):
        """Get pure fluid PC-SAFT parameters

        Args:
            c (int): Component index (FORTRAN)
        Returns:
            m (float): Mean number of segments
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
            eps (float): Association energy (J/mol)
            beta (float): Association volume (-)
        """
        self.activate()
        c_c = c_int(c)
        param_c = (c_double * 5)(0.0)
        self.s_get_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_get_pure_params.restype = None

        self.s_get_pure_params(byref(c_c),
                               param_c)
        m, sigma, eps_div_kb, eps, beta = param_c
        return m, sigma, eps_div_kb, eps, beta


    def lng_ii(self, temp, volume, n, i, lng_t=None, lng_v=None, lng_n=None, lng_tt=None, lng_vv=None,
               lng_tv=None, lng_tn=None, lng_vn=None, lng_nn=None):
        """Calculate logarithm og g at contact gitvne temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            i (int): FORTRAN component index
            lng_t (No type, optional): Flag to activate calculation. Defaults to None.
            lng_v (No type, optional): Flag to activate calculation. Defaults to None.
            lng_n (No type, optional): Flag to activate calculation. Defaults to None.
            lng_tt (No type, optional): Flag to activate calculation. Defaults to None.
            lng_vv (No type, optional): Flag to activate calculation. Defaults to None.
            lng_tv (No type, optional): Flag to activate calculation. Defaults to None.
            lng_tn (No type, optional): Flag to activate calculation. Defaults to None.
            lng_vn (No type, optional): Flag to activate calculation. Defaults to None.
            lng_nn (No type, optional): Flag to activate calculation. Defaults to None.

        Returns:
            ndarry:
            Optionally differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        lng_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)
        i_c = c_int(i)

        null_pointer = POINTER(c_double)()
        lng_t_c = null_pointer if lng_t is None else c_double(0.0)
        lng_v_c = null_pointer if lng_v is None else c_double(0.0)
        lng_n_c = null_pointer if lng_n is None else (c_double * len(n))(0.0)
        lng_tt_c = null_pointer if lng_tt is None else c_double(0.0)
        lng_vv_c = null_pointer if lng_vv is None else c_double(0.0)
        lng_tv_c = null_pointer if lng_tv is None else c_double(0.0)
        lng_tn_c = null_pointer if lng_tn is None else (c_double * len(n))(0.0)
        lng_vn_c = null_pointer if lng_vn is None else (c_double * len(n))(0.0)
        lng_nn_c = null_pointer if lng_nn is None else (c_double * len(n)**2)(0.0)

        self.s_lng_ii_pc_saft_tvn.argtypes = [POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_int),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double)]

        self.s_lng_ii_pc_saft_tvn.restype = None

        self.s_lng_ii_pc_saft_tvn(byref(temp_c),
                                  byref(v_c),
                                  n_c,
                                  byref(i_c),
                                  byref(lng_c),
                                  lng_t_c,
                                  lng_v_c,
                                  lng_n_c,
                                  lng_tt_c,
                                  lng_tv_c,
                                  lng_tn_c,
                                  lng_vv_c,
                                  lng_vn_c,
                                  lng_nn_c)

        return_tuple = (lng_c.value, )
        if not lng_t is None:
            return_tuple += (lng_t_c.value, )
        if not lng_v is None:
            return_tuple += (lng_v_c.value, )
        if not lng_n is None:
            return_tuple += (np.array(lng_n_c), )
        if not lng_tt is None:
            return_tuple += (lng_tt_c.value, )
        if not lng_tv is None:
            return_tuple += (lng_tv_c.value, )
        if not lng_vv is None:
            return_tuple += (lng_vv_c.value, )
        if not lng_tn is None:
            return_tuple += (np.array(lng_tn_c), )
        if not lng_vn is None:
            return_tuple += (np.array(lng_vn_c), )
        if not lng_nn is None:
            lng_nn = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    lng_nn[i][j] = lng_nn_c[i + j*len(n)]
            return_tuple += (lng_nn, )

        return return_tuple
