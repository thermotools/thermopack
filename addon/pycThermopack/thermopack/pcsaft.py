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
from .saft import saft
from . import utils
import warnings


class pcsaft(saft):
    """
    Interface to PC-SAFT model
    """
    def __init__(self, comps=None, parameter_reference="Default", simplified=False, polar=False):
        """Constructor
        Initialize PC-SAFT model in thermopack
        If no components are specified, model must be initialized for specific components later by direct call to 'init'.
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
            simplified (bool): Use simplified PC-SAFT (sPC-SAFT: 10.1021/ie020753p) (Default False)
            polar (bool): Use dipole and quadrupole contributions PCP-SAFT (10.1002/aic.10502, 10.1002/aic.10683 and 10.1021/jp072619u) (Default False)
        """
        # Load dll/so
        saft.__init__(self)

        # Init methods
        self.eoslibinit_init_pcsaft = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_pcsaft"))
        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name(
            "saft_interface", "pc_saft_get_kij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name(
            "saft_interface", "pc_saft_set_kij_asym"))
        self.s_get_ci = getattr(self.tp, self.get_export_name("", "thermopack_get_volume_shift_parameters"))
        self.s_set_ci = getattr(self.tp, self.get_export_name("", "thermopack_set_volume_shift_parameters"))

        # SAFT specific methods
        self.s_get_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_get_pure_params"))
        self.s_set_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "pc_saft_set_pure_params"))
        self.s_lng_ii_pc_saft_tvn = getattr(self.tp, self.get_export_name("pc_saft_nonassoc", "lng_ii_pc_saft_tvn"))
        # DFT interface
        self.s_calc_assoc_phi = getattr(self.tp, self.get_export_name("saft_interface", "calc_assoc_phi"))

        # Define parameters to be set by init
        self.nc = None
        self.m = None
        self.sigma = None
        self.eps_div_kb = None

        if comps is not None:
            self.init(comps, parameter_reference=parameter_reference, simplified=simplified, polar=polar)

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default", simplified=False, polar=False):
        """Constructor
        Initialize PC-SAFT model in thermopack

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
            self.m[i], self.sigma[i], self.eps_div_kb[i], eps, beta = self.get_pure_fluid_param(i+1)

    def get_ci(self, cidx):
        """Utility
        Get volume correction parameters

        Args:
            cidx (int): Component index

        Returns:
            ciA (float): Volume shift param of component cidx (m3/mol)
            ciB (float): Volume shift param of component cidx (m3/mol/K)
            ciC (float): Volume shift param of component cidx (m3/mol/K^2)
            ci_type (int): Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3)
        """
        cidx_c = c_int(cidx)
        ciA_c = c_double(0.0)
        ciB_c = c_double(0.0)
        ciC_c = c_double(0.0)
        ciD_c = c_double(0.0)
        ciE_c = c_double(0.0)
        ciF_c = c_double(0.0)
        ci_type_c = c_int(0)
        self.s_get_ci.argtypes = [POINTER(c_int),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_int)]

        self.s_get_ci.restype = None

        self.s_get_ci(byref(cidx_c),
                      byref(ciA_c),
                      byref(ciB_c),
                      byref(ciC_c),
                      byref(ciD_c),
                      byref(ciE_c),
                      byref(ciF_c),
                      byref(ci_type_c))

        return ciA_c.value, ciB_c.value, ciC_c.value, ciD_c.value, ciE_c.value, ciF_c.value, ci_type_c.value

    def set_ci(self, cidx, ciA, ciB=0.0, ciC=0.0, ciD=0.0, ciE=0.0, ciF=0.0, ci_type=1):
        """Utility
        Set volume correction parametrs

        Args:
            cidx (int): Component index
            ciA (float): Volume shift param of component cidx (m3/mol)
            ciB (float): Volume shift param of component cidx (m3/mol/K)
            ciC (float): Volume shift param of component cidx (m3/mol/K^2)
            ci_type (int): Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3, QUINTIC=6)
        """
        cidx_c = c_int(cidx)
        ciA_c = c_double(ciA)
        ciB_c = c_double(ciB)
        ciC_c = c_double(ciC)
        ciD_c = c_double(ciD)
        ciE_c = c_double(ciE)
        ciF_c = c_double(ciF)
        ci_type_c = c_int(ci_type)
        self.s_set_ci.argtypes = [POINTER(c_int),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_double),
                                  POINTER(c_int)]

        self.s_set_ci.restype = None

        self.s_set_ci(byref(cidx_c),
                      byref(ciA_c),
                      byref(ciB_c),
                      byref(ciC_c),
                      byref(ciD_c),
                      byref(ciE_c),
                      byref(ciF_c),
                      byref(ci_type_c))



    def get_kij(self, c1, c2):
        """Utility
        Get binary well depth interaction parameter

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
        """Utility
        Set binary well depth interaction parameter

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


    def set_pure_fluid_param(self, c, m, sigma, eps_div_kb, eps=0.0, beta=0.0):
        """Utility
        Set pure fluid PC-SAFT parameters

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

    def set_pure_params(self, c, m, sigma, eps_div_kb, eps=0.0, beta=0.0):
        """Deprecated
        Set pure fluid PC-SAFT parameters

        Args:
            c (int): Component index (FORTRAN)
            m (float): Mean number of segments
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
            eps (float): Association energy (J/mol)
            beta (float): Association volume (-)
        """
        warnings.warn("The method 'set_pure_params' has been repaced by 'set_pure_fluid_param', and may be removed in"
                      "the future.", DeprecationWarning, stacklevel=2)
        self.set_pure_fluid_param(c, m, sigma, eps_div_kb, eps=eps, beta=beta)

    def get_pure_fluid_param(self, c):
        """Utility
        Get pure fluid PC-SAFT parameters

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

    def get_pure_params(self, c):
        """Deprecated
        Get pure fluid PC-SAFT parameters

        Args:
            c (int): Component index (FORTRAN)
        Returns:
            m (float): Mean number of segments
            sigma (float): Segment diameter (m)
            eps_div_kb (float): Well depth divided by Boltzmann's constant (K)
            eps (float): Association energy (J/mol)
            beta (float): Association volume (-)
        """
        warnings.warn("The method 'get_pure_params' has been repaced by 'get_pure_fluid_param', and may be removed in"
                      "the future.", DeprecationWarning, stacklevel=2)
        return self.get_pure_fluid_param(c)

    def lng_ii(self, temp, volume, n, i, lng_t=None, lng_v=None, lng_n=None, lng_tt=None, lng_vv=None,
               lng_tv=None, lng_tn=None, lng_vn=None, lng_nn=None):
        """Utility
        Calculate logarithm of the radial distribution function at contact given temperature, volume and mol numbers.
        Differentials are computed as functions of (T, V, n).

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

        optional_flags = [lng_t, lng_v, lng_n, lng_tt, lng_vv, lng_tv, lng_tn, lng_vn, lng_nn]
        optional_arrayshapes = [(0,), (0,), (len(n),), (0,), (0,), (0,), (len(n),), (len(n),), (len(n), len(n))]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        lng_t_c, lng_v_c, lng_n_c, lng_tt_c, lng_vv_c, lng_tv_c, lng_tn_c, lng_vn_c, lng_nn_c = optional_ptrs

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
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

    def association_energy_density(self, temp, n_alpha, phi=None, phi_t=None, phi_n=None,
                                   phi_tt=None, phi_tn=None, phi_nn=None, Xk=None):
        """Utility
        Calculate association functional of Sauer and Gross https://doi.org/10/f95br5

        Args:
            temp (float): Temperature (K)
            n_alpha (np.ndarray): Weighted densities (mol based)
            phi (No type, optional): Flag to activate calculation. Defaults to None.
            phi_t (No type, optional): Flag to activate calculation. Defaults to None.
            phi_n (No type, optional): Flag to activate calculation. Defaults to None.
            phi_tt (No type, optional): Flag to activate calculation. Defaults to None.
            phi_tn (No type, optional): Flag to activate calculation. Defaults to None.
            phi_nn (No type, optional): Flag to activate calculation. Defaults to None.
            Xk (np.ndarray): Fraction of non-bonded molecules. Initial value on input, calculated value on output. Set to 0.2 initially.

        Returns:
            Optionally energy density and differentials
        """
        self.activate()
        temp_c = c_double(temp)
        dim = self.nc*6
        assert np.shape(n_alpha) == (self.nc, 6,)
        n_alpha_c = (c_double * dim)(*n_alpha.flatten('F'))
        n_assoc_siets = self.get_n_assoc_sites()
        optional_flags = [phi, phi_t, phi_n, phi_tt, phi_tn, phi_nn, Xk]
        optional_arrayshapes = [(0,), (0,), (self.nc, 6,), (0,), (self.nc, 6,), (self.nc, self.nc, 6, 6,), (n_assoc_siets,)]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        phi_c, phi_t_c, phi_n_c, phi_tt_c, phi_tn_c, phi_nn_c, Xk_c = optional_ptrs

        if Xk is not None:
            Xk_c = (c_double * n_assoc_siets)(*np.array(Xk))
            optional_ptrs[-1] = Xk_c

        self.s_calc_assoc_phi.argtypes = [POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double),
                                          POINTER(c_double)]

        self.s_calc_assoc_phi.restype = None
        self.s_calc_assoc_phi(n_alpha_c,
                              byref(temp_c),
                              phi_c,
                              phi_t_c,
                              phi_n_c,
                              phi_tt_c,
                              phi_tn_c,
                              phi_nn_c,
                              Xk_c)

        return_tuple = ()
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

class PCP_SAFT(pcsaft):
    def __init__(self, comps, parameter_reference="Default"):
        super().__init__(comps, parameter_reference=parameter_reference, polar=True, simplified=False)

class SPC_SAFT(pcsaft):
    def __init__(self, comps, parameter_reference="Default"):
        super().__init__(comps, parameter_reference=parameter_reference, polar=False, simplified=True)
