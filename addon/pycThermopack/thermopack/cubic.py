# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
import warnings
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from .thermo import thermo, c_len_type


class cubic(thermo):
    """
    Interface to cubic
    """
    def __init__(self, comps=None, eos=None, mixing="vdW", alpha="Classic",
             parameter_reference="Default", volume_shift=False):
        """Constructor
        Initialize cubic model in thermopack

        Unless both 'comps' and 'eos' parameters are specified, model must be initialized for specific components
        later by direct call to 'init'.
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            eos (str, optional): Equation of state (SRK, PR, ...)
            mixing (str, optional): Mixture model. Defaults to "vdW".
            alpha (str, optional): Alpha model. Defaults to "Classic".
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        super(cubic, self).__init__()

        # Init methods
        self.eoslibinit_init_cubic = getattr(self.tp, self.get_export_name("eoslibinit", "init_cubic"))
        self.eoslibinit_init_pseudo = getattr(self.tp, self.get_export_name("eoslibinit", "init_cubic_pseudo"))

        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name("", "thermopack_getkij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name("", "thermopack_setkijandji"))

        self.s_get_lij = getattr(self.tp, self.get_export_name("", "thermopack_getlij"))
        self.s_set_lij = getattr(self.tp, self.get_export_name("", "thermopack_setlijandji"))


        self.s_get_hv_param = getattr(self.tp, self.get_export_name("", "thermopack_gethvparam"))
        self.s_set_hv_param = getattr(self.tp, self.get_export_name("", "thermopack_sethvparam"))

        self.s_get_ws_param = getattr(self.tp, self.get_export_name("", "thermopack_getwsparam"))
        self.s_set_ws_param = getattr(self.tp, self.get_export_name("", "thermopack_setwsparam"))

        self.s_get_ci = getattr(self.tp, self.get_export_name("", "thermopack_get_volume_shift_parameters"))
        self.s_set_ci = getattr(self.tp, self.get_export_name("", "thermopack_set_volume_shift_parameters"))

        self.s_get_covolumes = getattr(self.tp, self.get_export_name("cubic_eos", "get_covolumes"))
        self.s_get_energy_constants = getattr(self.tp, self.get_export_name("cubic_eos", "get_energy_constants"))

        self.s_set_alpha_corr = getattr(self.tp, self.get_export_name("", "thermopack_set_alpha_corr"))
        self.s_set_beta_corr = getattr(self.tp, self.get_export_name("", "thermopack_set_beta_corr"))

        if None not in (comps, eos):
            self.init(comps, eos, mixing, alpha, parameter_reference, volume_shift)
        elif self is cubic:
            missing_args = []
            if comps is None:
                missing_args.append('comps')
            if eos is None:
                missing_args.append('eos')
            warnings.warn('Cubic EoS not completely initialized, due to missing parameter(s) :'+str(missing_args)+'.\n'
                          'Complete initialisation by explicitly calling this classes "init" method.', Warning)

    #################################
    # Init
    #################################

    def init(self, comps, eos, mixing="vdW", alpha="Classic",
             parameter_reference="Default", volume_shift=False):
        """Constructor
        Initialize cubic model in thermopack

        Args:
            comps (str): Comma separated list of component names
            eos (str): Equation of state (SRK, PR, ...)
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

        if volume_shift:
            vol_shift_c = c_int(1)
        else:
            vol_shift_c = c_int(0)

        self.eoslibinit_init_cubic.argtypes = [c_char_p,
                                               c_char_p,
                                               c_char_p,
                                               c_char_p,
                                               c_char_p,
                                               POINTER (c_int),
                                               c_len_type,
                                               c_len_type,
                                               c_len_type,
                                               c_len_type,
                                               c_len_type]

        self.eoslibinit_init_cubic.restype = None

        self.eoslibinit_init_cubic(comp_string_c,
                                   eos_c,
                                   mixing_c,
                                   alpha_c,
                                   ref_string_c,
                                   byref(vol_shift_c),
                                   comp_string_len,
                                   eos_len,
                                   mixing_len,
                                   alpha_len,
                                   ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))

    def init_pseudo(self, comps, Tclist, Pclist, acflist, Mwlist=None, mixing="vdW", alpha="Classic"):
        """Constructor
        Initialize pseudocomponents of cubic model in thermopack. The cubic
        init routine must have been called first.

        Args:
            comps (str): Comma separated list of names for all components
            Tclist (array_like): Critical temperatures (K)
            Pclist (array_like): Critical pressures (Pa)
            acflist (array_like): acentric factors (-)
            Mwlist (array_like): Molar masses (kg/mol)
            mixing (str): Mixing rule
            alpha (str): alpha correlation
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_len_type(len(mixing))
        alpha_c = c_char_p(alpha.encode('ascii'))
        alpha_len = c_len_type(len(alpha))

        Tc_c = (c_double * self.nc)(*Tclist)
        Pc_c = (c_double * self.nc)(*Pclist)
        acf_c = (c_double * self.nc)(*acflist)

        null_pointer = POINTER(c_double)()
        Mw_c = null_pointer if Mwlist is None else (c_double * self.nc)(*Mwlist)

        self.eoslibinit_init_pseudo.argtypes = [c_char_p,
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                c_char_p,
                                                c_char_p,
                                                c_len_type,
                                                c_len_type,
                                                c_len_type]
        self.eoslibinit_init_pseudo.restype = None
        self.eoslibinit_init_pseudo(comp_string_c,
                                    Tc_c,
                                    Pc_c,
                                    acf_c,
                                    Mw_c,
                                    mixing_c,
                                    alpha_c,
                                    comp_string_len,
                                    mixing_len,
                                    alpha_len)

    def get_kij(self, c1, c2):
        """Utility
        Get attractive energy interaction parameter kij, where aij = sqrt(ai*aj)*(1-kij)

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            kij (float): i-j interaction parameter
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
        Set attractive energy interaction parameter kij, where aij = sqrt(ai*aj)*(1-kij)

        Args:
            c1 (int): Component one
            c2 (int): Component two
            kij (float): i-j interaction parameter
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


    def get_lij(self, c1, c2):
        """Utility
        Get co-volume interaction parameter lij, where bij = 0.5*(bi+bj)*(1-lij)

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            lij (float): i-j interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        lij_c = c_double(0.0)
        self.s_get_lij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_get_lij.restype = None

        self.s_get_lij(byref(c1_c),
                       byref(c2_c),
                       byref(lij_c))

        return lij_c.value

    def set_lij(self, c1, c2, lij):
        """Utility
        Set co-volume interaction parameter lij, where bij = 0.5*(bi+bj)*(1-lij)

        Args:
            c1 (int): Component one
            c2 (int): Component two
            lij ([type]): [description]
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        lij_c = c_double(lij)
        self.s_set_lij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double)]

        self.s_set_lij.restype = None

        self.s_set_lij(byref(c1_c),
                       byref(c2_c),
                       byref(lij_c))

    def get_hv_param(self, c1, c2):
        """Utility
        Get Huron-Vidal parameters

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            alpha_ij (float): alpha param i-j
            alpha_ji (float): alpha param j-i
            a_ij (float): a param i-j
            a_ji (float): a param j-i
            b_ij (float): b param i-j
            b_ji (float): b param j-i
            c_ij (float): c param i-j
            c_ji (float): c param j-i
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        alpha_ij_c = c_double(0.0)
        alpha_ji_c = c_double(0.0)
        a_ij_c = c_double(0.0)
        a_ji_c = c_double(0.0)
        b_ij_c = c_double(0.0)
        b_ji_c = c_double(0.0)
        c_ij_c = c_double(0.0)
        c_ji_c = c_double(0.0)

        self.s_get_hv_param.argtypes = [POINTER(c_int),
                                        POINTER(c_int),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double)]

        self.s_get_hv_param.restype = None

        self.s_get_hv_param(byref(c1_c),
                            byref(c2_c),
                            byref(alpha_ij_c),
                            byref(alpha_ji_c),
                            byref(a_ij_c),
                            byref(a_ji_c),
                            byref(b_ij_c),
                            byref(b_ji_c),
                            byref(c_ij_c),
                            byref(c_ji_c))
        return alpha_ij_c.value, alpha_ji_c.value, a_ij_c.value, a_ji_c.value, b_ij_c.value, b_ji_c.value, c_ij_c.value, c_ji_c.value

    def set_hv_param(self, c1, c2, alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji):
        """Utility
        Set Huron-Vidal parameters

        Args:
            c1 (int): Component one
            c2 (int): Component two
            alpha_ij (float): alpha param i-j
            alpha_ji (float): alpha param j-i
            a_ij (float): a param i-j
            a_ji (float): a param j-i
            b_ij (float): b param i-j
            b_ji (float): b param j-i
            c_ij (float): c param i-j
            c_ji (float): c param j-i
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        alpha_ij_c = c_double(alpha_ij)
        alpha_ji_c = c_double(alpha_ji)
        a_ij_c = c_double(a_ij)
        a_ji_c = c_double(a_ji)
        b_ij_c = c_double(b_ij)
        b_ji_c = c_double(b_ji)
        c_ij_c = c_double(c_ij)
        c_ji_c = c_double(c_ji)

        self.s_set_hv_param.argtypes = [POINTER(c_int),
                                        POINTER(c_int),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double)]

        self.s_set_hv_param.restype = None

        self.s_set_hv_param(byref(c1_c),
                            byref(c2_c),
                            byref(alpha_ij_c),
                            byref(alpha_ji_c),
                            byref(a_ij_c),
                            byref(a_ji_c),
                            byref(b_ij_c),
                            byref(b_ji_c),
                            byref(c_ij_c),
                            byref(c_ji_c))


    def get_ws_param(self, c1, c2):
        """Utility
        Get Wong-Sandler parameters

        Args:
            c1 (int): Component one
            c2 (int): Component two

        Returns:
            alpha_ij (float): alpha param i-j
            alpha_ji (float): alpha param j-i
            k_ij (float): k param i-j
            k_ji (float): k param j-i
            tau_ij (float): tau param i-j
            tau_ji (float): tau param j-i
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        alpha_ij_c = c_double(0.0)
        alpha_ji_c = c_double(0.0)
        k_ij_c = c_double(0.0)
        k_ji_c = c_double(0.0)
        tau_ij_c = c_double(0.0)
        tau_ji_c = c_double(0.0)
        self.s_get_ws_param.argtypes = [POINTER(c_int),
                                        POINTER(c_int)]

        self.s_get_ws_param.restype = None

        self.s_get_ws_param(byref(c1_c),
                            byref(c2_c),
                            byref(alpha_ij_c),
                            byref(alpha_ji_c),
                            byref(k_ij_c),
                            byref(k_ji_c),
                            byref(tau_ij_c),
                            byref(tau_ji_c))
        return alpha_ij_c.value, alpha_ji_c.value, k_ij_c.value, k_ji_c.value, tau_ij_c.value, tau_ji_c.value

    def set_ws_param(self, c1, c2, alpha_ij, alpha_ji, k_ij, k_ji, tau_ij, tau_ji):
        """Utility
        Set Wong-Sandler parameters

        Args:
            c1 (int): Component one
            c2 (int): Component two
            alpha_ij (float): alpha param i-j
            alpha_ji (float): alpha param j-i
            k_ij (float): k param i-j
            k_ji (float): k param j-i
            tau_ij (float): tau param i-j
            tau_ji (float): tau param j-i
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        alpha_ij_c = c_double(alpha_ij)
        alpha_ji_c = c_double(alpha_ji)
        k_ij_c = c_double(k_ij)
        k_ji_c = c_double(k_ji)
        tau_ij_c = c_double(tau_ij)
        tau_ji_c = c_double(tau_ji)

        self.s_set_ws_param.argtypes = [POINTER(c_int),
                                        POINTER(c_int),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double)]

        self.s_set_ws_param.restype = None

        self.s_set_ws_param(byref(c1_c),
                            byref(c2_c),
                            byref(alpha_ij_c),
                            byref(alpha_ji_c),
                            byref(k_ij_c),
                            byref(k_ji_c),
                            byref(tau_ij_c),
                            byref(tau_ji_c))


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


    def get_covolumes(self):
        """Utility
        Get component covolumes (L/mol)

        Returns:
            np.ndarray: Component covolumes (L/mol)
        """
        self.activate()
        b_c = (c_double * self.nc)(0.0)
        self.s_get_covolumes.argtypes = [POINTER(c_double)]
        self.s_get_covolumes.restype = None
        self.s_get_covolumes(b_c)
        return np.array(b_c)

    def get_energy_constants(self):
        """Utility
        Get component energy constants in front of alpha. (Pa*L^2/mol^2)

        Returns:
            np.ndarray: Component energy constants in front of alpha. (Pa*L^2/mol^2)
        """
        self.activate()
        a_c = (c_double * self.nc)(0.0)
        self.s_get_energy_constants.argtypes = [POINTER(c_double)]
        self.s_get_energy_constants.restype = None
        self.s_get_energy_constants(a_c)
        return np.array(a_c)


    def set_alpha_corr(self, ic, corrname, coeffs):
        """Utility
        Set alpha correlation

        Args:
            ic (in): Component number
            corrname (string): Name of correlation
            coeffs (ndarray): Coefficients in correlation
        """

        numparam_c = c_int(len(coeffs))
        ic_c = c_int(ic)
        corrname_string_c = c_char_p(corrname.strip().encode('ascii'))
        corrname_string_len_c = c_len_type(len(corrname))
        coeffs_c = (c_double * len(coeffs)) (*coeffs)
        self.s_set_alpha_corr.argtypes = [POINTER(c_int),
                                          POINTER(c_int),
                                          c_char_p,
                                          POINTER(c_double),
                                          c_len_type]

        self.s_set_alpha_corr.restype = None

        self.s_set_alpha_corr(numparam_c,
                              ic_c,
                              corrname_string_c,
                              coeffs_c,
                              corrname_string_len_c)


    def set_beta_corr(self, ic, corrname, coeffs):
        """Utility
        Set beta correlation

        Args:
            ic (in): Component number
            corrname (string): Name of correlation
            coeffs (ndarray): Coefficients in correlation
        """

        numparam_c = c_int(len(coeffs))
        ic_c = c_int(ic)
        corrname_string_c = c_char_p(corrname.strip().encode('ascii'))
        corrname_string_len_c = c_len_type(len(corrname))
        coeffs_c = (c_double * len(coeffs)) (*coeffs)
        self.s_set_beta_corr.argtypes = [POINTER(c_int),
                                          POINTER(c_int),
                                          c_char_p,
                                          POINTER(c_double),
                                          c_len_type]

        self.s_set_beta_corr.restype = None

        self.s_set_beta_corr(numparam_c,
                             ic_c,
                             corrname_string_c,
                             coeffs_c,
                             corrname_string_len_c)

class VanDerWaals(cubic):

    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='VdW'`.
        """
        super().__init__(comps, 'VdW', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class SoaveRedlichKwong(cubic):

    def __init__(self, comps, mixing="vdW", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='SRK'`.
        """
        super().__init__(comps, 'SRK', mixing=mixing, alpha="Classic", parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class RedlichKwong(cubic):

    def __init__(self, comps, mixing="vdW", alpha="RK", parameter_reference="Default", volume_shift=False):
        """Constructor
        Convenience class for Redlich-Kwong, calls the `cubic` constructor. Set `alpha=Soave` in order to get SRK model.
        """
        super().__init__(comps, 'SRK', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class PengRobinson(cubic):

    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='PR'`. Default `alpha` is the original 1976 correlation.
        """
        super().__init__(comps, 'PR', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class PengRobinson78(cubic):

    def __init__(self, comps, mixing="vdW", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='PR'`. Using the 1978 `alpha` correlation.
        """
        super().__init__(comps, 'PR', mixing=mixing, alpha="PR78", parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class SchmidtWensel(cubic):

    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='SW'`.
        """
        super().__init__(comps, 'SW', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference,
                         volume_shift=volume_shift)

class PatelTeja(cubic):

    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default", volume_shift=False):
        """Constructor
        Basic convenience class, calls the `cubic` constructor with `eos='PT'`.
        """
        super().__init__(comps, 'PT', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference,
                         volume_shift=volume_shift)
