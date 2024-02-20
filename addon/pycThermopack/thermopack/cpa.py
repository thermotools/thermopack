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


class cpa(cubic):
    """
    Interface to cubic plus association model
    """
    def __init__(self, comps=None, eos="SRK", mixing="vdW", alpha="Classic",
             parameter_reference="Default"):
        """Constructor
        Initialize cubic plus association model in thermopack

        If no components are specified, model must be initialized for specific components later by direct call to 'init'
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            eos (str, optional): Cubic equation of state. Defaults to "SRK".
            mixing (str, optional): Mixture model. Defaults to "vdW".
            alpha (str, optional): Alpha model. Defaults to "Classic".
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        super(cpa, self).__init__()

        # Init methods
        self.eoslibinit_init_cpa = getattr(self.tp, self.get_export_name("eoslibinit", "init_cpa"))
        # Tuning methods
        self.s_get_kij = getattr(self.tp, self.get_export_name("saft_interface", "cpa_get_kij"))
        self.s_set_kij = getattr(self.tp, self.get_export_name("saft_interface", "cpa_set_kij"))
        self.s_get_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "cpa_get_pure_params"))
        self.s_set_pure_params = getattr(self.tp, self.get_export_name("saft_interface", "cpa_set_pure_params"))
        # Options methods
        self.s_use_simplified_cpa = getattr(self.tp, self.get_export_name("saft_interface", "setcpaformulation"))
        self.s_set_cpa_formulation = getattr(self.tp, self.get_export_name("saft_interface", "setcpaformulation"))
        # Info methods
        self.s_print_cpa_report = getattr(self.tp, self.get_export_name("saft_interface", "print_cpa_report"))

        if comps is not None:
            self.init(comps, eos, mixing, alpha, parameter_reference)


    #################################
    # Init
    #################################

    def init(self, comps, eos="SRK", mixing="vdW", alpha="Classic",
             parameter_reference="Default"):
        """Constructor
        Initialize cubic plus association model in thermopack

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

    def print_cpa_report(self):
        """Utility
        Print cpa parameters
        
        Parameters printed are the five pure parameters
        a0, b, epsilon, beta, c1, and the binary parameter
        for the cubic part, kij_a, and for the association part, kij_eps.
        """
        self.s_print_cpa_report()

    def get_kij(self, c1, c2):
        """Utility
        Get attractive energy interaction parameter

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

    def get_pure_params(self, ic):
        """Utility
        Get pure parameters

        Args:
            ic (int): Component index

        Returns:
            params (array_like): a0 (Pa*L^2/mol^2), b (L/mol), eps (J/mol), beta (-), c1 (-)
        """
        self.activate()
        ic_c = c_int(ic)
        params_c = (c_double * 5)(0.0)
        self.s_get_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_get_pure_params.restype = None

        self.s_get_pure_params(byref(ic_c), params_c)

        return np.array(params_c)

    def set_pure_params(self, ic, params):
        """Utility
            Set pure parameters
           Input a0, b in their conventional (non-SI) units,
           beta and eps in SI units, c1 dimensionless.

        Args:
            ic (int): Component index
            params (array_like): a0 (Pa*L^2/mol^2), b (L/mol), eps (J/mol), beta (-), c1 (-)
        """
        self.activate()
        ic_c = c_int(ic)
        params_c = (c_double * 5)(*params)
        self.s_set_pure_params.argtypes = [POINTER(c_int),
                                           POINTER(c_double)]

        self.s_set_pure_params.restype = None

        self.s_set_pure_params(byref(ic_c), params_c)

    def set_kij(self, c1, c2, kij_a, kij_eps):
        """Utility
        Set attractive energy interaction parameter

        Args:
            c1 (int): Component one
            c2 (int): Component two
            kij_a (int): cubic i-j interaction parameter 
            kij_eps (int): association i-j interaction parameter
        """
        self.activate()
        c1_c = c_int(c1)
        c2_c = c_int(c2)
        kij_a_c = c_double(kij_a)
        kij_eps_c = c_double(kij_eps)
        self.s_set_kij.argtypes = [POINTER(c_int),
                                   POINTER(c_int),
                                   POINTER(c_double),
                                   POINTER(c_double)]

        self.s_set_kij.restype = None

        self.s_set_kij(byref(c1_c),
                       byref(c2_c),
                       byref(kij_a_c),
                       byref(kij_eps_c))

    def use_simplified_cpa(self, simplified):
        """Utility
        Use simplified form for rdf in CPA

        Args:
            simplified (bool): True if simplified
        """
        simplified_c = c_bool(simplified)
        self.s_use_simplified_cpa.argtypes = [POINTER(c_bool)]
        self.s_use_simplified_cpa.restype = None
        self.s_use_simplified_cpa(byref(simplified_c))

    def set_cpa_formulation(self, simplified, elliot):
        """Utility
        Set CPA formulation
        Args:
            simplified (bool): Use simplified form for rdf in CPA?
            elliot (bool): use Elliot mixing rule for association Deltas?
        """
        simplified_c = c_bool(simplified)
        elliot_c = c_bool(elliot)
        self.s_set_cpa_formulation.argtypes = [POINTER(c_bool), POINTER(c_bool)]
        self.s_set_cpa_formulation.restype = None
        self.s_set_cpa_formulation(byref(simplified_c), byref(elliot_c))


class SRK_CPA(cpa):
    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default"):
        """Constructor
        Basic convenience class, calls the `cpa` constructor with `eos='SRK'`.
        """
        super().__init__(comps, 'SRK', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference)

class PR_CPA(cpa):
    def __init__(self, comps, mixing="vdW", alpha="Classic", parameter_reference="Default"):
        """Constructor
        Basic convenience class, calls the `cpa` constructor with `eos='PR'`.
        """
        super().__init__(comps, 'PR', mixing=mixing, alpha=alpha, parameter_reference=parameter_reference)