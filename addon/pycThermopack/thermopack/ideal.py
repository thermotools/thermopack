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


class ideal(thermo):
    """
    Interface to ideal eos
    """
    def __init__(self, comps=None, parameter_reference="Default"):
        """Constructor
        Initialize ideal model in thermopack

        Unless 'comps' parameter is specified, model must be initialized for specific components
        later by direct call to 'init'.
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        # Load dll/so
        super(ideal, self).__init__()

        # Init methods
        self.eoslibinit_init_ideal = getattr(self.tp, self.get_export_name("eoslibinit", "init_ideal_eos"))

        if comps is not None:
            self.init(comps, parameter_reference)
        else:
            missing_args = ['comps']
            warnings.warn('Ideal EoS not completely initialized, due to missing parameter(s) :'+str(missing_args)+'.\n'
                          'Complete initialisation by explicitly calling this classes "init" method.', Warning)

        # A range of methods, specifically those related to phase equilibria and -stability, make no sense for an
        # ideal gas. These are included in the list `not_supported`, and are monkey patched to raise a NotImplementedError
        # If new methods that should not be supported by `ideal` are added to `thermo`, just include them at the
        # end of the `not_supported` list.
        not_supported = [self.bubble_temperature, self.bubble_pressure, self.dew_temperature, self.dew_pressure,
                         self.get_envelope_twophase, self.get_pure_fluid_saturation_curve, self.get_binary_pxy,
                         self.get_binary_txy, self.get_bp_term, self.binary_triple_point_pressure,
                         self.global_binary_plot, self.solid_envelope_plot, self.envelope_isentrope_cross,
                         self.saturation_points_from_property, self.saturation_point_from_property_bracket_search,
                         self.critical, self.spinodal, self.spinodal_point, self.map_meta_isentrope, self.map_meta_isotherm,
                         self.tv_meta_ps]

        for method in not_supported:
            self.__dict__[method.__name__] = lambda *args, _method_name_=method.__name__, **kwargs: self.__not_supported__(_method_name_)

    @staticmethod
    def __not_supported__(method):
        raise NotImplementedError(f'Class ideal does not support method {method}')

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Constructor
        Initialize ideal model in thermopack

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))
        ierr_c = c_int(0)

        self.eoslibinit_init_ideal.argtypes = [c_char_p,
                                               POINTER (c_int),
                                               c_char_p,
                                               c_len_type,
                                               c_len_type]

        self.eoslibinit_init_ideal.restype = None

        self.eoslibinit_init_ideal(comp_string_c,
                                   ierr_c,
                                   ref_string_c,
                                   comp_string_len,
                                   ref_string_len)
        self.nc = max(len(comps.split(" ")),len(comps.split(",")))