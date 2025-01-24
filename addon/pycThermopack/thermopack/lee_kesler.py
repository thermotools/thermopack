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


class lee_kesler(thermo):
    """
    Interface to Lee-Kesler model
    """
    def __init__(self, comps=None, parameter_reference="Default"):
        """Constructor
        Init Lee-Kesler

        If no components are specified, model must be initialized for specific components later by direct call to 'init'
        Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

        Args:
            comps (str, optional): Comma separated list of component names
            parameter_reference (str, optional): Identifier for parameters set. Defaults to "Default".
        """
        # Load dll/so
        super(lee_kesler, self).__init__()

        # Init methods
        self.eoslibinit_init_lee_kesler = getattr(self.tp, self.get_export_name("eoslibinit", "init_lee_kesler"))
        
        # A range of methods, specifically the TV properties and TVP properties, which are not supported by Lee-Kesler EoS.
        not_supported = [self.enthalpy_tv, self.entropy_tv, self.fugacity_tv, self.chemical_potential_tv, 
                         self.helmholtz_tv, self.internal_energy_tv, self.pressure_tv, self.speed_of_sound_tv,
                         self.enthalpy_tvp, self.entropy_tvp, self.thermo_tvp]

        for method in not_supported:
            self.__dict__[method.__name__] = lambda *args, _method_name_=method.__name__, **kwargs: self.__not_supported__(_method_name_)


        if comps is not None:
            self.init(comps, parameter_reference)
        
    @staticmethod
    def __not_supported__(method):
        raise NotImplementedError(f'Class lee_kesler does not support TV-property methods (Tried to call: {method})')

    #################################
    # Init
    #################################

    def init(self, comps, parameter_reference="Default"):
        """Constructor
        Init Lee-Kesler

        Args:
            comps (str): Comma separated list of component names
            parameter_reference (str, optional): Identefier for parameters set. Defaults to "Default".
        """
        self.activate()
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))

        self.eoslibinit_init_lee_kesler.argtypes = [c_char_p,
                                                    c_char_p,
                                                    c_len_type,
                                                    c_len_type]

        self.eoslibinit_init_lee_kesler.restype = None

        self.eoslibinit_init_lee_kesler(comp_string_c,
                                        ref_string_c,
                                        comp_string_len,
                                        ref_string_len)

        self.nc = max(len(comps.split(" ")), len(comps.split(",")))
