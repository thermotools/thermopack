# Import ctypes
from ctypes import *
import numpy as np
from .thermo import thermo, c_len_type


class volume_translation(thermo):
    """
    Interface to volume translation methods
    """
    def __init__(self):
        """Constructor
        Set up volume translation methods

        """
        if type(self) is volume_translation:
            raise TypeError("volume_translation class cannot be instantiated directly")
        super(volume_translation, self).__init__()

        self.eoslibinit_init_volume_translation = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_volume_translation"))
        self.s_get_ci = getattr(self.tp, self.get_export_name("", "thermopack_get_volume_shift_parameters"))
        self.s_set_ci = getattr(self.tp, self.get_export_name("", "thermopack_set_volume_shift_parameters"))

    def init_peneloux_volume_translation(self, parameter_reference="Default"):
        """Utility
        Initialize Peneloux volume translations

        Args:
            parameter_reference (str): String defining parameter set, Defaults to "Default"
        """
        self.activate()
        volume_trans_model = "PENELOUX"
        volume_trans_model_c = c_char_p(volume_trans_model.encode('ascii'))
        volume_trans_model_len = c_len_type(len(volume_trans_model))
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))
        self.eoslibinit_init_volume_translation.argtypes = [c_char_p,
                                                            c_char_p,
                                                            c_len_type,
                                                            c_len_type]

        self.eoslibinit_init_volume_translation.restype = None

        self.eoslibinit_init_volume_translation(volume_trans_model_c,
                                                ref_string_c,
                                                volume_trans_model_len,
                                                ref_string_len)

    def disable_volume_translation(self):
        """Utility
        Disable volume translations

        """
        self.activate()
        volume_trans_model = "NOSHIFT"
        volume_trans_model_c = c_char_p(volume_trans_model.encode('ascii'))
        volume_trans_model_len = c_len_type(len(volume_trans_model))
        ref_string = "Default"
        ref_string_c = c_char_p(ref_string.encode('ascii'))
        ref_string_len = c_len_type(len(ref_string))
        self.eoslibinit_init_volume_translation.argtypes = [c_char_p,
                                                            c_char_p,
                                                            c_len_type,
                                                            c_len_type]

        self.eoslibinit_init_volume_translation.restype = None

        self.eoslibinit_init_volume_translation(volume_trans_model_c,
                                                ref_string_c,
                                                volume_trans_model_len,
                                                ref_string_len)

    def get_ci(self, cidx):
        """Utility
        Get volume correction parameters

        Args:
            cidx (int): Component index

        Returns:
            ciA (float): Volume shift param of component cidx (m3/mol)
            ciB (float): Volume shift param of component cidx (m3/mol/K)
            ciC (float): Volume shift param of component cidx (m3/mol/K^2)
            ciD (float): Volume shift param of component cidx (m3/mol/K^3)
            ciE (float): Volume shift param of component cidx (m3/mol/K^4)
            ciF (float): Volume shift param of component cidx (m3/mol/K^5)
            ci_type (int): Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3, QUINTIC=6)
        """
        self.activate()
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
        Set volume correction parameters

        Args:
            cidx (int): Component index
            ciA (float): Volume shift param of component cidx (m3/mol)
            ciB (float): Volume shift param of component cidx (m3/mol/K)
            ciC (float): Volume shift param of component cidx (m3/mol/K^2)
            ciD (float): Volume shift param of component cidx (m3/mol/K^3)
            ciE (float): Volume shift param of component cidx (m3/mol/K^4)
            ciF (float): Volume shift param of component cidx (m3/mol/K^5)
            ci_type (int): Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3, QUINTIC=6)
        """
        self.activate()
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
