from ctypes import *
import numpy as np
from sys import platform, exit
from os import path
from .thermo import thermo, c_len_type


class hydrate(thermo):
    """
    Interface to hydrate model and mapping methods
    """
    def __init__(self):
        """
        Initialize hydrate model and specific function pointers
        """
        # Load dll/so
        super(hydrate, self).__init__()

        self.s_hydrate_init = getattr(self.tp, self.get_export_name("hydrate", "init_hydrate_model"))
        self.s_fugacity_water_in_hydrate = getattr(self.tp, self.get_export_name("hydrate", "fugacity_water_in_hydrate_tpx"))
        self.s_fugacity_water_in_hydrate_tvn = getattr(self.tp, self.get_export_name("hydrate", "fugacity_water_in_hydrate_tvn"))
        self.s_map_hydrate_appearance_curve = getattr(self.tp, self.get_export_name("hydrate_curves", "map_hydrate_appearance_curve"))


    #################################
    # Init
    #################################

    def init_hydrate(self, parameter_reference="Default"):
        """Initialize hydrate model in thermopack

        Args:
            parameter_reference (str, optional): Which parameters to use?. Defaults to "Default".
        """
        self.activate()
        ref_string_c = c_char_p(parameter_reference.encode('ascii'))
        ref_string_len = c_len_type(len(parameter_reference))
        solid_model_c = POINTER(c_int)()
        self.s_hydrate_init.argtypes = [c_char_p,
                                        POINTER(c_int),
                                        c_len_type]

        self.s_hydrate_init.restype = None

        self.s_hydrate_init(ref_string_c,
                            solid_model_c,
                            ref_string_len)

    #################################
    # Fugacities
    #################################

    def fugacity_water_in_hydrate(self, T, P, z, phase):
        """Get fugacity of water in hydrate

        Args:
            T (float): Temperature (K)
            P (float): Pressure (Pa)
            x (array_like): Molar composition
            phase (int): Phase integer
        Returns:
            fug (float): fugacity of water in hydrate
        """
        self.activate()
        T_c = c_double(T)
        P_c = c_double(P)
        z_c = (c_double * len(z))(*z)
        phase_c = c_int(phase)
        fug_c = c_double(0.0)
        self.s_fugacity_water_in_hydrate.argtypes = [POINTER(c_double),
                                                     POINTER(c_double),
                                                     POINTER(c_double),
                                                     POINTER(c_int),
                                                     POINTER(c_double)]

        self.s_fugacity_water_in_hydrate.restype = None

        self.s_fugacity_water_in_hydrate(byref(T_c),
                                         byref(P_c),
                                         z_c,
                                         byref(phase_c),
                                         byref(fug_c))

        return fug_c.value

    def fugacity_water_in_hydrate(self, T, V, n):
        """Get fugacity of water in hydrate

        Args:
            T (float): Temperature (K)
            V (float): Volume (m3)
            z (array_like): Mol numbers (mol)
        Returns:
            fug (float): fugacity of water in hydrate
        """
        self.activate()
        T_c = c_double(T)
        V_c = c_double(V)
        n_c = (c_double * len(n))(*n)
        fug_c = c_double(0.0)
        fug_T_c = POINTER(c_double)()
        fug_V_c = POINTER(c_double)()
        fug_n_c = POINTER(c_double)()
        self.s_fugacity_water_in_hydrate_tvn.argtypes = [POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_int),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_double)]

        self.s_fugacity_water_in_hydrate_tvn.restype = None

        self.s_fugacity_water_in_hydrate_tvn(byref(T_c),
                                             byref(P_c),
                                             z_c,
                                             byref(phase_c),
                                             byref(fug_c),
                                             fug_T_c,
                                             fug_V_c,
                                             fug_n_c)

        return fug_c.value

    #################################
    # Curve mapping
    #################################

    def get_hydrate_apperance_curve(self, minimum_pressure, z, minimum_temperature,
                                    maximum_pressure, print_to_file=False):
        """Get the hydrate appearance curve curve

        Args:
            minimum_pressure (float): Start mapping form minimum pressure (Pa).
            z (array_like): Composition (-)
            minimum_temperature (float): Exit on minimum pressure (K).
            maximum_pressure (float): Exit on maximum pressure (Pa).
            print_to_file (boolean, optional): Save results to file hydrate.dat ?.
        Returns:
            ndarray: Temperature values (K)
            ndarray: Pressure values (Pa)
        """
        self.activate()
        nmax = 5000
        z_c = (c_double * len(z))(*z)
        min_temp_c = c_double(minimum_temperature)
        min_press_c = c_double(minimum_pressure)
        max_press_c = c_double(maximum_pressure)
        nmax_c = c_int(nmax)
        Ta_c = (c_double * nmax)(0.0)
        Pa_c = (c_double * nmax)(0.0)
        n_c = c_int(0)
        print_to_file_c = c_int(int(print_to_file == 'true'))

        self.s_map_hydrate_appearance_curve.argtypes = [POINTER( c_double ),
                                                        POINTER( c_double ),
                                                        POINTER( c_double ),
                                                        POINTER( c_double ),
                                                        POINTER( c_int ),
                                                        POINTER( c_int ),
                                                        POINTER( c_double ),
                                                        POINTER( c_double ),
                                                        POINTER( c_int )]

        self.s_map_hydrate_appearance_curve.restype = None

        self.s_map_hydrate_appearance_curve(z_c,
                                            byref(min_press_c),
                                            byref(max_press_c),
                                            byref(min_temp_c),
                                            byref(nmax_c),
                                            byref(n_c),
                                            Ta_c,
                                            Pa_c,
                                            byref(print_to_file_c))

        n = n_c.value

        if n > 0:
            t_vals = np.array(Ta_c[0:n])
            p_vals = np.array(Pa_c[0:n])
        else:
            t_vals = None
            p_vals = None

        return t_vals, p_vals
