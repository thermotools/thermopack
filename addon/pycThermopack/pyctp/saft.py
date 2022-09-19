# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from . import thermo
from .thermo import thermopack


c_len_type = thermo.c_len_type


class saft(thermopack):
    """
    Methods common for saft type equations of state
    """

    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        thermopack.__init__(self)

        # SAFT specific methods
        self.s_calc_saft_dispersion = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_saft_dispersion"))
        self.s_calc_hs_diameter = getattr(self.tp, self.get_export_name(
            "saft_interface", "calc_hard_sphere_diameter"))
        self.s_de_broglie_wavelength = getattr(
            self.tp, self.get_export_name("saft_interface", "de_broglie_wavelength"))
        self.s_potential = getattr(
            self.tp, self.get_export_name("saft_interface", "potential"))
        self.s_de_boer_parameter = getattr(
            self.tp, self.get_export_name("saft_interface",
                                          "de_boer_parameter"))
        self.s_adjust_mass_to_de_boer_parameter = getattr(
            self.tp, self.get_export_name("saft_interface",
                                          "adjust_mass_to_specified_de_boer_parameter"))
        self.s_calc_soft_repulsion = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_soft_repulsion"))

        self.m = None
        self.sigma = None
        self.eps_div_kb = None

    def hard_sphere_diameters(self, temp):
        """Calculate hard-sphere diameters given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)

        Returns:
            np.ndarray: Hard-sphere diameter (m)
            np.ndarray: Temperature differential of hard-sphere diameter (m/K)
        """
        self.activate()
        temp_c = c_double(temp)
        d_c = (c_double * self.nc)(0.0)
        dT_c = (c_double * self.nc)(0.0)

        self.s_calc_hs_diameter.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double)]

        self.s_calc_hs_diameter.restype = None

        self.s_calc_hs_diameter(byref(temp_c),
                                d_c,
                                dT_c)

        return  np.array(d_c), np.array(dT_c)

    def a_dispersion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                     a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Calculate dispersion contribution given temperature, volume and mol numbers.
           a = A_disp/(nRT)
        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            a_t (No type, optional): Flag to activate calculation. Defaults to None.
            a_v (No type, optional): Flag to activate calculation. Defaults to None.
            a_n (No type, optional): Flag to activate calculation. Defaults to None.
            a_tt (No type, optional): Flag to activate calculation. Defaults to None.
            a_vv (No type, optional): Flag to activate calculation. Defaults to None.
            a_tv (No type, optional): Flag to activate calculation. Defaults to None.
            a_tn (No type, optional): Flag to activate calculation. Defaults to None.
            a_vn (No type, optional): Flag to activate calculation. Defaults to None.
            a_nn (No type, optional): Flag to activate calculation. Defaults to None.

        Returns:
            ndarry:
            Optionally differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        a_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if a_t is None:
            a_t_c = null_pointer
        else:
            a_t_c = c_double(0.0)
        if a_v is None:
            a_v_c = null_pointer
        else:
            a_v_c = c_double(0.0)
        if a_n is None:
            a_n_c = null_pointer
        else:
            a_n_c = (c_double * len(n))(0.0)
        if a_tt is None:
            a_tt_c = null_pointer
        else:
            a_tt_c = c_double(0.0)
        if a_vv is None:
            a_vv_c = null_pointer
        else:
            a_vv_c = c_double(0.0)
        if a_tv is None:
            a_tv_c = null_pointer
        else:
            a_tv_c = c_double(0.0)
        if a_tn is None:
            a_tn_c = null_pointer
        else:
            a_tn_c = (c_double * len(n))(0.0)
        if a_vn is None:
            a_vn_c = null_pointer
        else:
            a_vn_c = (c_double * len(n))(0.0)
        if a_nn is None:
            a_nn_c = null_pointer
        else:
            a_nn_c = (c_double * len(n)**2)(0.0)

        self.s_calc_saft_dispersion.argtypes = [POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
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

        self.s_calc_saft_dispersion.restype = None

        self.s_calc_saft_dispersion(byref(temp_c),
                                    byref(v_c),
                                    n_c,
                                    byref(a_c),
                                    a_t_c,
                                    a_v_c,
                                    a_n_c,
                                    a_tt_c,
                                    a_vv_c,
                                    a_tv_c,
                                    a_tn_c,
                                    a_vn_c,
                                    a_nn_c)

        return_tuple = (a_c.value, )
        if not a_t is None:
            return_tuple += (a_t_c.value, )
        if not a_v is None:
            return_tuple += (a_v_c.value, )
        if not a_n is None:
            return_tuple += (np.array(a_n_c), )
        if not a_tt is None:
            return_tuple += (a_tt_c.value, )
        if not a_tv is None:
            return_tuple += (a_tv_c.value, )
        if not a_vv is None:
            return_tuple += (a_vv_c.value, )
        if not a_tn is None:
            return_tuple += (np.array(a_tn_c), )
        if not a_vn is None:
            return_tuple += (np.array(a_vn_c), )
        if not a_nn is None:
            a_nn = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    a_nn[i][j] = a_nn_c[i + j*len(n)]
            return_tuple += (a_nn, )

        return return_tuple

    def a_soft_repulsion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                         a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Calculate soft repuslion contribution given temperature, volume and mol numbers.
           a = A_soft_rep/(nRT)
        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            a_t (No type, optional): Flag to activate calculation. Defaults to None.
            a_v (No type, optional): Flag to activate calculation. Defaults to None.
            a_n (No type, optional): Flag to activate calculation. Defaults to None.
            a_tt (No type, optional): Flag to activate calculation. Defaults to None.
            a_vv (No type, optional): Flag to activate calculation. Defaults to None.
            a_tv (No type, optional): Flag to activate calculation. Defaults to None.
            a_tn (No type, optional): Flag to activate calculation. Defaults to None.
            a_vn (No type, optional): Flag to activate calculation. Defaults to None.
            a_nn (No type, optional): Flag to activate calculation. Defaults to None.

        Returns:
            ndarry:
            Optionally differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        a_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if a_t is None:
            a_t_c = null_pointer
        else:
            a_t_c = c_double(0.0)
        if a_v is None:
            a_v_c = null_pointer
        else:
            a_v_c = c_double(0.0)
        if a_n is None:
            a_n_c = null_pointer
        else:
            a_n_c = (c_double * len(n))(0.0)
        if a_tt is None:
            a_tt_c = null_pointer
        else:
            a_tt_c = c_double(0.0)
        if a_vv is None:
            a_vv_c = null_pointer
        else:
            a_vv_c = c_double(0.0)
        if a_tv is None:
            a_tv_c = null_pointer
        else:
            a_tv_c = c_double(0.0)
        if a_tn is None:
            a_tn_c = null_pointer
        else:
            a_tn_c = (c_double * len(n))(0.0)
        if a_vn is None:
            a_vn_c = null_pointer
        else:
            a_vn_c = (c_double * len(n))(0.0)
        if a_nn is None:
            a_nn_c = null_pointer
        else:
            a_nn_c = (c_double * len(n)**2)(0.0)

        self.s_calc_soft_repulsion.argtypes = [POINTER(c_double),
                                               POINTER(c_double),
                                               POINTER(c_double),
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

        self.s_calc_soft_repulsion.restype = None

        self.s_calc_soft_repulsion(byref(temp_c),
                                   byref(v_c),
                                   n_c,
                                   byref(a_c),
                                   a_t_c,
                                   a_v_c,
                                   a_n_c,
                                   a_tt_c,
                                   a_vv_c,
                                   a_tv_c,
                                   a_tn_c,
                                   a_vn_c,
                                   a_nn_c)

        return_tuple = (a_c.value, )
        if not a_t is None:
            return_tuple += (a_t_c.value, )
        if not a_v is None:
            return_tuple += (a_v_c.value, )
        if not a_n is None:
            return_tuple += (np.array(a_n_c), )
        if not a_tt is None:
            return_tuple += (a_tt_c.value, )
        if not a_tv is None:
            return_tuple += (a_tv_c.value, )
        if not a_vv is None:
            return_tuple += (a_vv_c.value, )
        if not a_tn is None:
            return_tuple += (np.array(a_tn_c), )
        if not a_vn is None:
            return_tuple += (np.array(a_vn_c), )
        if not a_nn is None:
            a_nn = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    a_nn[i][j] = a_nn_c[i + j*len(n)]
            return_tuple += (a_nn, )

        return return_tuple

    def de_broglie_wavelength(self, c, temp):
        """Calculate de Broglie wavelength

        Args:
            c (int): Component index (FORTRAN)
            temp (float): Temperature (K)

        Returns:
            float: de Broglie wavelength (m)
        """
        self.activate()
        temp_c = c_double(temp)
        c_c = c_int(c)
        lambda_c = c_double(0.0)

        self.s_de_broglie_wavelength.argtypes = [POINTER(c_double),
                                                 POINTER(c_double),
                                                 POINTER(c_double)]

        self.s_de_broglie_wavelength.restype = None

        self.s_de_broglie_wavelength(byref(c_c),
                                     byref(temp_c),
                                     byref(lambda_c))

        return lambda_c.value

    def print_saft_parameters(self, c):
        """Print saft parameters for component c

        Args:
            c (int): Component index (FORTRAN)

        """
        name = self.get_comp_name(c)
        print(f"{name}:")
        print(f"Segments: {self.m[c-1]}")
        print(f"sigma: {self.sigma[c-1]}")
        print(f"eps div kB: {self.eps_div_kb[c-1]}")

    def potential(self, ic, jc, r, temp):
        """Get potential energy divided by Boltzmann constant
        as a function of r

        Args:
            ic, jc (int): Component indices (FORTRAN)
            r (array_like): Distance between particles (m)
            temp (float): Temperature (K)
        Returns:
            array_like: Potential energy divided by Boltzmann constant (K)
        """
        self.activate()
        ic_c = c_int(ic)
        jc_c = c_int(jc)
        n_c = c_int(len(r))
        temp_c = c_double(temp)
        r_c = (c_double * len(r))(*r)
        pot_c = (c_double * len(r))(0.0)

        self.s_potential.argtypes = [POINTER(c_int),
                                     POINTER(c_int),
                                     POINTER(c_int),
                                     POINTER(c_double),
                                     POINTER(c_double),
                                     POINTER(c_double)]

        self.s_potential.restype = None

        self.s_potential(byref(ic_c),
                         byref(jc_c),
                         byref(n_c),
                         r_c,
                         byref(temp_c),
                         pot_c)

        return np.array(pot_c)

    def adjust_mass_to_de_boer_parameter(self, c, de_boer):
        """Adjust mass in order to get specified de Boer parameter

        Args:
            c (int): Component index (FORTRAN)
            de_boer (float): de Boer parameter

        """
        self.activate()
        c_c = c_int(c)
        de_boer_c = c_double(de_boer)

        self.s_adjust_mass_to_de_boer_parameter.argtypes = [POINTER(c_int),
                                                            POINTER(c_double)]

        self.s_adjust_mass_to_de_boer_parameter.restype = None

        self.s_adjust_mass_to_de_boer_parameter(byref(c_c),
                                                byref(de_boer_c))

    def de_boer_parameter(self, c):
        """Get de Boer parameter

        Args:
            c (int): Component index (FORTRAN)
        Results:
            de_boer (float): de Boer parameter

        """
        self.activate()
        c_c = c_int(c)
        de_boer_c = c_double(0.0)

        self.s_de_boer_parameter.argtypes = [POINTER(c_int),
                                             POINTER(c_double)]

        self.s_de_boer_parameter.restype = None

        self.s_de_boer_parameter(byref(c_c),
                                 byref(de_boer_c))
        return de_boer_c.value
