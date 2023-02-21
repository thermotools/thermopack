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

c_len_type = thermo.c_len_type


class saft(thermo.thermo):
    """
    Methods common for saft type equations of state
    """

    def __init__(self):
        """
        Initialize cubic specific function pointers
        """
        # Load dll/so
        super(saft, self).__init__()

        # SAFT specific methods
        self.s_calc_saft_dispersion = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_saft_dispersion"))
        self.s_calc_hs_diameter = getattr(self.tp, self.get_export_name(
            "saft_interface", "calc_hard_sphere_diameter"))
        self.s_de_broglie_wavelength = getattr(
            self.tp, self.get_export_name("saft_interface", "de_broglie_wavelength"))
        self.s_fres_multipol = getattr(
            self.tp, self.get_export_name("multipol", "fres_multipol"))

        self.m = None
        self.sigma = None
        self.eps_div_kb = None

    def hard_sphere_diameters(self, temp):
        """Calculate hard-sphere diameters given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)

        Returns:
            float: Hard-sphere diameter (m)
        """
        self.activate()
        temp_c = c_double(temp)
        d_c = (c_double * self.nc)(0.0)

        self.s_calc_hs_diameter.argtypes = [POINTER(c_double),
                                            POINTER(c_double)]

        self.s_calc_hs_diameter.restype = None

        self.s_calc_hs_diameter(byref(temp_c),
                                d_c)

        return np.array(d_c)

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
                                    a_tv_c,
                                    a_vv_c,
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


    def fres_multipol(self, temp, volume, n, qq=True, dd=True, dq=True):
        """Calculate reduced Helmholtz energy contribution from polar model

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            qq (bool): Include quadrupole-quadrupole contribution?
            dd (bool): Include dipole-dipole contribution?
            dq (bool): Include dipole-quadrupole contribution?

        Returns:
            float: Reduced Helmholtz energy contribution (mol)
        """
        self.activate()
        temp_c = c_double(temp)
        volume_c = c_double(volume)
        n_c = (c_double * len(n))(*n)
        qq_c = c_int(1 if qq else 0)
        dd_c = c_int(1 if dd else 0)
        dq_c = c_int(1 if dq else 0)
        f_c = c_double(0.0)

        self.s_fres_multipol.argtypes = [POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_int),
                                         POINTER(c_int),
                                         POINTER(c_int),
                                         POINTER(c_double)]

        self.s_fres_multipol.restype = None

        self.s_fres_multipol(byref(temp_c),
                             byref(volume_c),
                             n_c,
                             byref(qq_c),
                             byref(dd_c),
                             byref(dq_c),
                             byref(f_c))

        return f_c.value
