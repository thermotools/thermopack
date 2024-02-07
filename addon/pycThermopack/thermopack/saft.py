# Import ctypes
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
from . import utils
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path
# Import thermo
from .thermo import thermo, c_len_type


class saft(thermo):
    """
    Methods common for saft type equations of state
    """

    def __init__(self):
        """Internal
        Initialize SAFT specific function pointers
        """
        # Load dll/so
        super(saft, self).__init__()

        # Info methods
        self.s_print_binary_mix_report = getattr(
            self.tp, self.get_export_name("saft_interface", "printbinarymixturereportsaft"))

        # SAFT specific methods
        self.s_calc_saft_dispersion = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_saft_dispersion"))
        self.s_calc_saft_hard_sphere = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_saft_hard_sphere"))
        self.s_calc_saft_chain = getattr(
            self.tp, self.get_export_name("saft_interface", "calc_saft_chain"))
        self.s_calc_hs_diameter = getattr(self.tp, self.get_export_name(
            "saft_interface", "calc_hard_sphere_diameter"))
        self.s_calc_hs_diameter_ij = getattr(self.tp, self.get_export_name(
            "saft_interface", "calc_hard_sphere_diameter_ij"))
        self.s_calc_bmcsl_gij_fmt = getattr(self.tp, self.get_export_name(
            "hardsphere_bmcsl", "calc_bmcsl_gij_fmt"))

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
        self.s_truncation_corrections = getattr(
            self.tp, self.get_export_name("saft_interface", "truncation_corrections"))
        self.s_test_fmt_compatibility = getattr(
            self.tp, self.get_export_name("saft_interface", "test_fmt_compatibility"))
        self.s_fmt_energy_density = getattr(
            self.tp, self.get_export_name("fundamental_measure_theory", "fmt_energy_density"))
        self.s_de_broglie_wavelength = getattr(
            self.tp, self.get_export_name("saft_interface", "de_broglie_wavelength"))
        self.s_sigma_ij = getattr(
            self.tp, self.get_export_name("saft_interface", "sigma_ij"))
        self.s_epsilon_ij = getattr(
            self.tp, self.get_export_name("saft_interface", "epsilon_ij"))
        self.s_sigma_eff_ij = getattr(
            self.tp, self.get_export_name("saft_interface", "sigma_eff_ij"))
        self.s_epsilon_eff_ij = getattr(
            self.tp, self.get_export_name("saft_interface", "epsilon_eff_ij"))
        self.s_get_active_assoc_params = getattr(
            self.tp, self.get_export_name("saft_interface", "getactiveassocparams"))
        self.s_set_active_assoc_params = getattr(
            self.tp, self.get_export_name("saft_interface", "setactiveassocparams"))
        self.s_get_n_assoc_sites = getattr(
            self.tp, self.get_export_name("saft_interface", "get_n_assoc_sites"))
        self.s_alpha = getattr(
            self.tp, self.get_export_name("saft_interface", "alpha"))
        self.s_fres_multipol = getattr(
            self.tp, self.get_export_name("multipol", "fres_multipol"))
        self.s_multipol_model_control = getattr(
            self.tp, self.get_export_name("multipol", "multipol_model_control"))

        self.m = None
        self.sigma = None
        self.eps_div_kb = None

    def hard_sphere_diameters(self, temp):
        """Utility
        Calculate hard-sphere diameters given temperature, volume and mol numbers.

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

    def hard_sphere_diameter_ij(self, i, j, temp):
        """Utility
        Calculate non-additive hard-sphere diameter for i-j interaction given temperature.

        Args:
            i (int): Component index (FORTRAN)
            j (int): Component index (FORTRAN)
            temp (float): Temperature (K)

        Returns:
            float: Hard-sphere diameter (m)
            float: Temperature differential of hard-sphere diameter (m/K)
        """
        self.activate()
        i_c = c_int(i)
        j_c = c_int(j)
        temp_c = c_double(temp)
        d_c = c_double(0.0)
        dT_c = c_double(0.0)

        self.s_calc_hs_diameter_ij.argtypes = [POINTER(c_int),
                                               POINTER(c_int),
                                               POINTER(c_double),
                                               POINTER(c_double),
                                               POINTER(c_double)]

        self.s_calc_hs_diameter_ij.restype = None

        self.s_calc_hs_diameter_ij(byref(i_c),
                                   byref(j_c),
                                   byref(temp_c),
                                   byref(d_c),
                                   byref(dT_c))

        return  d_c.value, dT_c.value

    def a_dispersion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                     a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Utility
        Calculate dispersion contribution given temperature, volume and mol numbers. $a = A_{disp}/(nRT)$

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

        optional_flags = [a_t, a_v, a_n,
                          a_tt, a_vv, a_tv,
                          a_tn, a_vn, a_nn]
        optional_arrayshapes = [(0,), (0,), (len(n),),
                               (0,), (0,), (0,),
                               (len(n),), (len(n),), (len(n), len(n))]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        a_t_c, a_v_c, a_n_c, a_tt_c, a_vv_c, a_tv_c, a_tn_c, a_vn_c, a_nn_c = optional_ptrs

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
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

    def a_soft_repulsion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                         a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Utility
        Calculate soft repuslion contribution given temperature, volume and mol numbers.
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

        optional_flags = [a_t, a_v, a_n,
                          a_tt, a_vv, a_tv,
                          a_tn, a_vn, a_nn]
        optional_arrayshapes = [(0,), (0,), (len(n),),
                               (0,), (0,), (0,),
                               (len(n),), (len(n),), (len(n), len(n))]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        a_t_c, a_v_c, a_n_c, a_tt_c, a_vv_c, a_tv_c, a_tn_c, a_vn_c, a_nn_c = optional_ptrs

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
                                   a_tv_c,
                                   a_vv_c,
                                   a_tn_c,
                                   a_vn_c,
                                   a_nn_c)

        return_tuple = (a_c.value, )
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

    def a_hard_sphere(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                      a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Utility
        Calculate hard-sphere contribution given temperature, volume and mol numbers.
        a = A_hs/(nRT)
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

        optional_flags = [a_t, a_v, a_n,
                          a_tt, a_vv, a_tv,
                          a_tn, a_vn, a_nn]
        optional_arrayshapes = [(0,), (0,), (len(n),),
                               (0,), (0,), (0,),
                               (len(n),), (len(n),), (len(n), len(n))]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        a_t_c, a_v_c, a_n_c, a_tt_c, a_vv_c, a_tv_c, a_tn_c, a_vn_c, a_nn_c = optional_ptrs

        self.s_calc_saft_hard_sphere.argtypes = [POINTER(c_double),
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

        self.s_calc_saft_hard_sphere.restype = None

        self.s_calc_saft_hard_sphere(byref(temp_c),
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
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

    def a_chain(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None,
                     a_tv=None, a_tn=None, a_vn=None, a_nn=None):
        """Utility
        Calculate chain contribution given temperature, volume and mol numbers. $a = A_{chain}/(nRT)$

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

        optional_flags = [a_t, a_v, a_n,
                          a_tt, a_vv, a_tv,
                          a_tn, a_vn, a_nn]
        optional_arrayshapes = [(0,), (0,), (len(n),),
                               (0,), (0,), (0,),
                               (len(n),), (len(n),), (len(n), len(n))]
        optional_ptrs = utils.get_optional_pointers(optional_flags, optional_arrayshapes)
        a_t_c, a_v_c, a_n_c, a_tt_c, a_vv_c, a_tv_c, a_tn_c, a_vn_c, a_nn_c = optional_ptrs

        self.s_calc_saft_chain.argtypes = [POINTER(c_double) for _ in range(13)]
        self.s_calc_saft_chain.restype = None
        self.s_calc_saft_chain(byref(temp_c), byref(v_c), n_c, byref(a_c),
                                    a_t_c, a_v_c, a_n_c,
                                    a_tt_c, a_tv_c, a_vv_c, a_tn_c, a_vn_c, a_nn_c)

        return_tuple = (a_c.value, )
        return_tuple = utils.fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes)
        return return_tuple

    def de_broglie_wavelength(self, c, temp):
        """Utility
        Calculate de Broglie wavelength

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

        self.s_de_broglie_wavelength.argtypes = [POINTER(c_int),
                                                 POINTER(c_double),
                                                 POINTER(c_double)]

        self.s_de_broglie_wavelength.restype = None

        self.s_de_broglie_wavelength(byref(c_c),
                                     byref(temp_c),
                                     byref(lambda_c))

        return lambda_c.value

    def print_saft_parameters(self, c):
        """Utility
        Print saft parameters for component c

        Args:
            c (int): Component index (FORTRAN)

        """
        name = self.get_comp_name(c)
        print(f"{name}:")
        print(f"Segments: {self.m[c-1]}")
        print(f"sigma: {self.sigma[c-1]}")
        print(f"eps div kB: {self.eps_div_kb[c-1]}")

    def print_saft_binary_report(self,):
        """Utility
        Print report of SAFT parameters for binary mixture
        """
        self.activate()
        self.s_print_binary_mix_report()

    def potential(self, ic, jc, r, temp):
        """Utility
        Get potential energy divided by Boltzmann constant as a function of r

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
        """Utility
        Adjust mass in order to get specified de Boer parameter

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
        """Utility
        Get de Boer parameter

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

    def set_pure_assoc_param(self, ic, eps_assoc, beta_assoc):
        """Utility
        Set pure association parameters

        Args:
            ic (int): Component index
            eps_assoc (float): Association energy (J/mol).
            beta_assoc (float): Association volume (-)
        """
        self.activate()
        ic_c = c_int(ic)
        eps_assoc_c = c_double(eps_assoc)
        beta_assoc_c = c_double(beta_assoc)
        self.s_set_active_assoc_params.argtypes = [POINTER(c_int),
                                                   POINTER(c_double),
                                                   POINTER(c_double)]

        self.s_set_active_assoc_params.restype = None

        self.s_set_active_assoc_params(byref(ic_c),
                                              byref(eps_assoc_c),
                                              byref(beta_assoc_c))

    def get_pure_assoc_param(self, ic):
        """Utility
        Set pure association parameters

        Args:
            ic (int): Component index
        Results:
            eps_assoc (float): Association energy (J/mol).
            beta_assoc (float): Association volume (-)
        """
        self.activate()
        ic_c = c_int(ic)
        eps_assoc_c = c_double(0.0)
        beta_assoc_c = c_double(0.0)
        self.s_set_active_assoc_params.argtypes = [POINTER(c_int),
                                                   POINTER(c_double),
                                                   POINTER(c_double)]

        self.s_get_active_assoc_params.restype = None

        self.s_get_active_assoc_params(byref(ic_c),
                                       byref(eps_assoc_c),
                                       byref(beta_assoc_c))
        return eps_assoc_c.value, beta_assoc_c.value

    def get_n_assoc_sites(self):
        """Utility
        Get number of association sites

        Results:
            n_assoc_sites (int): Number of association sites.
        """
        self.activate()

        self.s_set_active_assoc_params.argtypes = []

        self.s_get_active_assoc_params.restype = c_int

        n_assoc_sites = self.s_get_n_assoc_sites()

        return n_assoc_sites

    def sigma_ij(self, i, j):
        """Utility
        Get size parameter for i-j interaction

        Args:
            i (int): Component index (FORTRAN)
            j (int): Component index (FORTRAN)
        Results:
            sigma_ij (float): Size paramater (m)

        """
        self.activate()
        i_c = c_int(i)
        j_c = c_int(j)
        sigma_ij_c = c_double(0.0)

        self.s_sigma_ij.argtypes = [POINTER(c_int),
                                    POINTER(c_int),
                                    POINTER(c_double)]

        self.s_sigma_ij.restype = None

        self.s_sigma_ij(byref(i_c),
                        byref(j_c),
                        byref(sigma_ij_c))
        return sigma_ij_c.value

    def epsilon_ij(self, i, j):
        """Utility
        Well depth divided by Boltzmann constant for i-j interaction

        Args:
            i (int): Component index (FORTRAN)
            j (int): Component index (FORTRAN)
        Results:
            epsilon_ij (float): Well depth divided by Boltzmann constant (K)

        """
        self.activate()
        i_c = c_int(i)
        j_c = c_int(j)
        epsilon_ij_c = c_double(0.0)

        self.s_epsilon_ij.argtypes = [POINTER(c_int),
                                      POINTER(c_int),
                                      POINTER(c_double)]

        self.s_epsilon_ij.restype = None

        self.s_epsilon_ij(byref(i_c),
                          byref(j_c),
                          byref(epsilon_ij_c))
        return epsilon_ij_c.value

    def sigma_eff_ij(self, i, j, temperature):
        """Utility
        Get effective size parameter for i-j interaction for Feynman-Hibbs corrected Mie potentials. For classical
        (not quantum-corrected models), returns the sigma parameter.

        Args:
            i (int): Component index (FORTRAN)
            j (int): Component index (FORTRAN)
            temperature (float): Temperature (K)
        Results:
            sigma_ij (float): Size paramater (m)

        """
        self.activate()
        i_c = c_int(i)
        j_c = c_int(j)
        temperature_c = c_double(temperature)
        sigma_ij_c = c_double(0.0)

        self.s_sigma_eff_ij.argtypes = [POINTER(c_int),
                                        POINTER(c_int),
                                        POINTER(c_double),
                                        POINTER(c_double)]

        self.s_sigma_eff_ij.restype = None

        self.s_sigma_eff_ij(byref(i_c),
                            byref(j_c),
                            byref(temperature_c),
                            byref(sigma_ij_c))
        return sigma_ij_c.value

    def epsilon_eff_ij(self, i, j, temperature):
        """Utility
        Effective well depth divided by Boltzmann constant for i-j interaction for Feynman-Hibbs corrected Mie potentials. For classical
        (not quantum-corrected models), returns the sigma parameter.

        Args:
            i (int): Component index (FORTRAN)
            j (int): Component index (FORTRAN)
            temperature (float): Temperature (K)
        Results:
            epsilon_ij (float): Effective well depth divided by Boltzmann constant (K)

        """
        self.activate()
        i_c = c_int(i)
        j_c = c_int(j)
        temperature_c = c_double(temperature)
        epsilon_ij_c = c_double(0.0)

        self.s_epsilon_eff_ij.argtypes = [POINTER(c_int),
                                          POINTER(c_int),
                                          POINTER(c_double),
                                          POINTER(c_double)]

        self.s_epsilon_eff_ij.restype = None

        self.s_epsilon_eff_ij(byref(i_c),
                              byref(j_c),
                              byref(temperature_c),
                              byref(epsilon_ij_c))
        return epsilon_ij_c.value

    def alpha(self, temperature):
        """Utility
        Get dimensionless van der Waals energy

        Args:
            temperature (float): Temperature (K)

        Returns:
            alpha (ndarray): Dimensionless van der Waals energy (-)

        """
        self.activate()
        alpha_c = (c_double * (self.nc**2))(0.0)
        temperature_c = c_double(temperature)

        self.s_alpha.argtypes = [POINTER(c_double),
                                 POINTER(c_double)]

        self.s_alpha.restype = None

        self.s_alpha(byref(temperature_c),
                     alpha_c)

        return np.array(alpha_c).reshape((self.nc, self.nc), order='F').\
            ravel(order="C").reshape((self.nc,self.nc))

    def truncation_correction(self, enable_truncation_correction, enable_shift_correction, reduced_radius_cut=3.5):
        """Utility
        Enable/disable truncation corrections

        Args:
            enable_truncation_correction (bool): Enable long range truncation correction
            enable_shift_correction (bool): Enable potential shift correction
            reduced_radius_cut (float): Reduced length cut-off
        """
        self.activate()
        enable_truncation_correction_c = c_int(1 if enable_truncation_correction else 0)
        enable_shift_correction_c = c_int(1 if enable_shift_correction else 0)
        rr_c = c_double(reduced_radius_cut)

        self.s_truncation_corrections.argtypes = [POINTER(c_int),
                                                  POINTER(c_int),
                                                  POINTER(c_double)]

        self.s_truncation_corrections.restype = None

        self.s_truncation_corrections(byref(enable_truncation_correction_c),
                                      byref(enable_shift_correction_c),
                                      byref(rr_c))

    def test_fmt_compatibility(self):
        """Utility
        Test if model setup is comaptible with the Fundamental Measure Theory (FMT)

        Returns:
            bool: Is model FMT consistent?
            bool: Is non-additive hard-sphere term used?
        """
        self.activate()
        is_fmt_consistent_c = c_int(0)
        na_enabled_c = c_int(0)

        self.s_test_fmt_compatibility.argtypes = [POINTER(c_int),
                                                  POINTER(c_int)]

        self.s_test_fmt_compatibility.restype = None

        self.s_test_fmt_compatibility(byref(is_fmt_consistent_c),
                                      byref(na_enabled_c))

        return  is_fmt_consistent_c.value == 1, na_enabled_c.value == 1

    def calc_bmcsl_gij_fmt(self, n_alpha, mu_ij, calc_g_ij_n=False, mu_ij_T=None):
        """Utility
        Calculate g_ij at contact according to Yu and Wu: 10.1063/1.1463435

        Args:
            n_alpha (np.ndarray): Weighted densities (n0, n1, n2, n3, nV1, nV2)
            mu_ij (float): mu_ij = d_i*d_j/(d_i+d_j)
            mu_ij_T (float): Temperature differential of mu_ij

        Returns:
            float: g_ij
            np.ndarray: g_ij_n
            float: g_ij_T
        """
        self.activate()
        n_alpha_c = (c_double * len(n_alpha))(*n_alpha)
        mu_ij_c = c_double(mu_ij)
        mu_ij_T_c = c_double(mu_ij_T if isinstance(mu_ij_T, float) else 0.0)
        g_ij_c = c_double(0.0)
        g_ij_n_c = (c_double * len(n_alpha))(0.0) if calc_g_ij_n else POINTER(c_double)()
        g_ij_T_c = POINTER(c_double)(c_double(0.0)) if isinstance(mu_ij_T, float) else POINTER(c_double)()

        self.s_calc_bmcsl_gij_fmt.argtypes = [POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double)]

        self.s_calc_bmcsl_gij_fmt.restype = None

        self.s_calc_bmcsl_gij_fmt(n_alpha_c,
                                  byref(mu_ij_c),
                                  byref(mu_ij_T_c),
                                  byref(g_ij_c),
                                  g_ij_n_c,
                                  g_ij_T_c)

        return_tuple = (g_ij_c.value, )
        if calc_g_ij_n:
            return_tuple += (np.array(g_ij_n_c), )
        if isinstance(mu_ij_T, float):
            return_tuple += (g_ij_T_c[0], )

        return  return_tuple

    def fmt_energy_density(self, n_alpha, phi_n=False, phi_nn=False, fmt_model="WB"):
        """Utility
        Calculate FMT reduced energy density

        Args:
            n_alpha (np.ndarray): Weighted densities (n0, n1, n2, n3, nV1, nV2) for the entire grid
            phi_n (bool): Calculate first order differetnials?
            phi_nn (bool): Calculate second order differetnials?
            fmt_model (str): FMT model (RF (Rosenfeld), WB (White Bear), WBII (White Bear Mark II))

        Returns:
            np.ndarray: phi
            np.ndarray: phi_n
            np.ndarray: phi_nn
        """
        self.activate()
        n_grid, nv = np.shape(n_alpha)
        n_grid_c = c_int(n_grid)
        nv_c = c_int(nv)
        n_alpha_c = (c_double * (n_grid*nv))(*n_alpha.ravel(order='F'))
        if fmt_model == "RF":
            fmt_model_c = c_int(1)
        elif fmt_model == "WB":
            fmt_model_c = c_int(2)
        elif fmt_model == "WBII":
            fmt_model_c = c_int(3)
        else:
            raise ValueError("Wrong FMT model")

        phi_c = (c_double * n_grid)(0.0)
        phi_n_c = (c_double * (n_grid*nv))(0.0) if phi_n else POINTER(c_double)()
        phi_nn_c = (c_double * (n_grid*nv*nv))(0.0) if phi_nn else POINTER(c_double)()

        self.s_fmt_energy_density.argtypes = [POINTER(c_int),
                                              POINTER(c_int),
                                              POINTER(c_int),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double)]

        self.s_fmt_energy_density.restype = None

        self.s_fmt_energy_density(byref(fmt_model_c),
                                  byref(n_grid_c),
                                  byref(nv_c),
                                  n_alpha_c,
                                  phi_c,
                                  phi_n_c,
                                  phi_nn_c)

        return_tuple = (np.array(phi_c), )
        if phi_n:
            return_tuple += (np.array(phi_n_c).reshape((n_grid, nv), order='F').ravel(order="C").reshape((n_grid, nv)), )
        if phi_nn:
            return_tuple += (np.array(phi_nn_c).reshape((n_grid, nv, nv), order='F').ravel(order="C").reshape((n_grid, nv, nv)), )
        return  return_tuple

    def fres_polar(self, temp, volume, n, qq=True, dd=True, dq=True):
        """Utility
        Calculate reduced Helmholtz energy contribution from polar model

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

    def polar_model_control(self, qq, dd, dq):
        """Utility
        Dictate what terms are included with for polar model

        Args:
            qq (bool): Include quadrupole-quadrupole contribution?
            dd (bool): Include dipole-dipole contribution?
            dq (bool): Include dipole-quadrupole contribution?
        """
        self.activate()
        qq_c = c_int(1 if qq else 0)
        dd_c = c_int(1 if dd else 0)
        dq_c = c_int(1 if dq else 0)

        self.s_multipol_model_control.argtypes = [POINTER(c_int),
                                                  POINTER(c_int),
                                                  POINTER(c_int)]

        self.s_multipol_model_control.restype = None

        self.s_multipol_model_control(byref(qq_c),
                                      byref(dd_c),
                                      byref(dq_c))
