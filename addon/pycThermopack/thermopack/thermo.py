"""
This is the core of the pythod interface to ThermoPack. All equation of state classes on the python side inherit from
the thermo class in this file. Please note that the docstrings of the methods in this file are used to generate
the markdown-documentation found elsewhere (the ThermoTools wiki, etc.). Therefore, new methods that are implemented
must conform to the following style guide for docstrings:

    1 : The first line of the docstring must include a "section name" (i.e. No leading blank line)
    2 : The leading description of the method must follow directly on the line following the "section name"
        (i.e. no intermediate blank line).
    3 : There must be (at least) one blank line following the leading description
    4 : The argument list, and return list, must be preceded by a line containing the word "Args" or "Returns"
    5 : The argument list, and return list, must be written as 'param_name (type) : Description', where the essential
        part is the colon is included as a separator.

    An example is:

        def myfunc(self, p1, p2, p3, p4=None, p5=<something>):
                '''Section name
                Description of what this function does (a kind of header). We can write lots of stuff here
                NOTE the double lineshift here:

                Args:
                     p1 (int) : The lineshift before 'Args' is necessary.
                     p2 (float) : The colons here are also necessary.
                     p3 (bool) : SomethingSomething
                     p4 (Optional, list) : etc.
                     p5 (Optional, <Something>) : This is the last argument
                Returns:
                    (float) : The colon here is also necessary.
                '''
"""
import sys
from ctypes import *
from os import path
import copy
import numpy as np
from . import plotutils, utils, platform_specifics

c_len_type = c_size_t  # c_size_t on GCC > 7 else c_len_type = c_int

class thermo(object):
    """
    Interface to thermopack
    """

    def __init__(self):
        """Internal
        Load libthermopack.(so/dll) and initialize function pointers
        """
        pf_specifics = platform_specifics.get_platform_specifics()
        self.prefix = pf_specifics["prefix"]
        self.module = pf_specifics["module"]
        self.postfix = pf_specifics["postfix"]
        self.postfix_nm = pf_specifics["postfix_no_module"]
        dyn_lib_path = path.join(path.dirname(
            __file__), pf_specifics["dyn_lib"])
        self.tp = cdll.LoadLibrary(dyn_lib_path)

        # Set phase flags
        self.s_get_phase_flags = self.tp.get_phase_flags_c
        self.get_phase_flags()

        # Model control
        self.s_add_eos = getattr(
            self.tp, self.get_export_name("thermopack_var", "add_eos"))
        self.s_delete_eos = getattr(
            self.tp, self.get_export_name("thermopack_var", "delete_eos"))
        self.s_delete_eos.argtypes = [POINTER(c_int)]
        self.s_activate_model = getattr(
            self.tp, self.get_export_name("thermopack_var", "activate_model"))

        # Information
        self.s_get_model_id = getattr(self.tp, self.get_export_name(
            "thermopack_var", "get_eos_identification"))

        # Init methods
        self.eoslibinit_init_thermo = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_thermo"))
        self.s_get_rgas = getattr(
            self.tp, self.get_export_name("thermopack_var", "get_rgas"))
        self.nc = None
        self.s_get_tmin = getattr(
            self.tp, self.get_export_name("thermopack_var", "get_tmin"))
        self.s_set_tmin = getattr(
            self.tp, self.get_export_name("thermopack_var", "set_tmin"))
        self.s_get_tmax = getattr(
            self.tp, self.get_export_name("thermopack_var", "get_tmax"))
        self.s_set_tmax = getattr(
            self.tp, self.get_export_name("thermopack_var", "set_tmax"))
        self.s_get_pmin = getattr(
            self.tp, self.get_export_name("thermopack_var", "get_pmin"))
        self.s_set_pmin = getattr(
            self.tp, self.get_export_name("thermopack_var", "set_pmin"))
        self.s_get_pmax = getattr(
            self.tp, self.get_export_name("thermopack_var", "get_pmax"))
        self.s_set_pmax = getattr(
            self.tp, self.get_export_name("thermopack_var", "set_pmax"))
        self.solideos_solid_init = getattr(
            self.tp, self.get_export_name("solideos", "solid_init"))
        self.eoslibinit_init_volume_translation = getattr(
            self.tp, self.get_export_name("eoslibinit", "init_volume_translation"))
        self.eoslibinit_redefine_critical_parameters = getattr(
            self.tp, self.get_export_name("eoslibinit", "redefine_critical_parameters"))

        # Eos interface
        self.s_eos_specificvolume = getattr(
            self.tp, self.get_export_name("eos", "specificvolume"))
        self.s_eos_zfac = getattr(self.tp, self.get_export_name("eos", "zfac"))
        self.s_eos_thermo = getattr(
            self.tp, self.get_export_name("eos", "thermo"))
        self.s_eos_entropy = getattr(
            self.tp, self.get_export_name("eos", "entropy"))
        self.s_eos_enthalpy = getattr(
            self.tp, self.get_export_name("eos", "enthalpy"))
        self.s_eos_compmoleweight = getattr(
            self.tp, self.get_export_name("eos", "compmoleweight"))
        self.s_eos_getCriticalParam = getattr(
            self.tp, self.get_export_name("eos", "getcriticalparam"))

        # Ideal property interface
        self.s_ideal_idealenthalpysingle = getattr(self.tp, self.get_export_name(
            "ideal", "idealenthalpysingle"))
        self.s_eos_idealentropysingle = getattr(self.tp, self.get_export_name(
            "ideal", "idealentropysingle"))
        self.s_ideal_get_entropy_reference_value = getattr(self.tp, self.get_export_name(
            "ideal", "get_entropy_reference_value"))
        self.s_ideal_set_entropy_reference_value = getattr(self.tp, self.get_export_name(
            "ideal", "set_entropy_reference_value"))
        self.s_ideal_get_enthalpy_reference_value = getattr(self.tp, self.get_export_name(
            "ideal", "get_enthalpy_reference_value"))
        self.s_ideal_set_enthalpy_reference_value = getattr(self.tp, self.get_export_name(
            "ideal", "set_enthalpy_reference_value"))

        # Speed of sound
        self.s_sos_sound_velocity_2ph = getattr(
            self.tp, self.get_export_name("speed_of_sound", "sound_velocity_2ph"))
        self.s_speed_of_sound_tv = getattr(
            self.tp, self.get_export_name("speed_of_sound", "speed_of_sound_tv"))

        # Component info
        self.s_compdata_compindex = getattr(
            self.tp, self.get_export_name("compdata", "comp_index_active"))
        self.s_compdata_compname = getattr(
            self.tp, self.get_export_name("compdata", "comp_name_active"))

        # Flashes
        self.s_set_ph_tolerance = getattr(
            self.tp, self.get_export_name("ph_solver", "setphtolerance"))
        self.s_twophasetpflash = getattr(
            self.tp, self.get_export_name("tp_solver", "twophasetpflash"))
        self.s_psflash_twophase = getattr(
            self.tp, self.get_export_name("ps_solver", "twophasepsflash"))
        #self.tpflash_multiphase = getattr(self.tp, '__mp_tp_solver_MOD_mp_flash_tp')
        self.s_uvflash_twophase = getattr(
            self.tp, self.get_export_name("uv_solver", "twophaseuvflash"))
        self.s_phflash_twophase = getattr(
            self.tp, self.get_export_name("ph_solver", "twophasephflash"))
        #self.s_svflash_twophase = getattr(self.tp, self.get_export_name("sv_solver", "twophasesvflash"))
        self.s_guess_phase = getattr(
            self.tp, self.get_export_name("thermo_utils", "guessphase"))

        # TV interfaces
        self.s_internal_energy_tv = getattr(
            self.tp, self.get_export_name("eostv", "internal_energy_tv"))
        self.s_entropy_tv = getattr(
            self.tp, self.get_export_name("eostv", "entropy_tv"))
        self.s_pressure_tv = getattr(
            self.tp, self.get_export_name("eostv", "pressure"))
        self.s_lnphi_tv = getattr(
            self.tp, self.get_export_name("eostv", "thermo_tv"))
        self.s_enthalpy_tv = getattr(
            self.tp, self.get_export_name("eostv", "enthalpy_tv"))
        self.s_helmholtz_energy = getattr(
            self.tp, self.get_export_name("eostv", "free_energy_tv"))
        self.s_chempot = getattr(self.tp, self.get_export_name(
            "eostv", "chemical_potential_tv"))

        # TVP interfaces
        self.s_entropy_tvp = getattr(
            self.tp, self.get_export_name("eostv", "entropy_tvp"))
        self.s_thermo_tvp = getattr(
            self.tp, self.get_export_name("eostv", "thermo_tvp"))
        self.s_enthalpy_tvp = getattr(
            self.tp, self.get_export_name("eostv", "enthalpy_tvp"))

        # Saturation properties
        self.s_bubble_t = getattr(
            self.tp, self.get_export_name("saturation", "safe_bubt"))
        self.s_bubble_p = getattr(
            self.tp, self.get_export_name("saturation", "safe_bubp"))
        self.s_dew_t = getattr(
            self.tp, self.get_export_name("saturation", "safe_dewt"))
        self.s_dew_p = getattr(
            self.tp, self.get_export_name("saturation", "safe_dewp"))
        self.s_envelope_plot = getattr(
            self.tp, self.get_export_name("saturation_curve", "envelopeplot"))
        self.s_pure_fluid_saturation_wrapper = getattr(
            self.tp, self.get_export_name("saturation_curve",
                                          "pure_fluid_saturation_wrapper"))
        self.s_binary_plot = getattr(
            self.tp, self.get_export_name("binaryplot", "vllebinaryxy"))
        self.s_global_binary_plot = getattr(
            self.tp, self.get_export_name("binaryplot", "global_binary_plot"))
        self.s_get_bp_term = getattr(
            self.tp, self.get_export_name("binaryplot", "get_bp_term"))
        self.s_three_phase_line = getattr(
            self.tp, self.get_export_name("binaryplot", "threephaseline"))
        self.s_solid_envelope_plot = getattr(
            self.tp, self.get_export_name("solid_saturation", "solidenvelopeplot"))
        self.s_melting_pressure_correlation = getattr(
            self.tp, self.get_export_name("solid_saturation", "melting_pressure_correlation"))
        self.s_sublimation_pressure_correlation = getattr(
            self.tp, self.get_export_name("solid_saturation", "sublimation_pressure_correlation"))
        self.s_isotherm = getattr(
            self.tp, self.get_export_name("isolines", "isotherm"))
        self.s_isobar = getattr(
            self.tp, self.get_export_name("isolines", "isobar"))
        self.s_isenthalp = getattr(
            self.tp, self.get_export_name("isolines", "isenthalp"))
        self.s_isentrope = getattr(
            self.tp, self.get_export_name("isolines", "isentrope"))
        self.s_envelope_isentrope_cross = getattr(
            self.tp, self.get_export_name("saturation_curve", "envelope_isentrope_cross"))

        # Stability
        self.s_crit_tv = getattr(
            self.tp, self.get_export_name("critical", "calccriticaltv"))
        self.s_map_stability_limit = getattr(
            self.tp, self.get_export_name("spinodal", "map_stability_limit"))
        self.s_initial_stab_limit_point = getattr(
            self.tp, self.get_export_name("spinodal", "initial_stab_limit_point"))
        self.s_map_meta_isentrope = getattr(
            self.tp, self.get_export_name("spinodal", "map_meta_isentrope"))
        self.s_solve_mu_t = getattr(self.tp, self.get_export_name(
            "mut_solver", "solve_mu_t"))
        self.s_solve_lnf_t = getattr(self.tp, self.get_export_name(
            "mut_solver", "solve_lnf_t"))
        self.s_map_meta_isotherm = getattr(self.tp, self.get_export_name(
            "mut_solver", "map_meta_isotherm"))

        # Virials
        self.s_virial_coeffcients = getattr(
            self.tp, self.get_export_name("eostv", "virial_coefficients"))
        self.s_second_virial_matrix = getattr(
            self.tp, self.get_export_name("eostv", "secondvirialcoeffmatrix"))
        self.s_binary_third_virial_matrix = getattr(
            self.tp, self.get_export_name("eostv", "binarythirdvirialcoeffmatrix"))

        # Joule-Thompson inversion
        self.s_joule_thompson_inversion = getattr(
            self.tp, self.get_export_name("joule_thompson_inversion", "map_jt_inversion"))

        self.add_eos()

    def __del__(self):
        """Internal
        Delete FORTRAN memory allocated for this instance
        """
        self.delete_eos()

    def activate(self):
        """Internal
        Activate this instance of thermopack parameters for calculation
        """
        self.s_activate_model.argtypes = [POINTER(c_int)]
        self.s_activate_model.restype = None
        self.s_activate_model(self.model_index_c)

    def add_eos(self):
        """Internal
        Allocate FORTRAN memory for this class instance
        """
        self.s_add_eos.argtypes = None
        self.s_add_eos.restype = c_int
        self.model_index_c = c_int(self.s_add_eos())

    def delete_eos(self):
        """Internal
        de-allocate FORTRAN memory for this class instance
        """
        self.s_delete_eos.restype = None
        self.s_delete_eos(self.model_index_c)
        self.model_index_c = None

    def get_model_id(self):
        """Internal
        Get model identification

        Returns:
            str: Eos name
        """
        self.activate()

        eosid_len = 40
        eosid_c = c_char_p(b" " * eosid_len)
        eosid_len_c = c_len_type(eosid_len)
        self.s_get_model_id.argtypes = [c_char_p, c_len_type]
        self.s_get_model_id.restype = None
        self.s_get_model_id(eosid_c, eosid_len_c)

        eosid = eosid_c.value.decode('ascii').strip()
        return eosid

    def get_export_name(self, module, method):
        """Internal
        Generate library export name based on module and method name

        Args:
            module (str): Name of module
            method (str): Name of method

        Returns:
            str: Library export name
        """
        if len(module) > 0:
            export_name = self.prefix + module + self.module + method + self.postfix
        else:
            export_name = method + self.postfix_nm
        return export_name

    #################################
    # Init
    #################################

    def init_thermo(self, eos, mixing, alpha, comps, nphases,
                    liq_vap_discr_method=None, csp_eos=None, csp_ref_comp=None,
                    kij_ref="Default", alpha_ref="Default", saft_ref="Default",
                    b_exponent=None, TrendEosForCp=None, cptype=None,
                    silent=None):
        """Internal
        Initialize thermopack

        Args:
            eos (str): Equation of state
            mixing (str): Mixture model for cubic eos
            alpha (str): Alpha formulations for cubic EOS
            comps (string): Comma separated list of components
            nphases (int): Maximum number of phases considered during multi-phase flash calculations
            liq_vap_discr_method (int, optional): Method to discriminate between liquid and vapor in case of an undefined single phase. Defaults to None.
            csp_eos (str, optional): Corrensponding state equation. Defaults to None.
            csp_ref_comp (str, optional): CSP reference component. Defaults to None.
            kij_ref (str, optional): Data set identifiers. Defaults to "Default".
            alpha_ref (str, optional): Data set identifiers. Defaults to "Default".
            saft_ref (str, optional): Data set identifiers. Defaults to "Default".
            b_exponent (float, optional): Exponent used in co-volume mixing. Defaults to None.
            TrendEosForCp (str, optional): Option to init trend for ideal gas properties. Defaults to None.
            cptype (int array, optional): Equation type number for Cp. Defaults to None.
            silent (bool, optional): Supress messages during init?. Defaults to None.
        """
        self.activate()
        self.nc = max(len(comps.split(" ")), len(comps.split(",")))

        null_pointer = POINTER(c_int)()
        eos_c = c_char_p(eos.encode('ascii'))
        eos_len = c_len_type(len(eos))
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_len_type(len(mixing))
        alpha_c = c_char_p(alpha.encode('ascii'))
        alpha_len = c_len_type(len(alpha))
        comp_string_c = c_char_p(comps.encode('ascii'))
        comp_string_len = c_len_type(len(comps))
        nphases_c = c_int(nphases)
        if liq_vap_discr_method is None:
            liq_vap_discr_method_c = null_pointer
        else:
            liq_vap_discr_method_c = POINTER(
                c_int)(c_int(liq_vap_discr_method))
        if csp_eos is None:
            csp_eos_c = c_char_p()
            csp_eos_len = c_len_type(0)
        else:
            csp_eos_c = c_char_p(csp_eos.encode('ascii'))
            csp_eos_len = c_len_type(len(csp_eos))
        if csp_ref_comp is None:
            csp_ref_comp_c = c_char_p()
            csp_ref_comp_len = c_len_type(0)
        else:
            csp_ref_comp_c = c_char_p(csp_ref_comp.encode('ascii'))
            csp_ref_comp_len = c_len_type(len(csp_ref_comp))
        kij_ref_len = c_len_type(len(kij_ref))
        kij_ref_c = c_char_p(kij_ref.encode('ascii'))
        alpha_ref_len = c_len_type(len(alpha_ref))
        alpha_ref_c = c_char_p(alpha_ref.encode('ascii'))
        saft_ref_len = c_len_type(len(saft_ref))
        saft_ref_c = c_char_p(saft_ref.encode('ascii'))
        if b_exponent is None:
            b_exponent_c = POINTER(c_double)()
        else:
            b_exponent_c = POINTER(c_double)(c_double(b_exponent))
        if TrendEosForCp is None:
            TrendEosForCp_c = c_char_p()
            TrendEosForCp_len = c_len_type(0)
        else:
            TrendEosForCp_c = c_char_p(TrendEosForCp.encode('ascii'))
            TrendEosForCp_len = c_len_type(len(TrendEosForCp))
        if cptype is None:
            cptype_c = null_pointer
        else:
            cptype_c = (c_int * self.nc)(*cptype)

        if silent is None:
            silent_c = null_pointer
        else:
            if silent:
                silent_int = 1
            else:
                silent_int = 0
            silent_c = POINTER(c_int)(c_int(silent_int))

        self.eoslibinit_init_thermo.argtypes = [c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                POINTER(c_int),
                                                POINTER(c_int),
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                POINTER(c_double),
                                                c_char_p,
                                                POINTER(c_int),
                                                POINTER(c_int),
                                                c_len_type, c_len_type,
                                                c_len_type, c_len_type,
                                                c_len_type, c_len_type,
                                                c_len_type, c_len_type,
                                                c_len_type, c_len_type]

        self.eoslibinit_init_thermo.restype = None

        self.eoslibinit_init_thermo(eos_c,
                                    mixing_c,
                                    alpha_c,
                                    comp_string_c,
                                    byref(nphases_c),
                                    liq_vap_discr_method_c,
                                    csp_eos_c,
                                    csp_ref_comp_c,
                                    kij_ref_c,
                                    alpha_ref_c,
                                    saft_ref_c,
                                    b_exponent_c,
                                    TrendEosForCp_c,
                                    cptype_c,
                                    silent_c,
                                    eos_len,
                                    mixing_len,
                                    alpha_len,
                                    comp_string_len,
                                    csp_eos_len,
                                    csp_ref_comp_len,
                                    kij_ref_len,
                                    alpha_ref_len,
                                    saft_ref_len,
                                    TrendEosForCp_len)

    def init_peneloux_volume_translation(self, parameter_reference="Default"):
        """Internal
        Initialialize Peneloux volume translations

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

    def redefine_critical_parameters(self, silent=True, Tc_initials=None, vc_initials=None):
        """Utility
        Recalculate critical properties of pure fluids

        Args:
            silent (bool): Ignore warnings? Defaults to True
            Tc_initials (array_like): Initial value for pure fluid critical temperatures (K). Negative values will trigger use of SRK values from data base.
            vc_initials (array_like): Initial value for pure fluid critical volumes (m3/mol). Negative values will trigger use of SRK values from data base.
        """
        self.activate()
        if silent:
            silent_c = c_int(1)
        else:
            silent_c = c_int(0)

        null_pointer = POINTER(c_double)()
        if Tc_initials is None:
            Tc_initials_c = null_pointer
        else:
            Tc_initials_c = (c_double * len(Tc_initials))(*Tc_initials)
        if vc_initials is None:
            vc_initials_c = null_pointer
        else:
            vc_initials_c = (c_double * len(vc_initials))(*vc_initials)

        self.eoslibinit_redefine_critical_parameters.argtypes = [POINTER(c_int),
                                                                 POINTER(
                                                                     c_double),
                                                                 POINTER(c_double)]

        self.eoslibinit_redefine_critical_parameters.restype = None

        self.eoslibinit_redefine_critical_parameters(
            byref(silent_c), Tc_initials_c, vc_initials_c)

    #################################
    # Solids
    #################################

    def init_solid(self, scomp):
        """Internal
        Initialize pure solid

        Args:
            scomp (str): Component name
        """
        self.activate()
        scomp_c = c_char_p(scomp.encode('ascii'))
        scomp_len = c_len_type(len(scomp))
        self.solideos_solid_init.argtypes = [c_char_p, c_len_type]
        self.solideos_solid_init.restype = None
        self.solideos_solid_init(scomp_c, scomp_len)

    #################################
    # Utility
    #################################

    def getcompindex(self, comp):
        """Utility
        Get component index

        Args:
            comp (str): Component name

        Returns:
            int: Component FORTRAN index
        """
        self.activate()
        comp_c = c_char_p(comp.encode('ascii'))
        comp_len = c_len_type(len(comp))
        self.s_compdata_compindex.argtypes = [c_char_p, c_len_type]
        self.s_compdata_compindex.restype = c_int
        idx = self.s_compdata_compindex(comp_c, comp_len)
        return idx

    def get_comp_name(self, index):
        """Utility
        Get component name

        Args:
            index (int): Component FORTRAN index

        Returns:
            comp (str): Component name
        """
        self.activate()
        comp_len = 40
        comp_c = c_char_p(b" " * comp_len)
        comp_len_c = c_len_type(comp_len)
        index_c = c_int(index)
        self.s_compdata_compname.argtypes = [
            POINTER(c_int), c_char_p, c_len_type]
        self.s_compdata_compname.restype = None
        self.s_compdata_compname(byref(index_c), comp_c, comp_len_c)
        compname = comp_c.value.decode('ascii').strip()
        return compname

    def compmoleweight(self, comp):
        """Utility
        Get component mole weight (g/mol)

        Args:
            comp (int): Component FORTRAN index

        Returns:
            float: Component mole weight (g/mol)
        """
        self.activate()
        comp_c = c_int(comp)
        self.s_eos_compmoleweight.argtypes = [POINTER(c_int)]
        self.s_eos_compmoleweight.restype = c_double
        mw_i = self.s_eos_compmoleweight(byref(comp_c))
        return mw_i

    def acentric_factor(self, i):
        '''Utility
        Get acentric factor of component i
        Args:
            i (int) component FORTRAN index
        returns:
            float: acentric factor
        '''
        self.activate()
        comp_c = c_int(i)
        w = c_double(0.0)
        tci = c_double(0.0)
        pci = c_double(0.0)
        vci = c_double(0.0)
        tnbi = c_double(0.0)

        self.s_eos_getCriticalParam.argtypes = [POINTER(c_int),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double)]
        self.s_eos_getCriticalParam.restype = None

        self.s_eos_getCriticalParam(byref(comp_c),
                                    byref(tci),
                                    byref(pci),
                                    byref(w),
                                    byref(vci),
                                    byref(tnbi))

        return w.value

    def get_phase_flags(self):
        """Utility
        Get phase identifiers used by thermopack

        Returns:
            int: Phase int  identifiers
        """
        iTWOPH = c_int()
        iLIQPH = c_int()
        iVAPPH = c_int()
        iMINGIBBSPH = c_int()
        iSINGLEPH = c_int()
        iSOLIDPH = c_int()
        iFAKEPH = c_int()

        self.s_get_phase_flags.argtypes = [POINTER(c_int),
                                           POINTER(c_int),
                                           POINTER(c_int),
                                           POINTER(c_int),
                                           POINTER(c_int),
                                           POINTER(c_int),
                                           POINTER(c_int)]
        self.s_get_phase_flags.restype = None
        self.s_get_phase_flags(byref(iTWOPH),
                               byref(iLIQPH),
                               byref(iVAPPH),
                               byref(iMINGIBBSPH),
                               byref(iSINGLEPH),
                               byref(iSOLIDPH),
                               byref(iFAKEPH))
        self.TWOPH = iTWOPH.value
        self.LIQPH = iLIQPH.value
        self.VAPPH = iVAPPH.value
        self.MINGIBBSPH = iMINGIBBSPH.value
        self.SINGLEPH = iSINGLEPH.value
        self.SOLIDPH = iSOLIDPH.value
        self.FAKEPH = iFAKEPH.value


    def get_phase_type(self, i_phase):
        """Utility
        Get phase type

        Args:
            i_phase (int): Phase flag returned by thermopack

        Returns:
            str: Phase type
        """
        phase_string_list = ["TWO_PHASE", "LIQUID", "VAPOR",
                             "MINIMUM_GIBBS", "SINGLE", "SOLID", "FAKE"]
        return phase_string_list[i_phase]

    @property
    def Rgas(self):
        self.activate()
        self.s_get_rgas.argtypes = []
        self.s_get_rgas.restype = c_double
        rgas = self.s_get_rgas()
        return rgas

    def set_tmin(self, temp):
        """Utility
        Set minimum temperature in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 80 K.

        Args:
            temp (float): Temperature (K)
        """
        if temp is not None:
            self.activate()
            temp_c = c_double(temp)
            self.s_set_tmin.argtypes = [POINTER(c_double)]
            self.s_set_tmin.restype = None
            self.s_set_tmin(byref(temp_c))

    def get_tmin(self):
        """Utility
        Get minimum temperature in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 80 K.

        Returns:
            float: Temperature (K)
        """
        self.activate()
        self.s_get_tmin.argtypes = []
        self.s_get_tmin.restype = c_double
        tmin = self.s_get_tmin()
        return tmin

    def set_tmax(self, temp):
        """Utility
        Set maximum temperature in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 999 K.

        Args:
            temp (float): Temperature (K)
        """
        if temp is not None:
            self.activate()
            temp_c = c_double(temp)
            self.s_set_tmax.argtypes = [POINTER(c_double)]
            self.s_set_tmax.restype = None
            self.s_set_tmax(byref(temp_c))

    def get_tmax(self):
        """Utility
        Get maximum temperature in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 999 K.

        Returns:
            float: Temperature (K)
        """
        self.activate()
        self.s_get_tmax.argtypes = []
        self.s_get_tmax.restype = c_double
        tmax = self.s_get_tmax()
        return tmax

    def set_pmin(self, press):
        """Utility
        Set minimum pressure in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 10 Pa.

        Args:
            press (float): Pressure (Pa)
        """
        if press is not None:
            self.activate()
            press_c = c_double(press)
            self.s_set_pmin.argtypes = [POINTER(c_double)]
            self.s_set_pmin.restype = None
            self.s_set_pmin(byref(press_c))

    def get_pmin(self):
        """Utility
        Get minimum pressure in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 10 Pa.

        Args:
            press (float): Pressure (Pa)
        """
        self.activate()
        self.s_get_pmin.argtypes = []
        self.s_get_pmin.restype = c_double
        pmin = self.s_get_pmin()
        return pmin

    def set_pmax(self, press):
        """Utility
        Set minimum pressure in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 100 MPa.

        Args:
            press (float): Pressure (Pa)
        """
        if press is not None:
            self.activate()
            press_c = c_double(press)
            self.s_set_pmax.argtypes = [POINTER(c_double)]
            self.s_set_pmax.restype = None
            self.s_set_pmax(byref(press_c))

    def get_pmax(self):
        """Utility
        Get minimum pressure in Thermopack. Used to limit search
        domain for numerical solvers. Default value set on init is 100 MPa.

        Args:
            press (float): Pressure (Pa)
        """
        self.activate()
        self.s_get_pmax.argtypes = []
        self.s_get_pmax.restype = c_double
        pmax = self.s_get_pmax()
        return pmax

    #################################
    # Phase properties
    #################################

    def specific_volume(self, temp, press, x, phase, dvdt=None, dvdp=None, dvdn=None):
        """Tp-property
        Calculate single-phase specific volume
        Note that the order of the output match the default order of input for the differentials.
        Note further that dvdt, dvdp and dvdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Molar composition
            phase (int): Calcualte root for specified phase
            dvdt (logical, optional): Calculate molar volume differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dvdp (logical, optional): Calculate molar volume differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dvdn (logical, optional): Calculate molar volume differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            float: Specific volume (m3/mol), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        phase_c = c_int(phase)
        v_c = c_double(0.0)

        if dvdt is None:
            dvdt_c = null_pointer
        else:
            dvdt_c = POINTER(c_double)(c_double(0.0))
        if dvdp is None:
            dvdp_c = null_pointer
        else:
            dvdp_c = POINTER(c_double)(c_double(0.0))
        if dvdn is None:
            dvdn_c = null_pointer
        else:
            dvdn_c = (c_double * len(x))(0.0)

        self.s_eos_specificvolume.argtypes = [POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_int),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double)]

        self.s_eos_specificvolume.restype = None

        self.s_eos_specificvolume(byref(temp_c),
                                  byref(press_c),
                                  x_c,
                                  byref(phase_c),
                                  byref(v_c),
                                  dvdt_c,
                                  dvdp_c,
                                  dvdn_c)
        return_tuple = (v_c.value, )
        if not dvdt is None:
            return_tuple += (dvdt_c[0], )
        if not dvdp is None:
            return_tuple += (dvdp_c[0], )
        if not dvdn is None:
            return_tuple += (np.array(dvdn_c), )

        return return_tuple

    def zfac(self, temp, press, x, phase, dzdt=None, dzdp=None, dzdn=None):
        """Tp-property
        Calculate single-phase compressibility
        Note that the order of the output match the default order of input for the differentials.
        Note further that dzdt, dzdp and dzdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Molar composition
            phase (int): Calcualte root for specified phase
            dzdt (logical, optional): Calculate compressibility differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dzdp (logical, optional): Calculate compressibility differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dzdn (logical, optional): Calculate compressibility differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            float: Compressibility (-), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        phase_c = c_int(phase)
        z_c = c_double(0.0)

        if dzdt is None:
            dzdt_c = null_pointer
        else:
            dzdt_c = POINTER(c_double)(c_double(0.0))
        if dzdp is None:
            dzdp_c = null_pointer
        else:
            dzdp_c = POINTER(c_double)(c_double(0.0))
        if dzdn is None:
            dzdn_c = null_pointer
        else:
            dzdn_c = (c_double * len(x))(0.0)

        self.s_eos_zfac.argtypes = [POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_int),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double)]

        self.s_eos_zfac.restype = None

        self.s_eos_zfac(byref(temp_c),
                        byref(press_c),
                        x_c,
                        byref(phase_c),
                        byref(z_c),
                        dzdt_c,
                        dzdp_c,
                        dzdn_c)
        return_tuple = (z_c.value, )
        if not dzdt is None:
            return_tuple += (dzdt_c[0], )
        if not dzdp is None:
            return_tuple += (dzdp_c[0], )
        if not dzdn is None:
            return_tuple += (np.array(dzdn_c), )

        return return_tuple

    def thermo(self, temp, press, x, phase, dlnfugdt=None, dlnfugdp=None,
               dlnfugdn=None, ophase=None, v=None):
        """Tp-property
        Calculate logarithm of fugacity coefficient given composition,
        temperature and pressure.
        Note that the order of the output match the default order of input for the differentials.
        Note further that dlnfugdt, dlnfugdp, dlnfugdn and ophase only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Molar composition (.)
            phase (int): Calcualte root for specified phase
            dlnfugdt (logical, optional): Calculate fugacity coefficient differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dlnfugdp (logical, optional): Calculate fugacity coefficient differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dlnfugdn (logical, optional): Calculate fugacity coefficient differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.
            ophase (int, optional): Phase flag. Only set when phase=MINGIBBSPH.
            v (float, optional): Specific volume (m3/mol)
        Returns:
            ndarray: fugacity coefficient (-), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()
        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        phase_c = c_int(phase)
        lnfug_c = (c_double * len(x))(0.0)

        if dlnfugdt is None:
            dlnfugdt_c = null_pointer
        else:
            dlnfugdt_c = (c_double * len(x))(0.0)
        if dlnfugdp is None:
            dlnfugdp_c = null_pointer
        else:
            dlnfugdp_c = (c_double * len(x))(0.0)
        if dlnfugdn is None:
            dlnfugdn_c = null_pointer
        else:
            dlnfugdn_c = (c_double * len(x)**2)(0.0)
        if ophase is None:
            ophase_c = POINTER(c_int)()
        else:
            ophase_c = POINTER(c_int)(c_int(0))
        metaExtremum_c = POINTER(c_int)()

        if v is None:
            v_c = null_pointer
        else:
            v_c = POINTER(c_double)(c_double(0.0))

        self.s_eos_thermo.argtypes = [POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_int),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_int),
                                      POINTER(c_int),
                                      POINTER(c_double)]

        self.s_eos_thermo.restype = None

        self.s_eos_thermo(byref(temp_c),
                          byref(press_c),
                          x_c,
                          byref(phase_c),
                          lnfug_c,
                          dlnfugdt_c,
                          dlnfugdp_c,
                          dlnfugdn_c,
                          ophase_c,
                          metaExtremum_c,
                          v_c)

        return_tuple = (np.array(lnfug_c), )
        if not dlnfugdt is None:
            return_tuple += (np.array(dlnfugdt_c), )
        if not dlnfugdp is None:
            return_tuple += (np.array(dlnfugdp_c), )
        if not dlnfugdn is None:
            dlnfugdn_r = np.zeros((len(x), len(x)))
            for i in range(len(x)):
                for j in range(len(x)):
                    dlnfugdn_r[i][j] = dlnfugdn_c[i+j*len(x)]
            return_tuple += (dlnfugdn_r, )
        if not ophase is None:
            return_tuple += (ophase_c[0], )
        if not v is None:
            return_tuple += (v_c[0], )

        return return_tuple

    def enthalpy(self, temp, press, x, phase, dhdt=None, dhdp=None, dhdn=None, residual=False):
        """Tp-property
        Calculate specific single-phase enthalpy
        Note that the order of the output match the default order of input for the differentials.
        Note further that dhdt, dhdp and dhdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Molar composition
            phase (int): Calcualte root for specified phase
            dhdt (logical, optional): Calculate enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dhdp (logical, optional): Calculate enthalpy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dhdn (logical, optional): Calculate enthalpy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.
            residual (logical, optional): Calculate residual enthalpy. Defaults to False.

        Returns:
            float: Specific enthalpy (J/mol), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        phase_c = c_int(phase)
        h_c = c_double(0.0)

        if dhdt is None:
            dhdt_c = null_pointer
        else:
            dhdt_c = POINTER(c_double)(c_double(0.0))
        if dhdp is None:
            dhdp_c = null_pointer
        else:
            dhdp_c = POINTER(c_double)(c_double(0.0))
        if dhdn is None:
            dhdn_c = null_pointer
        else:
            dhdn_c = (c_double * len(x))(0.0)

        if residual:
            residual_c = POINTER(c_int)(c_int(1))
        else:
            residual_c = POINTER(c_int)(c_int(0))

        self.s_eos_enthalpy.argtypes = [POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_int),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_int)]

        self.s_eos_enthalpy.restype = None

        self.s_eos_enthalpy(byref(temp_c),
                            byref(press_c),
                            x_c,
                            byref(phase_c),
                            byref(h_c),
                            dhdt_c,
                            dhdp_c,
                            dhdn_c,
                            residual_c)

        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )
        if not dhdp is None:
            return_tuple += (dhdp_c[0], )
        if not dhdn is None:
            return_tuple += (np.array(dhdn_c), )

        return return_tuple

    def entropy(self, temp, press, x, phase, dsdt=None, dsdp=None, dsdn=None, residual=False):
        """Tp-property
        Calculate specific single-phase entropy
        Note that the order of the output match the default order of input for the differentials.
        Note further that dsdt, dhsp and dsdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Molar composition
            phase (int): Calcualte root for specified phase
            dsdt (logical, optional): Calculate entropy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dsdp (logical, optional): Calculate entropy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dsdn (logical, optional): Calculate entropy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.
            residual (logical, optional): Calculate residual entropy. Defaults to False.

        Returns:
            float: Specific entropy (J/mol/K), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        phase_c = c_int(phase)
        s_c = c_double(0.0)

        if dsdt is None:
            dsdt_c = null_pointer
        else:
            dsdt_c = POINTER(c_double)(c_double(0.0))
        if dsdp is None:
            dsdp_c = null_pointer
        else:
            dsdp_c = POINTER(c_double)(c_double(0.0))
        if dsdn is None:
            dsdn_c = null_pointer
        else:
            dsdn_c = (c_double * len(x))(0.0)

        if residual:
            residual_c = POINTER(c_int)(c_int(1))
        else:
            residual_c = POINTER(c_int)(c_int(0))

        self.s_eos_entropy.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int)]

        self.s_eos_entropy.restype = None

        self.s_eos_entropy(byref(temp_c),
                           byref(press_c),
                           x_c,
                           byref(phase_c),
                           byref(s_c),
                           dsdt_c,
                           dsdp_c,
                           dsdn_c,
                           residual_c)
        return_tuple = (s_c.value, )
        if not dsdt is None:
            return_tuple += (dsdt_c[0], )
        if not dsdp is None:
            return_tuple += (dsdp_c[0], )
        if not dsdn is None:
            return_tuple += (np.array(dsdn_c), )

        return return_tuple

    def idealenthalpysingle(self, temp, j, dhdt=None):
        """Tp-property
        Calculate specific ideal enthalpy
        Note that the order of the output match the default order of input for the differentials.
        Note further that dhdt only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            j (int): Component index (FORTRAN)
            dhdt (logical, optional): Calculate ideal enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

        Returns:
            float: Specific ideal enthalpy (J/mol), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        j_c = c_int(j)
        h_c = c_double(0.0)

        if dhdt is None:
            dhdt_c = null_pointer
        else:
            dhdt_c = POINTER(c_double)(c_double(0.0))

        self.s_ideal_idealenthalpysingle.argtypes = [POINTER(c_double),
                                                     POINTER(c_int),
                                                     POINTER(c_double),
                                                     POINTER(c_double)]

        self.s_ideal_idealenthalpysingle.restype = None

        self.s_ideal_idealenthalpysingle(byref(temp_c),
                                         byref(j_c),
                                         byref(h_c),
                                         dhdt_c)
        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )

        return return_tuple

    def idealentropysingle(self,temp,press,j,dsdt=None,dsdp=None):
        """Tp-property
        Calculate specific ideal entropy
        Note that the order of the output match the default order of input for the differentials.
        Note further that dhdt, and dhdp only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            j (int): Component index (FORTRAN)
            dsdt (logical, optional): Calculate ideal entropy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dsdp (logical, optional): Calculate ideal entropy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

        Returns:
            float: Specific ideal entropy (J/mol/K), and optionally differentials
        """
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        j_c = c_int(j)
        s_c = c_double(0.0)

        if dsdt is None:
            dsdt_c = null_pointer
        else:
            dsdt_c = POINTER(c_double)(c_double(0.0))
        if dsdp is None:
            dsdp_c = null_pointer
        else:
            dsdp_c = POINTER(c_double)(c_double(0.0))

        self.s_eos_idealentropysingle.argtypes = [POINTER( c_double ),
                                                  POINTER( c_double ),
                                                  POINTER( c_int ),
                                                  POINTER( c_double ),
                                                  POINTER( c_double ),
                                                  POINTER( c_double )]

        self.s_eos_idealentropysingle.restype = None

        self.s_eos_idealentropysingle(byref(temp_c),
                                      byref(press_c),
                                      byref(j_c),
                                      byref(s_c),
                                      dsdt_c,
                                      dsdp_c)
        return_tuple = (s_c.value, )
        if not dsdt is None:
            return_tuple += (dsdt_c[0], )
        if not dsdp is None:
            return_tuple += (dsdp_c[0], )

        return return_tuple

    def set_ideal_entropy_reference_value(self, j, s0):
        """Utility
        Set specific ideal entropy reference value

        Args:
            j (integer): Component index
            s0 (float): Ideal entropy reference (J/mol/K)
        """
        self.activate()

        j_c = c_int(j)
        s0_c = c_double(s0)

        self.s_ideal_set_entropy_reference_value.argtypes = [POINTER(c_int),
                                                             POINTER(c_double)]

        self.s_ideal_set_entropy_reference_value.restype = None

        self.s_ideal_set_entropy_reference_value(byref(j_c),
                                                 byref(s0_c))

    def get_ideal_entropy_reference_value(self, j):
        """Utility
        Get specific ideal entropy reference value

        Args:
            j (integer): Component index

        Returns:
            float: Specific ideal entropy (J/mol/K)
        """
        self.activate()

        j_c = c_int(j)
        s0_c = c_double(0.0)

        self.s_ideal_get_entropy_reference_value.argtypes = [POINTER(c_int),
                                                             POINTER(c_double)]

        self.s_ideal_get_entropy_reference_value.restype = None

        self.s_ideal_get_entropy_reference_value(byref(j_c),
                                                 byref(s0_c))

        return s0_c.value

    def set_ideal_enthalpy_reference_value(self, j, h0):
        """Utility
        Set specific ideal enthalpy reference value

        Args:
            j (integer): Component index
            h0 (float): Ideal enthalpy reference (J/mol)
        """
        self.activate()

        j_c = c_int(j)
        h0_c = c_double(h0)

        self.s_ideal_set_enthalpy_reference_value.argtypes = [POINTER(c_int),
                                                              POINTER(c_double)]

        self.s_ideal_set_enthalpy_reference_value.restype = None

        self.s_ideal_set_enthalpy_reference_value(byref(j_c),
                                                  byref(h0_c))

    def get_ideal_enthalpy_reference_value(self, j):
        """Utility
        Get specific ideal enthalpy reference value

        Args:
            j (integer): Component index

        Returns:
            float: Specific ideal enthalpy (J/mol)
        """
        self.activate()

        j_c = c_int(j)
        h0_c = c_double(0.0)

        self.s_ideal_get_enthalpy_reference_value.argtypes = [POINTER(c_int),
                                                              POINTER(c_double)]

        self.s_ideal_get_enthalpy_reference_value.restype = None

        self.s_ideal_get_enthalpy_reference_value(byref(j_c),
                                                  byref(h0_c))

        return h0_c.value

    def speed_of_sound(self, temp, press, x, y, z, betaV, betaL, phase):
        """Tp-property
        Calculate speed of sound for single phase or two phase mixture assuming
        mechanical, thermal and chemical equilibrium.

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (array_like): Liquid molar composition
            y (array_like): Gas molar composition
            z (array_like): Overall molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (int): Calcualte root for specified phase

        Returns:
            float: Speed of sound (m/s)
        """
        self.activate()
        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        y_c = (c_double * len(y))(*y)
        z_c = (c_double * len(z))(*z)
        betaV_c = c_double(betaV)
        betaL_c = c_double(betaL)
        phase_c = c_int(phase)
        ph_c = POINTER(c_int)()

        self.s_sos_sound_velocity_2ph.argtypes = [POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_double),
                                                  POINTER(c_int),
                                                  POINTER(c_int)]

        self.s_sos_sound_velocity_2ph.restype = c_double

        sos = self.s_sos_sound_velocity_2ph(byref(temp_c),
                                            byref(press_c),
                                            x_c,
                                            y_c,
                                            z_c,
                                            byref(betaV_c),
                                            byref(betaL_c),
                                            byref(phase_c),
                                            ph_c)

        return sos

    def speed_of_sound_tv(self, temp, volume, n):
        """Tv-property
        Calculate speed of sound for single phase fluid

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)

        Returns:
            float: Speed of sound (m/s)
        """
        self.activate()
        temp_c = c_double(temp)
        volume_c = c_double(volume)
        n_c = (c_double * len(n))(*n)

        self.s_speed_of_sound_tv.argtypes = [POINTER(c_double),
                                             POINTER(c_double),
                                             POINTER(c_double)]

        self.s_speed_of_sound_tv.restype = c_double

        sos = self.s_speed_of_sound_tv(byref(temp_c),
                                       byref(volume_c),
                                       n_c)

        return sos

    #################################
    # Flash interfaces
    #################################

    def set_ph_tolerance(self, tol):
        """Flash interface
        Set tolerance of isobaric-isentalpic (PH) flash

        Args:
            tol (float): Tolerance
        """
        tol_c = c_double(tol)
        self.s_set_ph_tolerance.argtypes = [POINTER(c_double)]
        self.s_set_ph_tolerance.restype = None
        self.s_set_ph_tolerance(byref(tol_c))

    def two_phase_tpflash(self, temp, press, z):
        """Flash interface
        Do isothermal-isobaric (TP) flash

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            z (array_like): Overall molar composition

        Returns:
            FlashResult : Struct holding the result of the flash (phase fractions, phase compositions and phase identifier)
        """
        self.activate()
        temp_c = c_double(temp)
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.s_twophasetpflash.argtypes = [POINTER(c_double),
                                           POINTER(c_double),
                                           POINTER(c_double),
                                           POINTER(c_double),
                                           POINTER(c_double),
                                           POINTER(c_int),
                                           POINTER(c_double),
                                           POINTER(c_double)]

        self.s_twophasetpflash.restype = None

        self.s_twophasetpflash(byref(temp_c),
                               byref(press_c),
                               z_c,
                               byref(betaV_c),
                               byref(betaL_c),
                               byref(phase_c),
                               x_c,
                               y_c)

        x = np.array(x_c)
        y = np.array(y_c)

        result = utils.FlashResult(z, temp, press, x, y, betaV_c.value, betaL_c.value, phase_c.value, 'Tp')
        return result

    def two_phase_psflash(self, press, z, entropy, temp=None):
        """Flash interface
        Do isentropic-isobaric (SP) flash

        Args:
            press (float): Pressure (Pa)
            z (array_like): Overall molar composition
            entropy (float): Specific entropy (J/mol/K)
            temp (float, optional): Initial guess for temperature (K)

        Returns:
            FlashResult : Struct holding the result of the flash (Temperature, phase fractions,
                            phase compositions and phase identifier)
        """
        self.activate()
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)
        s_c = c_double(entropy)

        if not temp is None:
            temp_c = POINTER(c_double)(c_double(temp))
        else:
            temp_c = POINTER(c_double)(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)
        ierr_c = c_int(0)
        self.s_psflash_twophase.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_int),
                                            POINTER(c_int)]

        self.s_psflash_twophase.restype = None

        self.s_psflash_twophase(temp_c,
                                byref(press_c),
                                z_c,
                                byref(betaV_c),
                                byref(betaL_c),
                                x_c,
                                y_c,
                                byref(s_c),
                                byref(phase_c),
                                byref(ierr_c))

        if ierr_c.value > 0 or ierr_c.value < -1:
            raise Exception("PS flash calclualtion failed")

        x = np.array(x_c)
        y = np.array(y_c)

        result = utils.FlashResult(z, temp_c[0], press, x, y, betaV_c.value, betaL_c.value, phase_c.value, 'pS')
        return result

    def two_phase_phflash(self, press, z, enthalpy, temp=None):
        """Flash interface
        Do isenthalpic-isobaric (HP) flash

        Args:
            press (float): Pressure (Pa)
            z (array_like): Overall molar composition
            enthalpy (float): Specific enthalpy (J/mol)
            temp (float, optional): Initial guess for temperature (K)

        Returns:
            FlashResult : Struct holding the result of the flash (Temperature, phase fractions,
                            phase compositions and phase identifier)
        """
        self.activate()
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)
        h_c = c_double(enthalpy)

        if not temp is None:
            temp_c = POINTER(c_double)(c_double(temp))
        else:
            temp_c = POINTER(c_double)(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)
        ierr_c = c_int(0)

        self.s_phflash_twophase.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_int),
                                            POINTER(c_int)]

        self.s_phflash_twophase.restype = None

        self.s_phflash_twophase(temp_c,
                                byref(press_c),
                                z_c,
                                byref(betaV_c),
                                byref(betaL_c),
                                x_c,
                                y_c,
                                byref(h_c),
                                byref(phase_c),
                                byref(ierr_c))

        if ierr_c.value != 0:
            raise Exception("PH flash calclualtion failed")

        x = np.array(x_c)
        y = np.array(y_c)

        result = utils.FlashResult(z, temp_c[0], press, x, y, betaV_c.value, betaL_c.value, phase_c.value, 'pH')
        return result

    def two_phase_uvflash(self, z, specific_energy, specific_volume, temp=None, press=None):
        """Flash interface
        Do isoenergetic-isochoric (UV) flash

        Args:
            press (float): Pressure (Pa)
            z (array_like): Overall molar composition
            specific_energy (float): Specific energy (J/mol)
            specific_volume (float): Specific volume (m3/mol)
            temp (float, optional): Initial guess for temperature (K)
            press (float, optional): Initial guess for pressure (Pa)

        Returns:
            FlashResult : Struct holding the result of the flash (Temperature, pressure, phase fractions,
                            phase compositions and phase identifier)
        """
        self.activate()
        z_c = (c_double * len(z))(*z)
        e_c = c_double(specific_energy)
        v_c = c_double(specific_volume)

        if not temp is None:
            temp_c = POINTER(c_double)(c_double(temp))
        else:
            temp_c = POINTER(c_double)(c_double(0.0))

        if not press is None:
            press_c = POINTER(c_double)(c_double(press))
        else:
            press_c = POINTER(c_double)(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.s_uvflash_twophase.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_int)]

        self.s_uvflash_twophase(temp_c,
                                press_c,
                                z_c,
                                byref(betaV_c),
                                byref(betaL_c),
                                x_c,
                                y_c,
                                byref(e_c),
                                byref(v_c),
                                byref(phase_c))

        x = np.array(x_c)
        y = np.array(y_c)

        result = utils.FlashResult(z, temp_c[0], press_c[0], x, y, betaV_c.value, betaL_c.value, phase_c.value, 'UV')
        return result

    def guess_phase(self, temp, press, z):
        """Flash interface
        If only one root exsist for the equation of state the phase type can be
        determined from either the psedo-critical volume or a volume ratio to the co-volume

        Args:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)

        Returns:
            int: Phase int (VAPPH or LIQPH)
        """
        self.activate()
        temp_c = c_double(temp)
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)
        null_pointer = POINTER(c_double)()
        temp_comp_c = null_pointer
        press_comp_c = null_pointer
        vb_ratio_c = null_pointer

        self.s_guess_phase.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double)]

        self.s_guess_phase.restype = c_int

        phase = self.s_guess_phase(byref(temp_c),
                                   byref(press_c),
                                   z_c,
                                   temp_comp_c,
                                   press_comp_c,
                                   vb_ratio_c)

        return phase

    #################################
    # Temperature-volume property interfaces
    #################################

    def pressure_tv(self, temp, volume, n, dpdt=None, dpdv=None, dpdn=None, property_flag='IR'):
        """TV-property
        Calculate pressure given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dpdt (No type, optional): Flag to activate calculation. Defaults to None.
            dpdv (No type, optional): Flag to activate calculation. Defaults to None.
            dpdn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (str, optional): Calculate residual ('R'), ideal ('I') or total ('IR') pressure. Defaults to 'IR'.

        Returns:
            float: Pressure (Pa)
            Optionally pressure differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dpdt is None:
            dpdt_c = null_pointer
        else:
            dpdt_c = POINTER(c_double)(c_double(0.0))
        if dpdv is None:
            dpdv_c = null_pointer
        else:
            dpdv_c = POINTER(c_double)(c_double(0.0))
        d2pdv2_c = null_pointer
        if dpdn is None:
            dpdn_c = null_pointer
        else:
            dpdn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_pressure_tv.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int)]

        self.s_pressure_tv.restype = c_double

        P = self.s_pressure_tv(byref(temp_c),
                               byref(v_c),
                               n_c,
                               dpdv_c,
                               dpdt_c,
                               d2pdv2_c,
                               dpdn_c,
                               contribution_c)

        return_tuple = (P, )
        if not dpdt is None:
            return_tuple += (dpdt_c[0], )
        if not dpdv is None:
            return_tuple += (dpdv_c[0], )
        if not dpdn is None:
            return_tuple += (np.array(dpdn_c), )

        return return_tuple

    def internal_energy_tv(self, temp, volume, n, dedt=None, dedv=None,
                           dedn=None, property_flag="IR"):
        """TV-property
        Calculate internal energy given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dedt (No type, optional): Flag to activate calculation. Defaults to None.
            dedv (No type, optional): Flag to activate calculation. Defaults to None.
            dedn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (str, optional): Calculate residual ('R'), ideal ('I') or total ('IR') internal energy. Defaults to 'IR'.

        Returns:
            float: Energy (J)
            Optionally energy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        e_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dedt is None:
            dedt_c = null_pointer
        else:
            dedt_c = POINTER(c_double)(c_double(0.0))
        if dedv is None:
            dedv_c = null_pointer
        else:
            dedv_c = POINTER(c_double)(c_double(0.0))
        if dedn is None:
            dedn_c = null_pointer
        else:
            dedn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_internal_energy_tv.argtypes = [POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_double),
                                              POINTER(c_int)]

        self.s_internal_energy_tv.restype = None

        self.s_internal_energy_tv(byref(temp_c),
                                  byref(v_c),
                                  n_c,
                                  byref(e_c),
                                  dedt_c,
                                  dedv_c,
                                  dedn_c,
                                  contribution_c)

        return_tuple = (e_c.value, )
        if not dedt is None:
            return_tuple += (dedt_c[0], )
        if not dedv is None:
            return_tuple += (dedv_c[0], )
        if not dedn is None:
            return_tuple += (np.array(dedv_c), )

        return return_tuple

    def entropy_tv(self, temp, volume, n, dsdt=None, dsdv=None,
                   dsdn=None, property_flag="IR"):
        """TV-property
        Calculate entropy given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dsdt (No type, optional): Flag to activate calculation. Defaults to None.
            dsdv (No type, optional): Flag to activate calculation. Defaults to None.
            dsdn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

        Returns:
            float: Entropy (J/K)
            Optionally entropy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        s_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dsdt is None:
            dsdt_c = null_pointer
        else:
            dsdt_c = POINTER(c_double)(c_double(0.0))
        if dsdv is None:
            dsdv_c = null_pointer
        else:
            dsdv_c = POINTER(c_double)(c_double(0.0))
        if dsdn is None:
            dsdn_c = null_pointer
        else:
            dsdn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_entropy_tv.argtypes = [POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_int)]

        self.s_entropy_tv.restype = None

        self.s_entropy_tv(byref(temp_c),
                          byref(v_c),
                          n_c,
                          byref(s_c),
                          dsdt_c,
                          dsdv_c,
                          dsdn_c,
                          contribution_c)

        return_tuple = (s_c.value, )
        if not dsdt is None:
            return_tuple += (dsdt_c[0], )
        if not dsdv is None:
            return_tuple += (dsdv_c[0], )
        if not dsdn is None:
            return_tuple += (np.array(dsdn_c), )

        return return_tuple

    def enthalpy_tv(self, temp, volume, n, dhdt=None, dhdv=None,
                    dhdn=None, property_flag="IR"):
        """TV-property
        Calculate enthalpy given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dhdt (No type, optional): Flag to activate calculation. Defaults to None.
            dhdv (No type, optional): Flag to activate calculation. Defaults to None.
            dhdn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

        Returns:
            float: Enthalpy (J)
            Optionally enthalpy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        h_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dhdt is None:
            dhdt_c = null_pointer
        else:
            dhdt_c = POINTER(c_double)(c_double(0.0))
        if dhdv is None:
            dhdv_c = null_pointer
        else:
            dhdv_c = POINTER(c_double)(c_double(0.0))
        if dhdn is None:
            dhdn_c = null_pointer
        else:
            dhdn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_enthalpy_tv.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int)]

        self.s_enthalpy_tv.restype = None

        self.s_enthalpy_tv(byref(temp_c),
                           byref(v_c),
                           n_c,
                           byref(h_c),
                           dhdt_c,
                           dhdv_c,
                           dhdn_c,
                           contribution_c)

        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )
        if not dhdv is None:
            return_tuple += (dhdv_c[0], )
        if not dhdn is None:
            return_tuple += (np.array(dhdn_c), )

        return return_tuple

    def helmholtz_tv(self, temp, volume, n, dadt=None, dadv=None,
                     dadn=None, property_flag="IR"):
        """TV-property
        Calculate Helmholtz energy given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dadt (No type, optional): Flag to activate calculation. Defaults to None.
            dadv (No type, optional): Flag to activate calculation. Defaults to None.
            dadn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.
        Returns:
            float: Helmholtz energy (J)
            Optionally energy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        a_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dadt is None:
            dadt_c = null_pointer
        else:
            dadt_c = POINTER(c_double)(c_double(0.0))
        if dadv is None:
            dadv_c = null_pointer
        else:
            dadv_c = POINTER(c_double)(c_double(0.0))
        if dadn is None:
            dadn_c = null_pointer
        else:
            dadn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_helmholtz_energy.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_int)]

        self.s_helmholtz_energy.restype = None

        self.s_helmholtz_energy(byref(temp_c),
                                byref(v_c),
                                n_c,
                                byref(a_c),
                                dadt_c,
                                dadv_c,
                                dadn_c,
                                contribution_c)

        return_tuple = (a_c.value, )
        if not dadt is None:
            return_tuple += (dadt_c[0], )
        if not dadv is None:
            return_tuple += (dadv_c[0], )
        if not dadn is None:
            return_tuple += (np.array(dadn_c), )

        return return_tuple

    def chemical_potential_tv(self, temp, volume, n, dmudt=None, dmudv=None,
                              dmudn=None, property_flag="IR"):
        """TV-property
        Calculate chemical potential given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dmudt (No type, optional): Flag to activate calculation. Defaults to None.
            dmudv (No type, optional): Flag to activate calculation. Defaults to None.
            dmudn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

        Returns:
            float: Chemical potential (J/mol)
            Optionally chemical potential differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        mu_c = (c_double * len(n))(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dmudt is None:
            dmudt_c = null_pointer
        else:
            dmudt_c = (c_double * len(n))(0.0)
        if dmudv is None:
            dmudv_c = null_pointer
        else:
            dmudv_c = (c_double * len(n))(0.0)
        if dmudn is None:
            dmudn_c = null_pointer
        else:
            dmudn_c = (c_double * len(n)**2)(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_chempot.argtypes = [POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_double),
                                   POINTER(c_int)]

        self.s_chempot.restype = None

        self.s_chempot(byref(temp_c),
                       byref(v_c),
                       n_c,
                       mu_c,
                       dmudt_c,
                       dmudv_c,
                       dmudn_c,
                       contribution_c)

        return_tuple = (np.array(mu_c), )
        if not dmudt is None:
            return_tuple += (np.array(dmudt_c), )
        if not dmudv is None:
            return_tuple += (np.array(dmudv_c), )
        if not dmudn is None:
            dmudn = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    dmudn[i][j] = dmudn_c[i + j*len(n)]
            return_tuple += (np.array(dmudn), )

        return return_tuple

    def fugacity_tv(self, temp, volume, n, dlnphidt=None, dlnphidv=None, dlnphidn=None):
        """TV-property
        Calculate natural logarithm of fugacity given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dlnphidt (No type, optional): Flag to activate calculation. Defaults to None.
            dlnphidv (No type, optional): Flag to activate calculation. Defaults to None.
            dlnphidn (No type, optional): Flag to activate calculation. Defaults to None.

        Returns:
            ndarry: Natural logarithm of fugacity
            Optionally differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        lnphi_c = (c_double * len(n))(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dlnphidt is None:
            dlnphidt_c = null_pointer
        else:
            dlnphidt_c = (c_double * len(n))(0.0)
        if dlnphidv is None:
            dlnphidv_c = null_pointer
        else:
            dlnphidv_c = (c_double * len(n))(0.0)
        if dlnphidn is None:
            dlnphidn_c = null_pointer
        else:
            dlnphidn_c = (c_double * len(n)**2)(0.0)

        self.s_lnphi_tv.argtypes = [POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double)]

        self.s_lnphi_tv.restype = None

        self.s_lnphi_tv(byref(temp_c),
                        byref(v_c),
                        n_c,
                        lnphi_c,
                        dlnphidt_c,
                        dlnphidv_c,
                        dlnphidn_c)

        return_tuple = (np.array(lnphi_c), )
        if not dlnphidt is None:
            return_tuple += (np.array(dlnphidt_c), )
        if not dlnphidv is None:
            return_tuple += (np.array(dlnphidv_c), )
        if not dlnphidn is None:
            dlnphidn = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    dlnphidn[i][j] = dlnphidn_c[i + j*len(n)]
            return_tuple += (dlnphidn, )

        return return_tuple

    #################################
    # Temperature-volume property interfaces evaluating functions as if temperature-pressure
    #################################

    def entropy_tvp(self, temp, volume, n, dsdt=None, dsdp=None,
                    dsdn=None, property_flag="IR"):
        """TVp-property
        Calculate entropy given temperature, pressure and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dsdt (No type, optional): Flag to activate calculation. Defaults to None.
            dsdp (No type, optional): Flag to activate calculation. Defaults to None.
            dsdn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

        Returns:
            float: Entropy (J/K)
            Optionally entropy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        s_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dsdt is None:
            dsdt_c = null_pointer
        else:
            dsdt_c = POINTER(c_double)(c_double(0.0))
        if dsdp is None:
            dsdp_c = null_pointer
        else:
            dsdp_c = POINTER(c_double)(c_double(0.0))
        if dsdn is None:
            dsdn_c = null_pointer
        else:
            dsdn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_entropy_tvp.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int)]

        self.s_entropy_tvp.restype = None

        self.s_entropy_tvp(byref(temp_c),
                           byref(v_c),
                           n_c,
                           byref(s_c),
                           dsdt_c,
                           dsdp_c,
                           dsdn_c,
                           contribution_c)

        return_tuple = (s_c.value, )
        if not dsdt is None:
            return_tuple += (dsdt_c[0], )
        if not dsdp is None:
            return_tuple += (dsdp_c[0], )
        if not dsdn is None:
            return_tuple += (np.array(dsdn_c), )

        return return_tuple

    def enthalpy_tvp(self, temp, volume, n, dhdt=None, dhdp=None, dhdn=None, property_flag="IR"):
        """TVp-property
        Calculate enthalpy given temperature, volume and mol numbers.

        Args:
            temp (float): Temperature (K)
            volume (float): Volume (m3)
            n (array_like): Mol numbers (mol)
            dhdt (No type, optional): Flag to activate calculation. Defaults to None.
            dhdp (No type, optional): Flag to activate calculation. Defaults to None.
            dhdn (No type, optional): Flag to activate calculation. Defaults to None.
            property_flag (integer, optional): Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

        Returns:
            float: Enthalpy (J)
            Optionally enthalpy differentials
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(volume)
        h_c = c_double(0.0)
        n_c = (c_double * len(n))(*n)

        null_pointer = POINTER(c_double)()
        if dhdt is None:
            dhdt_c = null_pointer
        else:
            dhdt_c = POINTER(c_double)(c_double(0.0))
        if dhdp is None:
            dhdp_c = null_pointer
        else:
            dhdp_c = POINTER(c_double)(c_double(0.0))
        if dhdn is None:
            dhdn_c = null_pointer
        else:
            dhdn_c = (c_double * len(n))(0.0)

        contribution_c = utils.get_contribution_flag(property_flag)

        self.s_enthalpy_tvp.argtypes = [POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_double),
                                        POINTER(c_int)]

        self.s_enthalpy_tvp.restype = None

        self.s_enthalpy_tvp(byref(temp_c),
                            byref(v_c),
                            n_c,
                            byref(h_c),
                            dhdt_c,
                            dhdp_c,
                            dhdn_c,
                            contribution_c)

        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )
        if not dhdp is None:
            return_tuple += (dhdp_c[0], )
        if not dhdn is None:
            return_tuple += (np.array(dhdn_c), )

        return return_tuple

    def thermo_tvp(self, temp, v, n, phase, dlnfugdt=None, dlnfugdp=None,
                   dlnfugdn=None):
        """TVp-property
        Calculate logarithm of fugacity coefficient given molar numbers,
        temperature and pressure.
        Note that the order of the output match the default order of input for the differentials.
        Note further that dlnfugdt, dlnfugdp, dlnfugdn and ophase only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            v (float): Volume (m3)
            n (array_like): Molar numbers (mol)
            dlnfugdt (logical, optional): Calculate fugacity coefficient differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dlnfugdp (logical, optional): Calculate fugacity coefficient differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dlnfugdn (logical, optional): Calculate fugacity coefficient differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.
        Returns:
            ndarray: fugacity coefficient (-), and optionally differentials
        """
        self.activate()
        null_pointer = POINTER(c_double)()
        temp_c = c_double(temp)
        vol_c = c_double(v)
        n_c = (c_double * len(n))(*n)
        lnfug_c = (c_double * len(n))(0.0)

        if dlnfugdt is None:
            dlnfugdt_c = null_pointer
        else:
            dlnfugdt_c = (c_double * len(n))(0.0)
        if dlnfugdp is None:
            dlnfugdp_c = null_pointer
        else:
            dlnfugdp_c = (c_double * len(n))(0.0)
        if dlnfugdn is None:
            dlnfugdn_c = null_pointer
        else:
            dlnfugdn_c = (c_double * len(n)**2)(0.0)

        self.s_thermo_tvp.argtypes = [POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double)]

        self.s_thermo_tvp.restype = None

        self.s_thermo_tvp(byref(temp_c),
                          byref(vol_c),
                          n_c,
                          lnfug_c,
                          dlnfugdt_c,
                          dlnfugdp_c,
                          dlnfugdn_c)

        return_tuple = (np.array(lnfug_c), )
        if not dlnfugdt is None:
            return_tuple += (np.array(dlnfugdt_c), )
        if not dlnfugdp is None:
            return_tuple += (np.array(dlnfugdp_c), )
        if not dlnfugdn is None:
            dlnfugdn_r = np.zeros((len(n), len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    dlnfugdn_r[i][j] = dlnfugdn_c[i+j*len(n)]
            return_tuple += (dlnfugdn_r, )

        return return_tuple

    #################################
    # Saturation interfaces
    #################################

    def bubble_temperature(self, press, z):
        """Saturation interface
        Calculate bubble temperature given pressure and composition

        Args:
            press (float): Pressure (Pa)
            z (array_like): Composition (-)

        Raises:
            Exception: Faild to calculate

        Returns:
            float: Temperature (K)
            ndarray: Incipient phase composition
        """
        self.activate()
        press_c = c_double(press)
        y_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_bubble_t.argtypes = [POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_int)]

        self.s_bubble_t.restype = c_double

        temp = self.s_bubble_t(byref(press_c),
                               z_c,
                               y_c,
                               byref(ierr_c))

        y = np.array(y_c)
        if ierr_c.value != 0:
            raise Exception("bubble_temperature calclualtion failed")
        return temp, y

    def bubble_pressure(self, temp, z):
        """Saturation interface
        Calculate bubble pressure given temperature and composition

        Args:
            temp (float): Temperature (K)
            z (array_like): Composition (-)

        Raises:
            Exception: Faild to calculate

        Returns:
            float: Pressure (Pa)
            ndarray: Incipient phase composition
        """
        self.activate()
        temp_c = c_double(temp)
        y_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_bubble_p.argtypes = [POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_double),
                                    POINTER(c_int)]

        self.s_bubble_p.restype = c_double

        press = self.s_bubble_p(byref(temp_c),
                                z_c,
                                y_c,
                                byref(ierr_c))

        y = np.array(y_c)
        if ierr_c.value != 0:
            raise Exception("bubble_pressure calclualtion failed")
        return press, y

    def dew_temperature(self, press, z):
        """Saturation interface
        Calculate dew temperature given pressure and composition

        Args:
            press (float): Pressure (Pa)
            z (float): Compositon (-)

        Raises:
            Exception: Not able to solve for dew point

        Returns:
            float : Temperature (K)
            ndarray : Incipient phase composition (-)
        """
        self.activate()
        press_c = c_double(press)
        x_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_dew_t.argtypes = [POINTER(c_double),
                                 POINTER(c_double),
                                 POINTER(c_double),
                                 POINTER(c_int)]

        self.s_dew_t.restype = c_double

        temp = self.s_dew_t(byref(press_c),
                            x_c,
                            z_c,
                            byref(ierr_c))

        x = np.array(x_c)
        if ierr_c.value != 0:
            raise Exception("dew_temperature calclualtion failed")
        return temp, x

    def dew_pressure(self, temp, z):
        """Saturation interface
        Calculate dew pressure given temperature and composition

        Args:
            temp (float): Temperature (K)
            z (float): Compositon (-)

        Raises:
            Exception: Not able to solve for dew point

        Returns:
            float : Pressure (Pa)
            ndarray : Incipient phase composition (-)
        """
        self.activate()
        temp_c = c_double(temp)
        x_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_dew_p.argtypes = [POINTER(c_double),
                                 POINTER(c_double),
                                 POINTER(c_double),
                                 POINTER(c_int)]

        self.s_dew_p.restype = c_double

        press = self.s_dew_p(byref(temp_c),
                             x_c,
                             z_c,
                             byref(ierr_c))

        x = np.array(x_c)
        if ierr_c.value != 0:
            raise Exception("bubble_pressure calclualtion failed")
        return press, x

    def get_envelope_twophase(self, initial_pressure, z, maximum_pressure=1.5e7,
                              minimum_temperature=None, step_size_factor=1.0,
                              step_size=None, calc_v=False, initial_temperature=None):
        """Saturation interface
        Get the phase-envelope at a given composition

        Args:
            initial_pressure (float): Start mapping form dew point at initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_temperature (float , optional): Exit on minimum temperature (K). Defaults to None.
            step_size_factor (float , optional): Scale default step size for envelope trace. Defaults to 1.0. Reducing step_size_factor will give a denser grid.
            step_size (float , optional): Set maximum step size for envelope trace. Overrides step_size_factor. Defaults to None.
            calc_v (bool, optional): Calculate specifc volume of saturated phase? Defaults to False
            initial_temperature (bool, optional): Start mapping form dew point at initial temperature.
                                                  Overrides initial pressure. Defaults to None (K).
        Returns:
            ndarray: Temperature values (K)
            ndarray: Pressure values (Pa)
            ndarray (optional, if `calc_v=True`): Specific volume (m3/mol)
        """
        self.activate()
        nmax = 1000
        z_c = (c_double * len(z))(*z)
        temp_c = c_double(initial_temperature if initial_temperature is not None else 0.0)
        press_c = c_double(initial_pressure)
        spec_c = c_int(2 if initial_temperature is not None else 1)
        beta_in_c = c_double(1.0)
        max_press_c = c_double(maximum_pressure)
        nmax_c = c_int(nmax)
        Ta_c = (c_double * nmax)(0.0)
        Pa_c = (c_double * nmax)(0.0)
        Ki_c = (c_double * (nmax*len(z)))(0.0)
        beta_c = (c_double * nmax)(0.0)
        n_c = c_int(0)
        null_pointer = POINTER(c_double)()
        criconden_c = null_pointer
        crit_c = null_pointer
        ds_c = null_pointer if step_size is None else POINTER(c_double)(c_double(step_size))
        step_size_factor_c = POINTER(c_double)(c_double(step_size_factor))
        exitOnTriplePoint_c = POINTER(c_int)()
        tme_c = null_pointer if minimum_temperature is None else POINTER(c_double)(c_double(minimum_temperature))

        self.s_envelope_plot.argtypes = [POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_int),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_int),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_int),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_double),
                                         POINTER(c_int),
                                         POINTER(c_double),
                                         POINTER(c_double)]

        self.s_envelope_plot.restype = None

        self.s_envelope_plot(z_c,
                             byref(temp_c),
                             byref(press_c),
                             byref(spec_c),
                             byref(beta_in_c),
                             byref(max_press_c),
                             byref(nmax_c),
                             Ta_c,
                             Pa_c,
                             Ki_c,
                             beta_c,
                             byref(n_c),
                             criconden_c,
                             crit_c,
                             ds_c,
                             exitOnTriplePoint_c,
                             tme_c,
                             step_size_factor_c)

        t_vals = np.array(Ta_c[0:n_c.value])
        p_vals = np.array(Pa_c[0:n_c.value])

        return_tuple = (t_vals, p_vals)

        if calc_v:
            # Special treatment for single phase
            if np.amax(z) == 1:
                t_vals_single = np.zeros(2*n_c.value)
                p_vals_single = np.zeros(2*n_c.value)
                v_vals_single = np.zeros_like(t_vals_single)
                for i in range(n_c.value):
                    t_vals_single[i] = t_vals[i]
                    t_vals_single[-i-1] = t_vals[i]
                    p_vals_single[i] = p_vals[i]
                    p_vals_single[-i-1] = p_vals[i]
                    v_vals_single[i], = self.specific_volume(
                        t_vals[i], p_vals[i], z, self.VAPPH)
                    v_vals_single[-i-1], = self.specific_volume(
                        t_vals[i], p_vals[i], z, self.LIQPH)
                return_tuple = (t_vals_single, p_vals_single, v_vals_single)
            else:
                v_vals = np.zeros_like(t_vals)
                for i in range(n_c.value):
                    if beta_c[i] > 0.5:
                        phase = self.VAPPH
                    else:
                        phase = self.LIQPH
                    v_vals[i], = self.specific_volume(
                        t_vals[i], p_vals[i], z, phase)
                return_tuple += (v_vals, )

        return return_tuple

    def get_pure_fluid_saturation_curve(self,
                                        initial_pressure,
                                        initial_temperature=None,
                                        i=None,
                                        max_delta_press=0.2e5,
                                        nmax=100,
                                        log_linear_grid=False):
        """Saturation interface
        Get the pure fluid saturation line

        To start mapping from and initial temperature, use:
        get_pure_fluid_saturation_curve(None, initial_temperature=<my_temp>)

        Args:
            initial_pressure (float): Start mapping form dew point at initial pressure (Pa).
            initial_temperature (float, optional): Start mapping form dew point at initial temperature (K). Default None.
            i (int, optional): FORTRAN component index. Default None. Must be given if self.nc > 1.
            max_delta_press (float , optional): Maximum delta pressure betwween points (Pa). Defaults to 0.2e5.
            nmax (int, optional): Maximum number of points on envelope. Defaults to 100.
            log_linear_grid (logical, optional): Use log-linear grid?. Defaults to False.

        Returns:
            ndarray: Temperature values (K)
            ndarray: Pressure values (Pa)
            ndarray: Specific liquid volume (m3/mol)
            ndarray: Specific gas volume (m3/mol)
        """
        self.activate()
        if (initial_pressure is None and initial_temperature is None) or \
           (initial_pressure is not None and initial_temperature is not None):
            raise Exception("One of initial_pressure and initial_temperature must be given")
        if i is None:
            assert self.nc == 1
            z = np.ones(1)
        else:
            z = np.zeros(self.nc)
            z[i-1] = 1.0
        z_c = (c_double * len(z))(*z)
        t_or_p_c = c_double(initial_temperature if initial_pressure is None
                            else initial_pressure)
        start_from_temp_c = c_int(initial_pressure is None)
        nmax_c = c_int(nmax)
        max_delta_press_c = c_double(max_delta_press)
        log_linear_grid_c = c_int(log_linear_grid)
        Ta_c = (c_double * nmax)(0.0)
        Pa_c = (c_double * nmax)(0.0)
        vla_c = (c_double * nmax)(0.0)
        vga_c = (c_double * nmax)(0.0)
        n_c = c_int(0)

        self.s_pure_fluid_saturation_wrapper.argtypes = [POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_int),
                                                         POINTER(c_double),
                                                         POINTER(c_int),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_double),
                                                         POINTER(c_int),
                                                         POINTER(c_int)]

        self.s_pure_fluid_saturation_wrapper.restype = None

        self.s_pure_fluid_saturation_wrapper(z_c,
                                             byref(t_or_p_c),
                                             byref(start_from_temp_c),
                                             byref(max_delta_press_c),
                                             byref(log_linear_grid_c),
                                             Ta_c,
                                             Pa_c,
                                             vla_c,
                                             vga_c,
                                             byref(nmax_c),
                                             byref(n_c))

        t_vals = np.array(Ta_c[0:n_c.value])
        p_vals = np.array(Pa_c[0:n_c.value])
        vl_vals = np.array(vla_c[0:n_c.value])
        vg_vals = np.array(vga_c[0:n_c.value])

        return t_vals, p_vals, vl_vals, vg_vals

    def get_binary_pxy(self,
                       temp,
                       maximum_pressure=1.5e7,
                       minimum_pressure=1.0e5,
                       maximum_dz=0.003,
                       maximum_dlns=0.01):
        """Saturation interface
        Calculate binary three phase envelope

        Args:
            temp (float): Temperature (K)
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0e5.
            maximum_dz (float, optional): Maximum composition step. Defaults to 0.003.
            maximum_dlns (float, optional): Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

        Returns:
            tuple of arrays: LLE, L1VE, L2VE

            LLE : Liquid 1 - Liquid 2 Equilibrium
                LLE[0] -> Liquid 1 composition (mole fraction of component 1)
                LLE[1] -> Liquid 2 composition (mole fraction of component 1)
                LLE[2] -> Pressure [Pa]
            L1VE : Liquid 1 - Vapour Equilibrium
                L1VE[0] -> Bubble line composition (mole fraction of component 1)
                L1VE[1] -> Dew line composition (mole fraction of component 1)
                L1VE[2] -> Pressure [Pa]
            L2VE : Liquid 2 - Vapour Equilibrium
                L2VE[0] -> Bubble line composition (mole fraction of component 1)
                L2VE[1] -> Dew line composition (mole fraction of component 1)
                L2VE[2] -> Pressure [Pa]

            If one or more of the equilibria are not found the corresponding tuple is (None, None, None)
        """
        # Redefinition of module parameter:
        self.activate()
        nmax = 10000
        #c_int.in_dll(self.tp, self.get_export_name("binaryplot", "maxpoints")).value

        temp_c = c_double(temp)
        min_temp_c = c_double(0.0)
        ispec_c = c_int(1)
        press_c = c_double(0.0)
        max_press_c = c_double(maximum_pressure)
        min_press_c = c_double(minimum_pressure)
        dz_max_c = c_double(maximum_dz)
        dlns_max_c = c_double(maximum_dlns)
        filename = "binaryVLLE.dat"
        filename_c = c_char_p(filename.encode('ascii'))
        filename_len = c_len_type(len(filename))
        res_c = (c_double * (nmax*9))(0.0)
        nres_c = (c_int * 3)(0)
        wsf_c = c_int(1)
        ierr_c = c_int(0)

        self.s_binary_plot.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_char_p),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       POINTER(c_int),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       c_len_type]

        self.s_binary_plot.restype = None

        self.s_binary_plot(byref(temp_c),
                           byref(press_c),
                           byref(ispec_c),
                           byref(min_temp_c),
                           byref(max_press_c),
                           byref(dz_max_c),
                           filename_c,
                           byref(dlns_max_c),
                           res_c,
                           nres_c,
                           byref(wsf_c),
                           byref(min_press_c),
                           byref(ierr_c),
                           filename_len)

        if ierr_c.value > 0 or ierr_c.value < -1:
            raise Exception("binary_plot failed")

        nLLE = nres_c[0]
        nL1VE = nres_c[1]
        nL2VE = nres_c[2]

        if nLLE > 0:
            xLLE = np.zeros(nLLE)
            wLLE = np.zeros(nLLE)
            pLLE = np.zeros(nLLE)
            for i in range(nLLE):
                xLLE[i] = res_c[i*9]
                wLLE[i] = res_c[i*9+1]
                pLLE[i] = res_c[i*9+2]
            LLE = (xLLE, wLLE, pLLE)
        else:
            LLE = (None, None, None)

        if nL1VE > 0:
            xL1VE = np.zeros(nL1VE)
            wL1VE = np.zeros(nL1VE)
            pL1VE = np.zeros(nL1VE)
            for i in range(nL1VE):
                xL1VE[i] = res_c[i*9+3]
                wL1VE[i] = res_c[i*9+4]
                pL1VE[i] = res_c[i*9+5]
            L1VE = (xL1VE, wL1VE, pL1VE)
        else:
            L1VE = (None, None, None)

        if nL2VE > 0:
            xL2VE = np.zeros(nL2VE)
            wL2VE = np.zeros(nL2VE)
            pL2VE = np.zeros(nL2VE)
            for i in range(nL2VE):
                xL2VE[i] = res_c[i*9+6]
                wL2VE[i] = res_c[i*9+7]
                pL2VE[i] = res_c[i*9+8]
            L2VE = (xL2VE, wL2VE, pL2VE)
        else:
            L2VE = (None, None, None)

        return LLE, L1VE, L2VE

    def get_binary_txy(self,
                       pressure,
                       minimum_temperature=0.0,
                       maximum_dz=0.003,
                       maximum_dlns=0.005):
        """Saturation interface
        Calculate binary isobaric three phase envelope

        Args:
            pressure (float): Pressure (Pa)
            minimum_temperature (float, optional): Exit on minimum temperature (K).
            maximum_dz (float, optional): Maximum composition step. Defaults to 0.003.
            maximum_dlns (float, optional): Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

        Returns:
            tuple of arrays: LLE, L1VE, L2VE

            LLE : Liquid 1 - Liquid 2 Equilibrium
                LLE[0] -> Liquid 1 composition (mole fraction of component 1)
                LLE[1] -> Liquid 2 composition (mole fraction of component 1)
                LLE[2] -> Temperature [K]
            L1VE : Liquid 1 - Vapour Equilibrium
                L1VE[0] -> Bubble line composition (mole fraction of component 1)
                L1VE[1] -> Dew line composition (mole fraction of component 1)
                L1VE[2] -> Temperature [K]
            L2VE : Liquid 2 - Vapour Equilibrium
                L2VE[0] -> Bubble line composition (mole fraction of component 1)
                L2VE[1] -> Dew line composition (mole fraction of component 1)
                L2VE[2] -> Temperature [K]

            If one or more of the equilibria are not found the corresponding tuple is (None, None, None)
        """
        # Redefinition of module parameter:
        self.activate()
        nmax = 10000
        #c_int.in_dll(self.tp, self.get_export_name("binaryplot", "maxpoints")).value

        temp_c = c_double(0.0)
        min_temp_c = c_double(minimum_temperature)
        ispec_c = c_int(2)
        press_c = c_double(pressure)
        max_press_c = c_double(0.0)
        min_press_c = c_double(0.0)
        dz_max_c = c_double(maximum_dz)
        dlns_max_c = c_double(maximum_dlns)
        filename = "binaryVLLE.dat"
        filename_c = c_char_p(filename.encode('ascii'))
        filename_len = c_len_type(len(filename))
        res_c = (c_double * (nmax*9))(0.0)
        nres_c = (c_int * 3)(0)
        wsf_c = c_int(1)
        ierr_c = c_int(0)

        self.s_binary_plot.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_char_p),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       POINTER(c_int),
                                       POINTER(c_double),
                                       POINTER(c_int),
                                       c_len_type]

        self.s_binary_plot.restype = None

        self.s_binary_plot(byref(temp_c),
                           byref(press_c),
                           byref(ispec_c),
                           byref(min_temp_c),
                           byref(max_press_c),
                           byref(dz_max_c),
                           filename_c,
                           byref(dlns_max_c),
                           res_c,
                           nres_c,
                           byref(wsf_c),
                           byref(min_press_c),
                           byref(ierr_c),
                           filename_len)

        if ierr_c.value > 0 or ierr_c.value < -1:
            raise Exception("binary_plot failed")

        nLLE = nres_c[0]
        nL1VE = nres_c[1]
        nL2VE = nres_c[2]

        if nLLE > 0:
            xLLE = np.zeros(nLLE)
            wLLE = np.zeros(nLLE)
            TLLE = np.zeros(nLLE)
            for i in range(nLLE):
                xLLE[i] = res_c[i*9]
                wLLE[i] = res_c[i*9+1]
                TLLE[i] = res_c[i*9+2]
            LLE = (xLLE, wLLE, TLLE)
        else:
            LLE = (None, None, None)

        if nL1VE > 0:
            xL1VE = np.zeros(nL1VE)
            wL1VE = np.zeros(nL1VE)
            TL1VE = np.zeros(nL1VE)
            for i in range(nL1VE):
                xL1VE[i] = res_c[i*9+3]
                wL1VE[i] = res_c[i*9+4]
                TL1VE[i] = res_c[i*9+5]
            L1VE = (xL1VE, wL1VE, TL1VE)
        else:
            L1VE = (None, None, None)

        if nL2VE > 0:
            xL2VE = np.zeros(nL2VE)
            wL2VE = np.zeros(nL2VE)
            TL2VE = np.zeros(nL2VE)
            for i in range(nL2VE):
                xL2VE[i] = res_c[i*9+6]
                wL2VE[i] = res_c[i*9+7]
                TL2VE[i] = res_c[i*9+8]
            L2VE = (xL2VE, wL2VE, TL2VE)
        else:
            L2VE = (None, None, None)

        return LLE, L1VE, L2VE

    def get_bp_term(self,
                    i_term):
        """Saturation interface
        Get error description for binary plot error

        Args:
            i_term (int): binary plot error identifyer

        Returns:
            str: Error message
        """
        message_len = 50
        message_c = c_char_p(b" " * message_len)
        message_len = c_len_type(message_len)
        i_term_c = c_int(i_term)

        self.s_get_bp_term.argtypes = [POINTER(c_int),
                                       c_char_p,
                                       c_len_type]

        self.s_get_bp_term.restype = None

        self.s_get_bp_term(byref(i_term_c),
                           message_c,
                           message_len)
        message = message_c.value.decode('ascii')
        return message

    def binary_triple_point_pressure(self,
                                     temp,
                                     maximum_pressure=1.5e7,
                                     minimum_pressure=1.0e4):
        """Saturation interface
        Calculate triple point for binary mixture at specified temperature

        Args:
            temp (float): Temperature (K)
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0e4.

        Returns:
            has_triple_point (boolean): Does the mixture have a triple point?
            x (np.ndarray): Liquid 1 composition
            y (np.ndarray): Gas composition
            w (np.ndarray): Liquid 2 composition
            P (float): Pressure (Pa)
        """
        self.activate()
        temp_c = c_double(temp)
        min_temp_c = c_double(0.0)
        ispec_c = c_int(1) # Specify temperature
        has_triple_point_c = c_int(0)
        hasLLE_c = c_int(0)
        press_c = c_double(0.0)
        max_press_c = c_double(maximum_pressure)
        min_press_c = c_double(minimum_pressure)
        x_c = (c_double * self.nc)(0.0)
        y_c = (c_double * self.nc)(0.0)
        w_c = (c_double * self.nc)(0.0)

        self.s_three_phase_line.argtypes = [POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_int),
                                            POINTER(c_int),
                                            POINTER(c_int),
                                            POINTER(c_double),
                                            POINTER(c_double),
                                            POINTER(c_double)]

        self.s_three_phase_line.restype = None

        self.s_three_phase_line(byref(temp_c),
                                byref(press_c),
                                x_c,
                                y_c,
                                w_c,
                                byref(has_triple_point_c),
                                byref(hasLLE_c),
                                byref(ispec_c),
                                byref(min_temp_c),
                                byref(max_press_c),
                                byref(min_press_c))

        result = utils.BinaryTriplePoint(has_triple_point_c.value != 0, np.array(x_c), np.array(y_c), np.array(w_c), press_c.value, temp)
        return result

    def global_binary_plot(self,
                           maximum_pressure=1.5e7,
                           minimum_pressure=1.0e5,
                           minimum_temperature=150.0,
                           maximum_temperature=500.0,
                           include_azeotropes=False):
        """Saturation interface
        Calculate global binary phase envelope

        Args:
            maximum_pressure (float, optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            minimum_pressure (float, optional): Exit on minimum pressure (Pa). Defaults to 1.0e5.
            minimum_temperature (float, optional): Terminate phase line traceing at minimum temperature. Defaults to 150.0 K.
            maximum_temperature (float, optional): Terminate phase line traceing at maximum temperature. Defaults to 500.0 K.
            include_azeotropes (bool, optional): Include azeotropic lines. Defaults to False.

        Returns:
            tuple of arrays
        """
        self.activate()
        max_press_c = c_double(maximum_pressure)
        min_press_c = c_double(minimum_pressure)
        max_temp_c = c_double(maximum_temperature)
        min_temp_c = c_double(minimum_temperature)
        az_bool_c = c_int(1 if include_azeotropes else 0)
        filename = "global_binary.dat"
        filename_c = c_char_p(filename.encode('ascii'))
        filename_len = c_len_type(len(filename))
        i_term_c = c_int(0)

        self.s_global_binary_plot.argtypes = [POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              c_char_p,
                                              POINTER( c_int ),
                                              POINTER( c_double ),
                                              POINTER( c_int ),
                                              c_len_type]

        self.s_global_binary_plot.restype = None

        self.s_global_binary_plot(byref(min_press_c),
                                  byref(max_press_c),
                                  byref(min_temp_c),
                                  filename_c,
                                  byref(i_term_c),
                                  byref(max_temp_c),
                                  byref(az_bool_c),
                                  filename_len)

        if not i_term_c.value == 0:
            message = self.get_bp_term(i_term_c.value)
            print(message)

        # Load file with filename and read into arrays
        return plotutils.get_globa_binary_data(filename)

    def solid_envelope_plot(self, initial_pressure, z, maximum_pressure=1.5e7,
                            minimum_temperature=170.0, calc_esv=False):
        """Saturation interface
        Calculate phase envelope including solid lines

        Args:
            initial_pressure (float): Start mapping from initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Exit on maximum pressure (Pa). Defaults to 1.5e7.
            calc_esv (bool, optional): Calculate specifc volume of saturated phase? Defaults to False

        Returns:
            tuple of arrays
        """
        self.activate()
        z_c = (c_double * len(z))(*z)
        temp_c = c_double(0.0)
        press_c = c_double(initial_pressure)
        max_press_c = c_double(maximum_pressure)
        filename = "solid_envelope.dat"
        filename_c = c_char_p(filename.encode('ascii'))
        filename_len = c_len_type(len(filename))
        i_spec_c = c_int(1)
        esv_bool_c = c_int(1 if calc_esv else 0)

        min_t = self.get_tmin()
        self.set_tmin(minimum_temperature)

        self.s_solid_envelope_plot.argtypes = [POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_int ),
                                               POINTER( c_double ),
                                               c_char_p,
                                               POINTER( c_int ),
                                               c_len_type]

        self.s_solid_envelope_plot.restype = None

        self.s_solid_envelope_plot(z_c,
                                   byref(temp_c),
                                   byref(press_c),
                                   byref(i_spec_c),
                                   byref(max_press_c),
                                   filename_c,
                                   byref(esv_bool_c),
                                   filename_len)

        self.set_tmin(min_t)

        #if .not. i_term_c.value == 0:
        #    message = self.get_bp_term(iTerm)
        #    print(message)

        # Load file with filename and read into lists....
        return plotutils.get_solid_envelope_data(filename)

    def melting_pressure_correlation(self,i,maximum_temperature=None,nmax=100,scale_to_eos=True):
        """Saturation interface
        Calculate melting line form correlation

        Args:
            i (int): component FORTRAN index (first index is 1)
            maximum_temperature (float, optional): Get values up to maximum_temperature. Defaults to correlation limit.
            nmax (int): Number of points in equidistant grid. Defaults to 100.
            scale_to_eos (bool, optional): Scale pressures to match triple point pressure? Defaults to True

        Returns:
            T_melt (ndarray): Melting temperature (K)
            p_melt (ndarray): Melting pressure (Pa)
        """
        self.activate()
        temp_melt_c = (c_double * nmax)(0.0)
        press_melt_c = (c_double * nmax)(0.0)
        temp_max_c = c_double(1.0e10 if maximum_temperature is None else maximum_temperature)
        scale_to_eos_c = c_int(1 if scale_to_eos else 0)
        i_comp_c = c_int(i)
        nmax_c = c_int(nmax)
        ierr_c = c_int(0)

        self.s_melting_pressure_correlation.argtypes = [POINTER( c_double ),
                                                        POINTER( c_int ),
                                                        POINTER( c_int ),
                                                        POINTER( c_int ),
                                                        POINTER( c_double ),
                                                        POINTER( c_double ),
                                                        POINTER( c_int )]

        self.s_melting_pressure_correlation.restype = None

        self.s_melting_pressure_correlation(byref(temp_max_c),
                                            byref(i_comp_c),
                                            byref(scale_to_eos_c),
                                            byref(nmax_c),
                                            temp_melt_c,
                                            press_melt_c,
                                            byref(ierr_c))


        if ierr_c.value != 0:
            raise Exception("Melting line calculation failed")

        return np.array(temp_melt_c), np.array(press_melt_c)

    def sublimation_pressure_correlation(self,i,minimum_temperature=None,nmax=100,scale_to_eos=True):
        """Saturation interface
        Calculate melting line form correlation

        Args:
            i (int): component FORTRAN index (first index is 1)
            minimum_temperature (float, optional): Get values from minimum_temperature. Defaults to correlation limit.
            nmax (int): Number of points in equidistant grid. Defaults to 100.
            scale_to_eos (bool, optional): Scale pressures to match triple point pressure? Defaults to True

        Returns:
            T_subl (ndarray): Sublimation temperature (K)
            p_subl (ndarray): Sublimation pressure (Pa)
        """
        self.activate()
        temp_subl_c = (c_double * nmax)(0.0)
        press_subl_c = (c_double * nmax)(0.0)
        temp_min_c = c_double(0.0 if minimum_temperature is None else minimum_temperature)
        scale_to_eos_c = c_int(1 if scale_to_eos else 0)
        i_comp_c = c_int(i)
        nmax_c = c_int(nmax)
        ierr_c = c_int(0)

        self.s_sublimation_pressure_correlation.argtypes = [POINTER( c_double ),
                                                            POINTER( c_int ),
                                                            POINTER( c_int ),
                                                            POINTER( c_int ),
                                                            POINTER( c_double ),
                                                            POINTER( c_double ),
                                                            POINTER( c_int )]

        self.s_sublimation_pressure_correlation.restype = None

        self.s_sublimation_pressure_correlation(byref(temp_min_c),
                                                byref(i_comp_c),
                                                byref(scale_to_eos_c),
                                                byref(nmax_c),
                                                temp_subl_c,
                                                press_subl_c,
                                                byref(ierr_c))


        if ierr_c.value != 0:
            raise Exception("Sublimation line calculation failed")

        return np.array(temp_subl_c), np.array(press_subl_c)

    def envelope_isentrope_cross(self, entropy, initial_pressure, z, maximum_pressure=1.5e7,
                              minimum_temperature=None, step_size=None, initial_temperature=None):
        """Saturation interface
        Get location where isentrope intercext with isopleth. Trace envelope from
        initial_pressure and look for crossing.

        Args:
            entropy (float): Entropy (J/mol/K).
            initial_pressure (float): Start mapping isopleth form dew point at initial pressure (Pa).
            z (array_like): Composition (-)
            maximum_pressure (float , optional): Stop envelope tracking at maximum pressure (Pa). Defaults to 1.5e7.
            minimum_temperature (float , optional): Exit envelope tracking minimumtemperature (K). Defaults to None.
            step_size (float , optional): Set maximum step size for envelope trace. Defaults to None.
            calc_v (bool, optional): Calculate specifc volume of saturated phase? Defaults to False
            initial_temperature (bool, optional): Start mapping form dew point at initial temperature.
                                                  Overrides initial pressure. Defaults to None (K).
        Returns:
            float: Temperature values (K)
            foat: Pressure values (Pa)
            float: Specific volume (m3/mol)
            int: Phase flag for main phase
            ndarray: Incipient composition (mol/mol)
        """
        self.activate()


        if initial_temperature is not None:
            initial_pressure, x = self.dew_pressure(initial_temperature, z)
        else:
            initial_temperature, x = self.dew_temperature(initial_pressure, z)
        z_c = (c_double * len(z))(*z)
        entropy_c = c_double(entropy)
        temp_c = c_double(initial_temperature)
        press_c = c_double(initial_pressure)
        x_c = (c_double * len(x))(*x)
        y_c = (c_double * len(z))(*z)
        max_press_c = c_double(maximum_pressure)
        Ti_c = c_double(0.0)
        Pi_c = c_double(0.0)
        wi_c = (c_double * len(z))(0.0)
        phase_c = c_int(0)
        ierr_c = c_int(0)
        ds_c = POINTER(c_double)() if step_size is None else POINTER(c_double)(c_double(step_size))

        self.s_envelope_isentrope_cross.argtypes = [POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_int),
                                                    POINTER(c_double),
                                                    POINTER(c_double),
                                                    POINTER(c_int)]

        self.s_envelope_isentrope_cross.restype = c_int

        has_crossing_int = self.s_envelope_isentrope_cross(z_c,
                                                           byref(temp_c),
                                                           byref(press_c),
                                                           x_c,
                                                           y_c,
                                                           byref(max_press_c),
                                                           byref(entropy_c),
                                                           byref(Ti_c),
                                                           byref(Pi_c),
                                                           byref(phase_c),
                                                           wi_c,
                                                           ds_c,
                                                           byref(ierr_c))

        has_crossing = (has_crossing_int == 1 and ierr_c.value == 0)
        if has_crossing:
            Ti = Ti_c.value
            Pi = Pi_c.value
            wi = np.array(wi_c)
            phase = phase_c.value
            vi, = self.specific_volume(Ti, Pi, z, phase)
        else:
            Ti = None
            Pi = None
            wi = None
            phase = None
            vi = None

        return has_crossing, Ti, Pi, vi, wi, phase

    def get_isotherm(self,
                     temp,
                     z,
                     minimum_pressure=1.0e5,
                     maximum_pressure=1.5e7,
                     nmax=100):
        """Isoline
        Get iso-therm at specified temperature

        Args:
            temp (float): Temperature (K)
            z (array_like): Composition (-)
            minimum_pressure (float, optional): Map to minimum pressure. Defaults to 1.0e5. (Pa)
            maximum_pressure (float, optional): Map to maximum pressure. Defaults to 1.5e7. (Pa)
            nmax (int, optional): Maximum number of points on iso-therm. Defaults to 100.

        Returns:
           Multiple numpy arrays.
        """
        self.activate()
        temp_c = c_double(temp)
        minimum_pressure_c = c_double(minimum_pressure)
        maximum_pressure_c = c_double(maximum_pressure)
        z_c = (c_double * len(z))(*z)
        va_c = (c_double * nmax)(0.0)
        pa_c = (c_double * nmax)(0.0)
        sa_c = (c_double * nmax)(0.0)
        ha_c = (c_double * nmax)(0.0)
        nmax_c = c_int(nmax)
        na_c = c_int(0)

        self.s_isotherm.argtypes = [POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int )]

        self.s_isotherm.restype = None

        self.s_isotherm(byref(temp_c),
                        byref(minimum_pressure_c),
                        byref(maximum_pressure_c),
                        z_c,
                        byref(nmax_c),
                        pa_c,
                        va_c,
                        sa_c,
                        ha_c,
                        byref(na_c))

        p_vals = np.array(pa_c[0:na_c.value])
        v_vals = np.array(va_c[0:na_c.value])
        s_vals = np.array(sa_c[0:na_c.value])
        h_vals = np.array(ha_c[0:na_c.value])

        return p_vals, v_vals, s_vals, h_vals

    def get_isobar(self,
                   press,
                   z,
                   minimum_temperature=200.0,
                   maximum_temperature=500.0,
                   nmax=100):
        """Isoline
        Get isobar at specified pressure. Use as
        `T, v, s, h = get_isobar(p, z)`, where `(T, v, s, h)` is the temperature, specific volume, specific entropy and
        specific enthalpy along the isobar with pressure `p` and molar composition `z`.

        Args:
            press (float): Pressure (Pa)
            z (array_like): Composition (-)
            minimum_temperature (float, optional): Minimum temperature. Defaults to 200.0. (K)
            maximum_temperature (float, optional): Maximum temperature. Defaults to 500.0. (K)
            nmax (int, optional): Maximum number of points on iso-bar. Defaults to 100.

        Returns:
            (tuple of arrays) : Corresponding to (temperature, specific volume, specific entropy, specific enthalpy)
            along the isobar.
        """
        self.activate()
        press_c = c_double(press)
        minimum_temperature_c = c_double(minimum_temperature)
        maximum_temperature_c = c_double(maximum_temperature)
        z_c = (c_double * len(z))(*z)
        va_c = (c_double * nmax)(0.0)
        ta_c = (c_double * nmax)(0.0)
        sa_c = (c_double * nmax)(0.0)
        ha_c = (c_double * nmax)(0.0)
        nmax_c = c_int(nmax)
        na_c = c_int(0)

        self.s_isobar.argtypes = [POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_int ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_int )]

        self.s_isobar.restype = None

        self.s_isobar(byref(press_c),
                      byref(minimum_temperature_c),
                      byref(maximum_temperature_c),
                      z_c,
                      byref(nmax_c),
                      ta_c,
                      va_c,
                      sa_c,
                      ha_c,
                      byref(na_c))

        t_vals = np.array(ta_c[0:na_c.value])
        v_vals = np.array(va_c[0:na_c.value])
        s_vals = np.array(sa_c[0:na_c.value])
        h_vals = np.array(ha_c[0:na_c.value])

        return t_vals, v_vals, s_vals, h_vals

    def get_isenthalp(self,
                      enthalpy,
                      z,
                      minimum_pressure=1.0e5,
                      maximum_pressure=1.5e7,
                      minimum_temperature=200.0,
                      maximum_temperature=500.0,
                      nmax=100):
        """Isoline
        Get isenthalpic line at specified enthalpy. Use as
        `T, p, v, s = get_isenthalp(h, z)`, where `(T, p, v, s)` is the temperature, pressure, specific volume and
        specific entropy along the isenthalp with specific enthalpy `h` and molar composition `z`.

        Args:
            enthalpy (float): Enthalpy (J/mol)
            z (array_like): Composition (-)
            minimum_pressure (float, optional): Minimum pressure. Defaults to 1.0e5. (Pa)
            maximum_pressure (float, optional): Maximum pressure. Defaults to 1.5e7. (Pa)
            minimum_temperature (float, optional): Minimum temperature. Defaults to 200.0. (K)
            maximum_temperature (float, optional): Maximum temperature. Defaults to 500.0. (K)
            nmax (int, optional): Maximum number of points on isenthalp. Defaults to 100.

        Returns:
            (tuple of arrays) : Corresponding to (temperature, pressure, specific volume, specific entropy) along the
            isenthalp.
        """
        self.activate()
        enthalpy_c = c_double(enthalpy)
        minimum_pressure_c = c_double(minimum_pressure)
        maximum_pressure_c = c_double(maximum_pressure)
        minimum_temperature_c = c_double(minimum_temperature)
        maximum_temperature_c = c_double(maximum_temperature)
        z_c = (c_double * len(z))(*z)
        va_c = (c_double * nmax)(0.0)
        ta_c = (c_double * nmax)(0.0)
        sa_c = (c_double * nmax)(0.0)
        pa_c = (c_double * nmax)(0.0)
        nmax_c = c_int(nmax)
        na_c = c_int(0)

        self.s_isenthalp.argtypes = [POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int )]

        self.s_isenthalp.restype = None

        self.s_isenthalp(byref(enthalpy_c),
                         byref(minimum_pressure_c),
                         byref(maximum_pressure_c),
                         byref(minimum_temperature_c),
                         byref(maximum_temperature_c),
                         z_c,
                         byref(nmax_c),
                         pa_c,
                         va_c,
                         sa_c,
                         ta_c,
                         byref(na_c))

        t_vals = np.array(ta_c[0:na_c.value])
        v_vals = np.array(va_c[0:na_c.value])
        s_vals = np.array(sa_c[0:na_c.value])
        p_vals = np.array(pa_c[0:na_c.value])

        return t_vals, p_vals, v_vals, s_vals

    def get_isentrope(self,
                      entropy,
                      z,
                      minimum_pressure=1.0e5,
                      maximum_pressure=1.5e7,
                      minimum_temperature=200.0,
                      maximum_temperature=500.0,
                      nmax=100):
        """Isoline
        Get isentrope at specified entropy. Use as
        `T, p, v, h = get_isenthalp(s, z)`, where `(T, p, v, h)` is the temperature, pressure, specific volume and
        specific enthalpy along the isentrope with specific entropy `s` and molar composition `z`.

        Args:
            entropy (float): Entropy (J/mol/K)
            z (array_like): Composition (-)
            minimum_pressure (float, optional): Minimum pressure. Defaults to 1.0e5. (Pa)
            maximum_pressure (float, optional): Maximum pressure. Defaults to 1.5e7. (Pa)
            minimum_temperature (float, optional): Minimum temperature. Defaults to 200.0. (K)
            maximum_temperature (float, optional): Maximum temperature. Defaults to 500.0. (K)
            nmax (int, optional): Maximum number of points on isentrope. Defaults to 100.

        Returns:
            (tuple of arrays) : Corresponding to (temperature, pressure, specific volume, specific enthalpy) along the
            isentrope.
        """
        self.activate()
        entropy_c = c_double(entropy)
        minimum_pressure_c = c_double(minimum_pressure)
        maximum_pressure_c = c_double(maximum_pressure)
        minimum_temperature_c = c_double(minimum_temperature)
        maximum_temperature_c = c_double(maximum_temperature)
        z_c = (c_double * len(z))(*z)
        va_c = (c_double * nmax)(0.0)
        ta_c = (c_double * nmax)(0.0)
        ha_c = (c_double * nmax)(0.0)
        pa_c = (c_double * nmax)(0.0)
        nmax_c = c_int(nmax)
        na_c = c_int(0)

        self.s_isentrope.argtypes = [POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int )]

        self.s_isentrope.restype = None

        self.s_isentrope(byref(entropy_c),
                         byref(minimum_pressure_c),
                         byref(maximum_pressure_c),
                         byref(minimum_temperature_c),
                         byref(maximum_temperature_c),
                         z_c,
                         byref(nmax_c),
                         pa_c,
                         va_c,
                         ha_c,
                         ta_c,
                         byref(na_c))

        t_vals = np.array(ta_c[0:na_c.value])
        v_vals = np.array(va_c[0:na_c.value])
        h_vals = np.array(ha_c[0:na_c.value])
        p_vals = np.array(pa_c[0:na_c.value])

        return t_vals, p_vals, v_vals, h_vals

    #################################
    # Stability interfaces
    #################################

    def critical(self, n, temp=0.0, v=0.0, tol=1.0e-7, v_min=None):
        """Stability interface
        Calculate critical point in variables T and V

        Args:
            n (array_like): Mol numbers (mol)
            temp (float, optional): Initial guess for temperature (K). Defaults to 0.0.
            v (float, optional): Initial guess for volume (m3/mol). Defaults to 0.0.
            tol (float, optional): Error tolerance (-). Defaults to 1.0e-8.
            v_min (float, optional): Minimum volume for search (m3/mol). Defaults to None.

        Raises:
            Exception: Failure to solve for critical point

        Returns:
            float: Temperature (K)
            float: Volume (m3/mol)
            float: Pressure (Pa)
        """
        self.activate()
        temp_c = c_double(temp)
        v_c = c_double(v)
        n_c = (c_double * len(n))(*n)
        ierr_c = c_int(0)
        P_c = c_double(0.0)
        tol_c = c_double(tol)
        v_min_c = POINTER(c_double)() if v_min is None else POINTER(c_double)(c_double(v_min))
        self.s_crit_tv.argtypes = [POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_int ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double )]

        self.s_crit_tv.restype = None

        self.s_crit_tv(byref(temp_c),
                       byref(v_c),
                       n_c,
                       byref(ierr_c),
                       byref(tol_c),
                       v_min_c,
                       byref(P_c))

        if ierr_c.value != 0:
            raise Exception("critical calclualtion failed")

        return temp_c.value, v_c.value, P_c.value

    def critical_temperature(self, i):
        '''Stability interface
        Get critical temperature of component i

        Args:
            i (int): component FORTRAN index (first index is 1)
        returns:
            float: critical temperature (K)
        '''
        self.activate()
        comp_c = c_int(i)

        w = c_double(0.0)
        tci = c_double(0.0)
        pci = c_double(0.0)
        vci = c_double(0.0)
        tnbi = c_double(0.0)

        self.s_eos_getCriticalParam.argtypes = [POINTER(c_int),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double)]
        self.s_eos_getCriticalParam.restype = None

        self.s_eos_getCriticalParam(byref(comp_c),
                                    byref(tci),
                                    byref(pci),
                                    byref(w),
                                    byref(vci),
                                    byref(tnbi))

        return tci.value

    def critical_pressure(self, i):
        '''Stability interface
        Get critical pressure of component i

        Args:
            i (int): component FORTRAN index (first index is 1)
        returns:
            float: critical pressure (Pa)
        '''
        self.activate()
        comp_c = c_int(i)

        w = c_double(0.0)
        tci = c_double(0.0)
        pci = c_double(0.0)
        vci = c_double(0.0)
        tnbi = c_double(0.0)

        self.s_eos_getCriticalParam.argtypes = [POINTER(c_int),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double)]
        self.s_eos_getCriticalParam.restype = None

        self.s_eos_getCriticalParam(byref(comp_c),
                                    byref(tci),
                                    byref(pci),
                                    byref(w),
                                    byref(vci),
                                    byref(tnbi))

        return pci.value

    def critical_volume(self, i):
        '''Stability interface
        Get specific critical volume of component i
        Args:
            i (int) component FORTRAN index
        returns:
            float: specific critical volume
        '''
        self.activate()
        comp_c = c_int(i)
        w = c_double(0.0)
        tci = c_double(0.0)
        pci = c_double(0.0)
        vci = c_double(0.0)
        tnbi = c_double(0.0)

        self.s_eos_getCriticalParam.argtypes = [POINTER(c_int),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double)]
        self.s_eos_getCriticalParam.restype = None

        self.s_eos_getCriticalParam(byref(comp_c),
                                    byref(tci),
                                    byref(pci),
                                    byref(w),
                                    byref(vci),
                                    byref(tnbi))

        return vci.value

    def get_critical_parameters(self, i):
        '''Stability interface
        Get critical temperature, volume and pressure of component i

        Args:
            i (int): component FORTRAN index (first index is 1)
        returns:
            float: critical temperature (K)
            float: critical volume (m3/mol)
            float: critical pressure (Pa)
        '''
        self.activate()
        comp_c = c_int(i)

        w = c_double(0.0)
        tci = c_double(0.0)
        pci = c_double(0.0)
        vci = c_double(0.0)
        tnbi = c_double(0.0)

        self.s_eos_getCriticalParam.argtypes = [POINTER(c_int),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double),
                                                POINTER(c_double)]
        self.s_eos_getCriticalParam.restype = None

        self.s_eos_getCriticalParam(byref(comp_c),
                                    byref(tci),
                                    byref(pci),
                                    byref(w),
                                    byref(vci),
                                    byref(tnbi))

        return tci.value, vci.value, pci.value

    def spinodal(self,
                 z,
                 initial_pressure=1.0e5,
                 initial_liquid_temperature=None,
                 dlnv=None,
                 min_temperature_vapor=None):
        """Stability interface
        Trace spinodal curve

        Args:
            z (array_like): Composition (-)
            initial_pressure (float): Initial pressure (Pa). Defaults to 1.0e5.
            initial_liquid_temperature (float, optional): Initial temperature on liquid spinodal (K).
            dlnv (float, optional): Override step size (-).
            min_vapor_temperature (float, optional): Minimum temperature on vapor spinodal (K).

        Raises:
            Exception: Failure to trace spinodal

        Returns:
            np.ndarray: Temperature (K)
            np.ndarray: Volume (m3/mol)
            np.ndarray: Pressure (Pa)
        """
        self.activate()
        n_max = 1000
        p0_c = c_double(initial_pressure)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)
        n_c = c_int(0)
        vol_c = (c_double * n_max)(0.0)
        press_c = (c_double * n_max)(0.0)
        temp_c = (c_double * n_max)(0.0)

        if min_temperature_vapor is not None:
            t_min = min_temperature_vapor
        else:
            t_min = 0.0
            for i in range(self.nc):
                t_min += z[i]*self.critical_temperature(i+1)
            t_min *= 0.6
        t_min_c = c_double(t_min)

        if dlnv is None:
            dlnv_c = POINTER(c_double)()
        else:
            dlnv_c = POINTER(c_double)(c_double(dlnv))

        if initial_liquid_temperature is None:
            t_liq_start_c = POINTER(c_double)()
        else:
            t_liq_start_c = POINTER(c_double)(c_double(initial_liquid_temperature))

        self.s_map_stability_limit.argtypes = [POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_double ),
                                               POINTER( c_int ),
                                               POINTER( c_int ),
                                               POINTER( c_double ),
                                               POINTER( c_double )]

        self.s_map_stability_limit.restype = None

        self.s_map_stability_limit(byref(p0_c),
                                   z_c,
                                   byref(t_min_c),
                                   temp_c,
                                   press_c,
                                   vol_c,
                                   byref(n_c),
                                   byref(ierr_c),
                                   dlnv_c,
                                   t_liq_start_c)

        if ierr_c.value != 0:
            raise Exception("Spinodial calclualtion failed")

        T = np.array(temp_c[0:n_c.value])
        v = np.array(vol_c[0:n_c.value])
        P = np.array(press_c[0:n_c.value])

        return T,v,P

    def spinodal_point(self,
                       z,
                       pressure,
                       phase,
                       temperature=None):
        """Stability interface
        Solve for spinodal curve point. Not able to solve for points close to critical point.
        Solve for temperature if given, otherwise solve for pressure.

        Args:
            z (array_like): Composition (-)
            pressure (float): Pressure (Pa)
            phase (int): Phase flag (VAPPH/LIQPH)
            temperature (float, optional): Temperature (K). Solve for temperature if given.

        Raises:
            Exception: Failure to solve for spinodal curve point

        Returns:
            float: Temperature (K)
            float: Volume (m3/mol)
        """
        self.activate()
        n_max = 1000
        p0_c = c_double(pressure)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)
        n_c = c_int(0)
        vol_c = c_double(0.0)
        temp_c = c_double(0.0)
        phase_c = c_int(phase)

        if temperature is None:
            t_min_c = POINTER(c_double)()
        else:
            t_min_c = POINTER(c_double)(c_double(temperature))

        self.s_initial_stab_limit_point.argtypes = [POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_int ),
                                                    POINTER( c_int ),
                                                    POINTER( c_double )]

        self.s_initial_stab_limit_point.restype = None

        self.s_initial_stab_limit_point(byref(p0_c),
                                        z_c,
                                        byref(vol_c),
                                        byref(temp_c),
                                        byref(phase_c),
                                        byref(ierr_c),
                                        t_min_c)

        if ierr_c.value != 0:
            raise Exception("Spinodial point calclualtion failed")

        return temp_c.value,vol_c.value

    def map_meta_isentrope(self,
                           z,
                           initial_pressure,
                           entropy,
                           minimum_pressure,
                           n_max=50):
        """Stability interface & Isoline
        Trace isentrope into meta-stable region. Trace from pressure to minimum_pressure

        Args:
            z (array_like): Composition (-)
            initial_pressure (float): Initial pressure (Pa)
            entropy (float): Entropy (J/mol/K).
            minimum_pressure (float): Minimum pressure (Pa).
            n_max (int): Number of points on curve. Default 50.

        Raises:
            Exception: Failure to map isentrope

        Returns:
            np.ndarray: Temperature (K)
            np.ndarray: Volume (m3/mol)
            np.ndarray: Pressure (Pa)
        """
        self.activate()
        initial_pressure_c = c_double(initial_pressure)
        entropy_c = c_double(entropy)
        minimum_pressure_c = c_double(minimum_pressure)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)
        n_c = c_int(n_max)
        vol_c = (c_double * n_max)(0.0)
        press_c = (c_double * n_max)(0.0)
        temp_c = (c_double * n_max)(0.0)

        self.s_map_meta_isentrope.argtypes = [POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_int ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_int )]

        self.s_map_meta_isentrope.restype = None

        self.s_map_meta_isentrope(byref(initial_pressure_c),
                                  byref(entropy_c),
                                  z_c,
                                  byref(minimum_pressure_c),
                                  byref(n_c),
                                  temp_c,
                                  vol_c,
                                  press_c,
                                  byref(ierr_c))

        if ierr_c.value != 0:
            raise Exception("Isentrope mapping into the meta-stable region failed")

        return np.array(temp_c), np.array(vol_c), np.array(press_c)

    def map_meta_isotherm(self,
                          temperature,
                          z,
                          phase,
                          n=50):
        """Stability interface & Isoline
        Trace isotherm from saturation line to spinodal. Solve for phase in
        chemical and thermal equilibrium with a phase defined by z anf phase flag..

        Args:
            temperature (float): Temperature (K)
            z (array_like): Composition (-)
            phase (float): Phase with composition z (LIQPH or VAPPH)
            n (int): Number of points on curve. Default 50.

        Raises:
            Exception: Failure to map isotherm

        Returns:
            np.ndarray: Volume of meta-stable phase (m3/mol)
            np.ndarray: Density (mol/m3) of equilibrium phase in each point, dimension (n,nc).
        """
        self.activate()
        temperature_c = c_double(temperature)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)
        n_c = c_int(n)
        phase_c = c_int(phase)
        vol_c = (c_double * n)(0.0)
        rho_c = (c_double * (n*self.nc))(0.0)

        self.s_map_meta_isotherm.argtypes = [POINTER( c_double ),
                                             POINTER( c_double ),
                                             POINTER( c_int ),
                                             POINTER( c_int ),
                                             POINTER( c_double ),
                                             POINTER( c_double ),
                                             POINTER( c_int )]

        self.s_map_meta_isotherm.restype = None

        self.s_map_meta_isotherm(byref(temperature_c),
                                 z_c,
                                 byref(n_c),
                                 byref(phase_c),
                                 vol_c,
                                 rho_c,
                                 byref(ierr_c))

        if ierr_c.value != 0:
            raise Exception("Isotherm mapping into the meta-stable region failed")

        rho = np.zeros((n, self.nc))
        for i in range(n):
            for j in range(self.nc):
                rho[i][j] = rho_c[i+j*n]

        return np.array(vol_c), rho

    def density_mu_t(self, temp, mu, rho_initial):
        """Stability interface & Other property
        Solve for densities (mu=mu(T,rho)) given temperature and chemical potential.

        Args:
            temp (float): Temperature (K)
            mu (array_like): Flag to activate calculation.
            rho_initial (array_like): Initial guess for component densities (mol/m3).

        Returns:
            rho (array_like): Array of component densities (mol/m3).
        """
        self.activate()
        temp_c = c_double(temp)
        mu_c = (c_double * len(mu))(*mu)
        rho_c = (c_double * len(mu))(*rho_initial)
        ierr_c = c_int(0)
        self.s_solve_mu_t.argtypes = [POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_double),
                                      POINTER(c_int)]

        self.s_solve_mu_t.restype = None

        self.s_solve_mu_t(mu_c,
                          byref(temp_c),
                          rho_c,
                          byref(ierr_c))

        if ierr_c.value != 0:
            raise Exception("mu-T solver failed")

        return np.array(rho_c)

    def density_lnf_t(self, temp, lnf, rho_initial):
        """Stability interface & Other property
        Solve densities (lnf=lnf(T,rho)) given temperature and fugcaity coefficients.

        Args:
            temp (float): Temperature (K)
            lnf (array_like): Logaritm of fugacity coefficients.
            rho_initial (array_like): Initial guess for component densities (mol/m3).

        Returns:
            rho (array_like): Array of component densities (mol/m3).
        """
        self.activate()
        temp_c = c_double(temp)
        lnf_c = (c_double * len(lnf))(*lnf)
        rho_c = (c_double * len(lnf))(*rho_initial)
        ierr_c = c_int(0)
        self.s_solve_lnf_t.argtypes = [POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_double),
                                       POINTER(c_int)]

        self.s_solve_lnf_t.restype = None

        self.s_solve_lnf_t(lnf_c,
                           byref(temp_c),
                           rho_c,
                           byref(ierr_c))

        return np.array(rho_c)

    #################################
    # Virial interfaces
    #################################

    def virial_coeffcients(self, temp, n):
        """Virial interface
        Calculate (composition-dependent) virial coefficients B and C,
        defined as P/RT = rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.

        Args:
            temp (float): Temperature
            n (array_like): Mol numbers (mol)

        Returns:
            float: B (m3/mol)
            float: C (m6/mol2)
        """
        self.activate()
        temp_c = POINTER( c_double )(c_double(temp))
        n_c = (c_double * len(n))(*n)
        B_c = c_double(0.0)
        C_c = c_double(0.0)
        self.s_virial_coeffcients.argtypes = [POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double )]

        self.s_virial_coeffcients.restype = None

        self.s_virial_coeffcients(temp_c,
                                  n_c,
                                  byref(B_c),
                                  byref(C_c))

        return B_c.value, C_c.value

    def second_virial_matrix(self, temp):
        """Virial interface
        Calculate composition-independent virial coefficients B,
        defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.
        Including cross coefficients.

        Args:
            temp (float): Temperature (K)

        Returns:
            ndarray: B - Second virial coefficient matrix (m3/mol)
        """
        self.activate()
        temp_c = POINTER( c_double )(c_double(temp))
        bmat_c = (c_double * self.nc**2)(0.0)

        self.s_second_virial_matrix.argtypes = [POINTER( c_double ),
                                                POINTER( c_double )]

        self.s_second_virial_matrix.restype = None

        self.s_second_virial_matrix(temp_c, bmat_c)

        bmat = np.zeros((self.nc,self.nc))
        for i in range(self.nc):
            for j in range(self.nc):
                bmat[i][j] = bmat_c[i+j*self.nc]

        return bmat

    def binary_third_virial_matrix(self, temp):
        """Virial interface
        Calculate composition-independent virial coefficients C,
        defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.
        Including cross coefficients
        Currently the code only support binary mixtures

        Args:
            temp (float): Temperature (K)

        Returns:
            ndarray: C - Third virial coefficient matrix (m6/mol2)
        """
        self.activate()
        assert self.nc == 2
        temp_c = POINTER( c_double )(c_double(temp))
        cmat_c = (c_double * self.nc**2)(0.0)

        self.s_binary_third_virial_matrix.argtypes = [POINTER( c_double ),
                                                      POINTER( c_double )]

        self.s_binary_third_virial_matrix.restype = None

        self.s_binary_third_virial_matrix(temp_c, cmat_c)

        cmat = np.zeros((self.nc,self.nc))
        for i in range(self.nc):
            for j in range(self.nc):
                cmat[i][j] = cmat_c[i+j*self.nc]

        return cmat

    #################################
    # Joule-Thompson interface
    #################################

    def joule_thompson_inversion(self, z, nmax=1000):
        """Joule-Thompson interface
        Calculate Joule-Thompson inversion curve

        Args:
            z (array like): Compozition
            nmax (int): Array size

        Returns:
            ndarray: temp - Temperature (K)
            ndarray: press - Pressure (Pa)
            ndarray: vol - Volume (m3/mol)
        """
        self.activate()
        z_c = (c_double * len(z))(*z)
        nmax_c = c_int(nmax)
        temp_c = (c_double * nmax)(0.0)
        press_c = (c_double * nmax)(0.0)
        vol_c = (c_double * nmax)(0.0)
        ierr_c = c_int(0)
        n_c = c_int(0)

        self.s_joule_thompson_inversion.argtypes = [POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_double ),
                                                    POINTER( c_int ),
                                                    POINTER( c_int ),
                                                    POINTER( c_int )]

        self.s_joule_thompson_inversion.restype = None

        self.s_joule_thompson_inversion(z_c,
                                        temp_c,
                                        vol_c,
                                        press_c,
                                        byref(nmax_c),
                                        byref(n_c),
                                        byref(ierr_c))


        if ierr_c.value != 0:
            raise Exception("Joule-Thompson inversion curve mapping failed")

        t_vals = np.array(temp_c[0:n_c.value])
        v_vals = np.array(vol_c[0:n_c.value])
        p_vals = np.array(press_c[0:n_c.value])

        return t_vals, p_vals, v_vals
