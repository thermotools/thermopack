
from ctypes import *
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Import platform to detect OS
from sys import platform, exit
# Import os utils
from os import path

# GNU FORTRAN
G_PREFIX = "__"
G_MODULE = "MOD"
G_POSTFIX = ""
# INTEL FORTRAN (x64)
I_PREFIX=""
I_MODULE="mp"
I_POSTFIX="_"

def get_platform_specifics():
    os_id = ""
    prefix = ""
    module = ""
    postfix = ""
    dyn_lib = ""
    if platform == "linux" or platform == "linux2":
        # linux
        # Assuming GNU FORTRAN
        prefix = G_PREFIX
        module = G_MODULE
        postfix = G_POSTFIX
        dyn_lib = "libthermopack.so"
        os_id = "linux"
    elif platform == "darwin":
        # MAC OS X
        print("Thermopack python interface not yet tested for MAC OS")
        exit(1)
    elif platform == "win32":
        # Windows
        # Assuming INTEL FORTRAN
        prefix = I_PREFIX #"_"
        module = I_MODULE
        postfix = I_POSTFIX
        dyn_lib = "thermopack.dll"
        os_id = "win"
    elif platform == "win64":
        # Windows 64-bit
        # Assuming INTEL FORTRAN
        prefix = I_PREFIX
        module = I_MODULE
        postfix = I_POSTFIX
        dyn_lib = "thermopack.dll"
        os_id = "win"
    return prefix, module, postfix, dyn_lib, os_id


class thermopack(object):
    """
    Interface to thermopack
    """
    def __init__(self):
        """
        Load libthermopack.so and initialize function pointers
        """
        self.prefix, self.module, self.postfix, dyn_lib, os_id = get_platform_specifics()
        dyn_lib_path = path.join(path.dirname(__file__), dyn_lib)
        if os_id == "linux":
            self.tp = cdll.LoadLibrary(dyn_lib_path)
        else:
            self.tp = cdll.LoadLibrary("C:\\code\\2020\\thermopack\\MSVStudio\\x64\\Debug\\thermopack.dll")

        self.model_id = None
        # Set phase flags
        self.s_get_phase_flags = self.tp.get_phase_flags_
        self.get_phase_flags()

        # Init methods
        self.eoslibinit_init_thermo = getattr(self.tp, self.get_export_name("eoslibinit", "init_thermo"))
        self.nc = None
        self.Rgas = None
        self.minimum_temperature_c = c_double.in_dll(self.tp, self.get_export_name("tpconst", "tptmin"))
        self.minimum_pressure_c = c_double.in_dll(self.tp, self.get_export_name("tpconst", "tppmin"))
        self.solideos_solid_init = getattr(self.tp, self.get_export_name("solideos", "solid_init"))

        # Eos interface
        self.eos_specificvolume = getattr(self.tp, self.get_export_name("eos", "specificvolume"))
        self.eos_zfac = getattr(self.tp, self.get_export_name("eos", "zfac"))
        self.eos_thermo = getattr(self.tp, self.get_export_name("eos", "thermo"))
        self.eos_entropy = getattr(self.tp, self.get_export_name("eos", "entropy"))
        self.eos_enthalpy = getattr(self.tp, self.get_export_name("eos", "enthalpy"))
        self.eos_compmoleweight = getattr(self.tp, self.get_export_name("eos", "compmoleweight"))
        self.eos_idealenthalpysingle = getattr(self.tp, self.get_export_name("eos", "idealenthalpysingle"))

        # Speed of sound
        #self.sos_singlePhaseSpeedOfSound = getattr(self.tp, '__speed_of_sound_MOD_singlephasespeedofsound')
        self.sos_sound_velocity_2ph = getattr(self.tp, self.get_export_name("speed_of_sound", "sound_velocity_2ph"))

        # Parameters
        #self.parameters_getcomp = getattr(self.tp, self.get_export_name("parameters", "getcomp"))

        # Flashes
        self.set_ph_tolerance = getattr(self.tp, self.get_export_name("ph_solver", "setphtolerance"))
        self.twophasetpflash = getattr(self.tp, self.get_export_name("tp_solver", "twophasetpflash"))
        self.psflash_twophase = getattr(self.tp, self.get_export_name("ps_solver", "twophasepsflash"))
        #self.tpflash_multiphase = getattr(self.tp, '__mp_tp_solver_MOD_mp_flash_tp')
        self.uvflash_twophase = getattr(self.tp, self.get_export_name("uv_solver", "twophaseuvflash"))
        self.phflash_twophase = getattr(self.tp, self.get_export_name("ph_solver", "twophasephflash"))
        self.svflash_twophase = getattr(self.tp, self.get_export_name("sv_solver", "twophasesvflash"))
        self.guess_phase = getattr(self.tp, self.get_export_name("thermo_utils", "guessphase"))

        # TV interfaces
        self.s_internal_energy_tv = getattr(self.tp, self.get_export_name("eostv", "internal_energy"))
        self.s_entropy_tv = getattr(self.tp, self.get_export_name("eostv", "entropytv"))
        self.s_pressure_tv = getattr(self.tp, self.get_export_name("eostv", "pressure"))
        self.s_lnphi_tv = getattr(self.tp, self.get_export_name("eostv", "thermotv"))
        self.s_enthalpy_tv = getattr(self.tp, self.get_export_name("eostv", "enthalpytv"))
        self.s_helmholtz_energy = getattr(self.tp, self.get_export_name("eostv", "free_energy"))
        self.s_chempot = getattr(self.tp, self.get_export_name("eostv", "chemical_potential"))

        # Saturation properties
        self.s_bubble_t = getattr(self.tp, self.get_export_name("saturation", "safe_bubt"))
        self.s_bubble_p = getattr(self.tp, self.get_export_name("saturation", "safe_bubp"))
        self.s_dew_t = getattr(self.tp, self.get_export_name("saturation", "safe_dewt"))
        self.s_dew_p = getattr(self.tp, self.get_export_name("saturation", "safe_dewp"))
        self.s_envelope_plot = getattr(self.tp, self.get_export_name("saturation_curve", "envelopeplot"))
        self.s_binary_plot = getattr(self.tp, self.get_export_name("binaryplot", "vllebinaryxy"))
        # Stability
        self.s_crit_tv = getattr(self.tp, self.get_export_name("critical", "calccriticaltv"))

        # Virials
        self.s_virial_coeffcients = getattr(self.tp, self.get_export_name("eostv", "virial_coefficients"))
        self.s_second_virial_matrix = getattr(self.tp, self.get_export_name("eostv", "secondvirialcoeffmatrix"))
        self.s_binary_third_virial_matrix = getattr(self.tp, self.get_export_name("eostv", "binarythirdvirialcoeffmatrix"))


    def activate(self):
        """Activate this instance of thermopack parameters for calculation
        """
        if self.model_id is not None:
            print("Activate")
        else:
            print("Check for default....")

    def get_export_name(self, module, method):
        """Generate library export name based on module and method name

        Args:
            module (string): Name of module
            method (string): Name of method

        Returns:
            string: Library export name
        """
        return self.prefix + module + "_" + self.module + "_" + method + self.postfix

    #################################
    # Init
    #################################

    def init_thermo(self,eosLib,eos,mixing,alpha,ncomp,comp_string,nphases,
                    liq_vap_discr_method=None,csp_eos=None,csp_ref_comp=None,
                    kij_setno=None,alpha_setno=None,saft_setno=None,
                    b_exponent=None,TrendEosForCp=None,cptype=None,
                    silent=None):
        """
        Initialize thermopack
        """
        null_pointer = POINTER(c_int)()

        eosLib_len = c_int(len(eosLib))
        eosLib_c = c_char_p(eosLib.encode('ascii'))
        eos_c = c_char_p(eos.encode('ascii'))
        eos_len = c_int(len(eos))
        mixing_c = c_char_p(mixing.encode('ascii'))
        mixing_len = c_int(len(mixing))
        alpha_c = c_char_p(alpha.encode('ascii'))
        alpha_len = c_int(len(alpha))
        ncomp_c = c_int(ncomp)
        comp_string_c = c_char_p(comp_string.encode('ascii'))
        comp_string_len = c_int(len(comp_string))
        nphases_c = c_int(nphases)
        if liq_vap_discr_method is None:
            liq_vap_discr_method_c = null_pointer
        else:
            liq_vap_discr_method_c = POINTER(c_int)(c_int(liq_vap_discr_method))
        if csp_eos is None:
            csp_eos_c = c_char_p()
            csp_eos_len = c_int(0)
        else:
            csp_eos_c = c_char_p(csp_eos.encode('ascii'))
            csp_eos_len = c_int(len(csp_eos))
        if csp_ref_comp is None:
            csp_ref_comp_c = c_char_p()
            csp_ref_comp_len = c_int(0)
        else:
            csp_ref_comp_c = c_char_p(csp_ref_comp.encode('ascii'))
            csp_ref_comp_len = c_int(len(csp_ref_comp))
        if kij_setno is None:
            kij_setno_c = null_pointer
        else:
            kij_setno_c = POINTER(c_int)(c_int(kij_setno))
        if alpha_setno is None:
            alpha_setno_c = null_pointer
        else:
            alpha_setno_c = POINTER(c_int)(c_int(alpha_setno))
        if saft_setno is None:
            saft_setno_c = null_pointer
        else:
            saft_setno_c = (c_int * ncomp)(*saft_setno)
        if b_exponent is None:
            b_exponent_c = POINTER(c_double)()
        else:
            b_exponent_c = POINTER(c_double)(c_double(b_exponent))
        if TrendEosForCp is None:
            TrendEosForCp_c = c_char_p()
            TrendEosForCp_len = c_int(0)
        else:
            TrendEosForCp_c = c_char_p(csp_eos.encode('ascii'))
            TrendEosForCp_len = c_int(len(csp_eos))
        if cptype is None:
            cptype_c = null_pointer
        else:
            cptype_c = (c_int * ncomp)(*cptype)

        if silent is None:
            silent_c = null_pointer
        else:
            silent_c = c_int(silent)

        self.eoslibinit_init_thermo.argtypes = [c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                c_char_p,
                                                POINTER( c_int ),
                                                c_char_p,
                                                POINTER( c_int ),
                                                POINTER( c_int ),
                                                c_char_p,
                                                c_char_p,
                                                POINTER( c_int ),
                                                POINTER( c_int ),
                                                POINTER( c_int ),
                                                POINTER( c_double ),
                                                c_char_p,
                                                POINTER( c_int ),
                                                POINTER( c_int ),
                                                c_int, c_int,
                                                c_int, c_int,
                                                c_int, c_int,
                                                c_int, c_int]


        self.eoslibinit_init_thermo.restype = None

        self.eoslibinit_init_thermo(eosLib_c,
                                    eos_c,
                                    mixing_c,
                                    alpha_c,
                                    byref(ncomp_c),
                                    comp_string_c,
                                    byref(nphases_c),
                                    liq_vap_discr_method_c,
                                    csp_eos_c,
                                    csp_ref_comp_c,
                                    kij_setno_c,
                                    alpha_setno_c,
                                    saft_setno_c,
                                    b_exponent_c,
                                    TrendEosForCp_c,
                                    cptype_c,
                                    silent_c,
                                    eosLib_len,
                                    eos_len,
                                    mixing_len,
                                    alpha_len,
                                    comp_string_len,
                                    csp_eos_len,
                                    csp_ref_comp_len,
                                    TrendEosForCp_len)
        self.nc = ncomp
        self.Rgas = c_double.in_dll(self.tp, self.get_export_name("tpconst", "rgas")).value

    def init_solid(self,scomp):
        """
        Initialize solid component
        """

        scomp_c = c_char_p(scomp.encode('ascii'))
        scomp_len = c_int(len(scomp))
        self.solideos_solid_init.argtypes = [c_char_p, c_int]
        self.solideos_solid_init.restype = None
        self.solideos_solid_init(scomp_c, scomp_len)

    #################################
    # Utility
    #################################

    def getcomp(self, comp):
        """Get component index

        Args:
            comp (string): Component name

        Returns:
            integer: Component FORTRAN index
        """
        comp_c = c_char_p(comp.encode('ascii'))
        self.parameters_getcomp.argtypes = [c_char_p]
        self.parameters_getcomp.restype = c_int
        idx = self.parameters_getcomp(comp_c, c_int)
        return idx

    def compmoleweight(self, comp):
        """Get component mole weight (g/mol)

        Args:
            comp (integer): Component FORTRAN index

        Returns:
            float: component mole weight (g/mol)
        """
        comp_c = c_int(comp)
        self.eos_compmoleweight.argtypes = [POINTER( c_int )]
        self.eos_compmoleweight.restype = c_double
        mw_i = self.eos_compmoleweight(byref(comp_c))
        return mw_i

    def get_phase_flags(self):
        """Get phase identifiers used by thermopack

        Returns:
            [integer]: Phase integer identifiers
        """
        iTWOPH = c_int()
        iLIQPH = c_int()
        iVAPPH = c_int()
        iMINGIBBSPH = c_int()
        iSINGLEPH = c_int()
        iSOLIDPH = c_int()
        iFAKEPH = c_int()

        self.s_get_phase_flags.argtypes = [POINTER( c_int ),
                                           POINTER( c_int ),
                                           POINTER( c_int ),
                                           POINTER( c_int ),
                                           POINTER( c_int ),
                                           POINTER( c_int ),
                                           POINTER( c_int )]
        self.s_get_phase_flags.restype = None
        self.s_get_phase_flags(byref(iTWOPH),
                               byref(iLIQPH),
                               byref(iVAPPH),
                               byref(iMINGIBBSPH),
                               byref(iSINGLEPH),
                               byref(iSOLIDPH),byref(iFAKEPH))
        self.TWOPH = iTWOPH.value
        self.LIQPH = iLIQPH.value
        self.VAPPH = iVAPPH.value
        self.MINGIBBSPH = iMINGIBBSPH.value
        self.SINGLEPH = iSINGLEPH.value
        self.SOLIDPH = iSOLIDPH.value
        self.FAKEPH = iFAKEPH.value

    def get_phase_type(self, i_phase):
        """Get phase type

        Args:
            i_phase (integer): Phase flag returned by thermopack

        Returns:
            string: Phase type
        """
        phase_string_list = ["TWO_PHASE", "LIQUID", "VAPOR", "MINIMUM_GIBBS", "SINGLE", "SOLID", "FAKE"]
        return phase_string_list[i_phase]

    def set_tmin(self, temp):
        self.minimum_temperature_c.value = temp

    def set_pmin(self, press):
        self.minimum_pressure_c.value = press

    #################################
    # Phase properties
    #################################

    def specific_volume(self,temp,press,x,phase,dvdt=None,dvdp=None,dvdn=None):
        """ Calculate single-phase specific volume
            Note that the order of the output match the default order of input for the differentials.
            Note further that dvdt, dvdp and dvdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Molar composition
            phase (integer): Calcualte root for specified phase
            dvdt (logical, optional): Calculate volume differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dvdp (logical, optional): Calculate volume differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dvdn (logical, optional): Calculate volume differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            [float]: Specific volume (m3/mol), and optionally differentials
        """
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

        self.eos_specificvolume.argtypes = [POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_int ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double )]

        self.eos_specificvolume.restype = None

        self.eos_specificvolume(byref(temp_c),
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

    def zfac(self,temp,press,x,phase,dzdt=None,dzdp=None,dzdn=None):
        """ Calculate single-phase compressibility
            Note that the order of the output match the default order of input for the differentials.
            Note further that dzdt, dzdp and dzdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Molar composition
            phase (integer): Calcualte root for specified phase
            dzdt (logical, optional): Calculate compressibility differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dzdp (logical, optional): Calculate compressibility differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dzdn (logical, optional): Calculate compressibility differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            [float]: Compressibility (-), and optionally differentials
        """
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

        self.eos_zfac.argtypes = [POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_int ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double ),
                                  POINTER( c_double )]

        self.eos_zfac.restype = None

        self.eos_zfac(byref(temp_c),
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

    def thermo(self,temp,press,x,phase,dlnfugdt=None,dlnfugdp=None,
               dlnfugdn=None,ophase=None,v=None):
        """
        Calculate logarithm of fugacity coefficient given composition,
        temperature and pressure
        dlnfugdt, dlnfugdp, dlnfugdn, ophase are
        only flags to enable calculation
        """
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

        self.eos_thermo.argtypes = [POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int ),
                                    POINTER( c_int ),
                                    POINTER( c_double )]

        self.eos_thermo.restype = None

        self.eos_thermo(byref(temp_c),
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
            dlnfugdn_r = np.zeros((len(x),len(x)))
            for i in range(len(x)):
                for j in range(len(x)):
                    dlnfugdn_r[i][j] = dlnfugdn_c[i+j*len(x)]
            return_tuple += (dlnfugdn_r, )
        if not ophase is None:
            return_tuple += (ophase_c[0], )
        if not v is None:
            return_tuple += (v_c[0], )

        return return_tuple

    def enthalpy(self,temp,press,x,phase,dhdt=None,dhdp=None,dhdn=None):
        """ Calculate specific single-phase enthalpy
            Note that the order of the output match the default order of input for the differentials.
            Note further that dhdt, dhdp and dhdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Molar composition
            phase (integer): Calcualte root for specified phase
            dhdt (logical, optional): Calculate enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dhdp (logical, optional): Calculate enthalpy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dhdn (logical, optional): Calculate enthalpy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            [float]: Specific enthalpy (J/mol), and optionally differentials
        """
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

        residual_c = POINTER(c_int)()
        self.eos_enthalpy.argtypes = [POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_int ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_int )]

        self.eos_enthalpy.restype = None

        self.eos_enthalpy(byref(temp_c),
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

    def entropy(self,temp,press,x,phase,dsdt=None,dsdp=None,dsdn=None):
        """ Calculate specific single-phase entropy
            Note that the order of the output match the default order of input for the differentials.
            Note further that dsdt, dhsp and dsdn only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Molar composition
            phase (integer): Calcualte root for specified phase
            dsdt (logical, optional): Calculate entropy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dsdp (logical, optional): Calculate entropy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.
            dsdn (logical, optional): Calculate entropy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

        Returns:
            [float]: Specific entropy (J/mol/K), and optionally differentials
        """
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
        residual_c = POINTER(c_int)()

        self.eos_entropy.argtypes = [POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_double ),
                                     POINTER( c_int )]

        self.eos_entropy.restype = None

        self.eos_entropy(byref(temp_c),
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

    def idealenthalpysingle(self,temp,press,j,dhdt=None,dhdp=None):
        """ Calculate specific ideal enthalpy
            Note that the order of the output match the default order of input for the differentials.
            Note further that dhdt, and dhdp only are flags to enable calculation.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Molar composition
            phase (integer): Calcualte root for specified phase
            dhdt (logical, optional): Calculate ideal enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.
            dhdp (logical, optional): Calculate ideal enthalpy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

        Returns:
            [float]: Specific ideal enthalpy (J/mol), and optionally differentials
        """
        null_pointer = POINTER(c_double)()

        temp_c = c_double(temp)
        press_c = c_double(press)
        j_c = c_int(j)
        h_c = c_double(0.0)

        if dhdt is None:
            dhdt_c = null_pointer
        else:
            dhdt_c = POINTER(c_double)(c_double(0.0))
        if dhdp is None:
            dhdp_c = null_pointer
        else:
            dhdp_c = POINTER(c_double)(c_double(0.0))

        self.eos_idealenthalpysingle.argtypes = [POINTER( c_double ),
                                                 POINTER( c_double ),
                                                 POINTER( c_int ),
                                                 POINTER( c_double ),
                                                 POINTER( c_double ),
                                                 POINTER( c_double )]

        self.eos_idealenthalpysingle.restype = None

        self.eos_idealenthalpysingle(byref(temp_c),
                                     byref(press_c),
                                     byref(j_c),
                                     byref(h_c),
                                     dhdt_c,
                                     dhdp_c)
        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )
        if not dhdp is None:
            return_tuple += (dhdp_c[0], )

        return return_tuple

    def speed_of_sound(self,temp,press,x,y,z,betaV,betaL,phase):
        """Calculate speed of sound for single phase or two phase mixture assuming
        mechanical, thermal and chemical equilibrium.

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            x (float array): Liquid molar composition
            y (float array): Gas molar composition
            z (float array): Overall molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (integer): Calcualte root for specified phase

        Returns:
            [float]: Speed of sound (m/s)
        """
        temp_c = c_double(temp)
        press_c = c_double(press)
        x_c = (c_double * len(x))(*x)
        y_c = (c_double * len(y))(*y)
        z_c = (c_double * len(z))(*z)
        betaV_c = c_double(betaV)
        betaL_c = c_double(betaL)
        phase_c = c_int(phase)
        ph_c = POINTER(c_int)()

        self.sos_sound_velocity_2ph.argtypes = [POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_double ),
                                                POINTER( c_int ),
                                                POINTER( c_int )]

        self.sos_sound_velocity_2ph.restype = c_double

        sos = self.sos_sound_velocity_2ph(byref(temp_c),
                                          byref(press_c),
                                          x_c,
                                          y_c,
                                          z_c,
                                          byref(betaV_c),
                                          byref(betaL_c),
                                          byref(phase_c),
                                          ph_c)

        return sos

    #################################
    # Flash interfaces
    #################################

    def set_ph_tolerance(tol):
        """Set tolerance of isobaric-isentalpic (PH) flash

        Args:
            tol (float): Tolerance
        """
        tol_c = c_double(tol)
        self.set_ph_tolerance.argtypes = [POINTER( c_double )]
        self.set_ph_tolerance.restype = None
        self.set_ph_tolerance(byref(tol_c))

    def two_phase_tpflash(self,temp,press,z):
        """Do isothermal-isobaric (TP) flash

        Args:
            temp (float): Temperature (K)
            press (folat): Pressure (Pa)
            z (float array): Overall molar composition

        Returns:
            x (float array): Liquid molar composition
            y (float array): Gas molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (integer): Phase identifier (iTWOPH/iLIQPH/iVAPPH)
        """
        temp_c = c_double(temp)
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.twophasetpflash.argtypes = [POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_int ),
                                         POINTER( c_double ),
                                         POINTER( c_double )]

        self.twophasetpflash.restype = None

        self.twophasetpflash(byref(temp_c),
                             byref(press_c),
                             z_c,
                             byref(betaV_c),
                             byref(betaL_c),
                             byref(phase_c),
                             x_c,
                             y_c)

        x = np.array(x_c)
        y = np.array(y_c)

        return x, y, betaV_c.value, betaL_c.value, phase_c.value


    def two_phase_psflash(self,press,z,entropy,temp=None):
        """Do isentropic-isobaric (SP) flash

        Args:
            press (float): Pressure (Pa)
            z (float array): Overall molar composition
            entropy (float): Specific entropy (J/mol/K)
            temp (float, optional): Initial guess for temperature (K)

        Returns:
            temp (float): Temperature (K)
            x (float array): Liquid molar composition
            y (float array): Gas molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (integer): Phase identifier (iTWOPH/iLIQPH/iVAPPH)
        """

        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)
        s_c = c_double(entropy)

        if not temp is None:
            temp_c = POINTER( c_double )(c_double(temp))
        else:
            temp_c = POINTER( c_double )(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.psflash_twophase.argtypes = [POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_int )]

        self.psflash_twophase.restype = None

        self.psflash_twophase(temp_c,
                              byref(press_c),
                              z_c,
                              byref(betaV_c),
                              byref(betaL_c),
                              x_c,
                              y_c,
                              byref(s_c),
                              byref(phase_c))

        x = np.array(x_c)
        y = np.array(y_c)

        return temp_c[0], x, y, betaV_c.value, betaL_c.value, phase_c.value

    def two_phase_phflash(self,press,z,enthalpy,temp=None):
        """Do isenthalpic-isobaric (HP) flash

        Args:
            press (float): Pressure (Pa)
            z (float array): Overall molar composition
            enthalpy (float): Specific enthalpy (J/mol)
            temp (float, optional): Initial guess for temperature (K)

        Returns:
            temp (float): Temperature (K)
            x (float array): Liquid molar composition
            y (float array): Gas molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (integer): Phase identifier (iTWOPH/iLIQPH/iVAPPH)
        """
        press_c = c_double(press)
        z_c = (c_double * len(z))(*z)
        h_c = c_double(enthalpy)

        if not temp is None:
            temp_c = POINTER( c_double )(c_double(temp))
        else:
            temp_c = POINTER( c_double )(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.phflash_twophase.argtypes = [POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_int )]

        self.phflash_twophase.restype = None

        self.phflash_twophase(temp_c,
                              byref(press_c),
                              z_c,
                              byref(betaV_c),
                              byref(betaL_c),
                              x_c,
                              y_c,
                              byref(h_c),
                              byref(phase_c))

        x = np.array(x_c)
        y = np.array(y_c)

        return temp_c[0], x, y, betaV_c.value, betaL_c.value, phase_c.value

    def two_phase_uvflash(self,z,specific_energy,specific_volume,temp=None,press=None):
        """Do isoenergetic-isochoric (UV) flash

        Args:
            press (float): Pressure (Pa)
            z (float array): Overall molar composition
            specific_energy (float): Specific energy (J/mol)
            specific_volume (float): Specific volume (m3/mol)
            temp (float, optional): Initial guess for temperature (K)
            press (float, optional): Initial guess for pressure (Pa)

        Returns:
            temp (float): Temperature (K)
            press (float): Pressure (Pa)
            x (float array): Liquid molar composition
            y (float array): Gas molar composition
            betaV (float): Molar gas phase fraction
            betaL (float): Molar liquid phase fraction
            phase (integer): Phase identifier (iTWOPH/iLIQPH/iVAPPH)
        """

        z_c = (c_double * len(z))(*z)
        e_c = c_double(specific_energy)
        v_c = c_double(specific_volume)

        if not temp is None:
            temp_c = POINTER( c_double )(c_double(temp))
        else:
            temp_c = POINTER( c_double )(c_double(0.0))

        if not press is None:
            press_c = POINTER( c_double )(c_double(press))
        else:
            press_c = POINTER( c_double )(c_double(0.0))

        x_c = (c_double * len(z))(0.0)
        y_c = (c_double * len(z))(0.0)
        betaV_c = c_double(0.0)
        betaL_c = c_double(0.0)
        phase_c = c_int(0)

        self.phflash_twophase.argtypes = [POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_double ),
                                          POINTER( c_int )]

        self.phflash_twophase.restype = None

        self.phflash_twophase(temp_c,
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

        return temp_c[0], press_c[0], x, y, betaV_c.value, betaL_c.value, phase_c.value


    #################################
    # Temperature-volume property interfaces
    #################################

    def pressure_tv(self,temp,volume,n,dpdt=None,dpdv=None,dpdn=None):

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

        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_pressure_tv.argtypes = [POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_int )]

        self.s_pressure_tv.restype = c_double

        P = self.s_pressure_tv(byref(temp_c),
                               byref(v_c),
                               n_c,
                               dpdv_c,
                               dpdt_c,
                               d2pdv2_c,
                               dpdn_c,
                               recalculate_c)

        return_tuple = (P, )
        if not dpdt is None:
            return_tuple += (dpdt_c[0], )
        if not dpdv is None:
            return_tuple += (dpdv_c[0], )
        if not dpdn is None:
            return_tuple += (np.array(dpdn_c), )

        return return_tuple

    def internal_energy_tv(self,temp,volume,n,dedt=None,dedv=None):

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

        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_internal_energy_tv.argtypes = [POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_double ),
                                              POINTER( c_int )]

        self.s_internal_energy_tv.restype = None

        self.s_internal_energy_tv(byref(temp_c),
                                  byref(v_c),
                                  n_c,
                                  byref(e_c),
                                  dedt_c,
                                  dedv_c,
                                  recalculate_c)

        return_tuple = (e_c.value, )
        if not dedt is None:
            return_tuple += (dedt_c[0], )
        if not dedv is None:
            return_tuple += (dedv_c[0], )

        return return_tuple

    def entropy_tv(self,temp,volume,n,dsdt=None,dsdv=None,dsdn=None):

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

        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_entropy_tv.argtypes = [POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_double ),
                                      POINTER( c_int )]

        self.s_entropy_tv.restype = None

        self.s_entropy_tv(byref(temp_c),
                          byref(v_c),
                          n_c,
                          byref(s_c),
                          dsdt_c,
                          dsdv_c,
                          dsdn_c,
                          recalculate_c)

        return_tuple = (s_c.value, )
        if not dsdt is None:
            return_tuple += (dsdt_c[0], )
        if not dsdv is None:
            return_tuple += (dsdv_c[0], )
        if not dsdn is None:
            return_tuple += (np.array(dsdn_c), )

        return return_tuple

    def enthalpy_tv(self,temp,volume,n,dhdt=None,dhdv=None,dhdn=None):

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

        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_enthalpy_tv.argtypes = [POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_int )]

        self.s_enthalpy_tv.restype = None

        self.s_enthalpy_tv(byref(temp_c),
                           byref(v_c),
                           n_c,
                           byref(h_c),
                           dhdt_c,
                           dhdv_c,
                           dhdn_c,
                           recalculate_c)

        return_tuple = (h_c.value, )
        if not dhdt is None:
            return_tuple += (dhdt_c[0], )
        if not dhdv is None:
            return_tuple += (dhdv_c[0], )
        if not dhdn is None:
            return_tuple += (np.array(dhdn_c), )

        return return_tuple

    def helmholtz_tv(self,temp,volume,n,dadt=None,dadv=None):

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
        d2adt2_c = null_pointer
        d2adv2_c = null_pointer
        d2advdt_c = null_pointer
        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_helmholtz_energy.argtypes = [POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_double ),
                                            POINTER( c_int )]

        self.s_helmholtz_energy.restype = None

        self.s_helmholtz_energy(byref(temp_c),
                                byref(v_c),
                                n_c,
                                byref(a_c),
                                dadt_c,
                                dadv_c,
                                d2adt2_c,
                                d2adv2_c,
                                d2advdt_c,
                                recalculate_c)

        return_tuple = (a_c.value, )
        if not dadt is None:
            return_tuple += (dadt_c[0], )
        if not dadv is None:
            return_tuple += (dadv_c[0], )

        return return_tuple

    def chemical_potential_tv(self,temp,volume,n,dmudt=None,dmudv=None,dmudn=None):

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

        recalculate_c = POINTER(c_int)(c_int(1))

        self.s_chempot.argtypes = [POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_int )]

        self.s_chempot.restype = None

        self.s_chempot(byref(temp_c),
                       byref(v_c),
                       n_c,
                       mu_c,
                       dmudv_c,
                       dmudt_c,
                       dmudn_c,
                       recalculate_c)

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

    def fugacity_tv(self,temp,volume,n,dlnphidt=None,dlnphidv=None,dlnphidn=None):

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

        self.s_lnphi_tv.argtypes = [POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double )]

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
            dlnphidn = np.zeros((len(n),len(n)))
            for i in range(len(n)):
                for j in range(len(n)):
                    dlnphidn[i][j] = dlnphidn_c[i + j*len(n)]
            return_tuple += (dlnphidn, )

        return return_tuple

    #################################
    # Saturation interfaces
    #################################

    def bubble_temperature(self,press,z):
        press_c = c_double(press)
        y_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_bubble_t.argtypes = [POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int )]

        self.s_bubble_t.restype = c_double

        temp = self.s_bubble_t(byref(press_c),
                               z_c,
                               y_c,
                               byref(ierr_c))

        y = np.array(y_c)
        if ierr_c.value != 0:
            raise Exception("bubble_temperature calclualtion failed")
        return temp, y

    def bubble_pressure(self,temp,z):
        temp_c = c_double(temp)
        y_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_bubble_p.argtypes = [POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_double ),
                                    POINTER( c_int )]

        self.s_bubble_p.restype = c_double

        press = self.s_bubble_p(byref(temp_c),
                                z_c,
                                y_c,
                                byref(ierr_c))

        y = np.array(y_c)
        if ierr_c.value != 0:
            raise Exception("bubble_pressure calclualtion failed")
        return press, y

    def dew_temperature(self,press,z):
        press_c = c_double(press)
        x_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_dew_t.argtypes = [POINTER( c_double ),
                                 POINTER( c_double ),
                                 POINTER( c_double ),
                                 POINTER( c_int )]

        self.s_dew_t.restype = c_double

        temp = self.s_dew_t(byref(press_c),
                            x_c,
                            z_c,
                            byref(ierr_c))

        x = np.array(x_c)
        if ierr_c.value != 0:
            raise Exception("dew_temperature calclualtion failed")
        return temp, x

    def dew_pressure(self,temp,z):
        temp_c = c_double(temp)
        x_c = (c_double * len(z))(0.0)
        z_c = (c_double * len(z))(*z)
        ierr_c = c_int(0)

        self.s_dew_p.argtypes = [POINTER( c_double ),
                                 POINTER( c_double ),
                                 POINTER( c_double ),
                                 POINTER( c_int )]

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
                              minimum_temperature=None, step_size=None):
        """Get the phase-envelope

        Arguments
        T_init      Initial guess for temperature for p_init
        p_init      Initial pressure
        z           Mixture molar composition

        Returns
        Temperature values (K)
        Pressure values (Pa)
        Molar composition of incipient phase (-)
        Label of incipient phase (-)
        """
        nmax = 1000
        z_c = (c_double * len(z))(*z)
        temp_c = c_double(0.0)
        press_c = c_double(initial_pressure)
        spec_c = c_int(1)
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
        if step_size is None:
            ds_c = null_pointer
        else:
            ds_c = POINTER(c_double)(c_double(step_size))
        exitOnTriplePoint_c = POINTER(c_int)()
        if minimum_temperature is None:
            tme_c = null_pointer
        else:
            tme_c = POINTER(c_double)(c_double(minimum_temperature))

        self.s_envelope_plot.argtypes = [POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_int ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_int ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_int ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_double ),
                                         POINTER( c_int ),
                                         POINTER( c_double )]

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
                             tme_c)

        Tvals = np.array(Ta_c[0:n_c.value])
        Pvals = np.array(Pa_c[0:n_c.value])

        return Tvals, Pvals

    def get_binary_pxy(self,
                       temp,
                       maximum_pressure=1.5e7,
                       minimum_pressure=1.0e5,
                       maximum_dz = 0.003,
                       maximum_dlns=0.01):
        # Redefinition of module parameter:
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
        filename_len = c_int(len(filename))
        res_c = (c_double * (nmax*9))(0.0)
        nres_c = (c_int * 3)(0)
        wsf_c = c_int(1)

        self.s_binary_plot.argtypes = [POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_int ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_char_p ),
                                       POINTER( c_double ),
                                       POINTER( c_double ),
                                       POINTER( c_int ),
                                       POINTER( c_int ),
                                       POINTER( c_double ),
                                       c_int]

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
                           filename_len)

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


    #################################
    # Stability interfaces
    #################################

    def critical(self, n, temp=0.0, v=0.0, tol=1.0e-8):
        temp_c = c_double(temp)
        v_c = c_double(v)
        n_c = (c_double * len(n))(*n)
        ierr_c = c_int(0)
        P_c = c_double(0.0)
        tol_c = c_double(tol)
        self.s_crit_tv.argtypes = [POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_double ),
                                   POINTER( c_int ),
                                   POINTER( c_double ),
                                   POINTER( c_double )]

        self.s_crit_tv.restype = None

        self.s_crit_tv(byref(temp_c),
                       byref(v_c),
                       n_c,
                       byref(ierr_c),
                       byref(tol_c),
                       byref(P_c))

        if ierr_c.value != 0:
            raise Exception("critical calclualtion failed")

        return temp_c.value, v_c.value, P_c.value

    #################################
    # Virial interfaces
    #################################

    def virial_coeffcients(self, temp, n):
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
