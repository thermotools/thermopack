<!--- 
Generated at: 2023-04-11T13:44:24.532600
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
thermo class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

# Methods in the thermo class (`thermo.py`)

The `thermo` class, found in `addon/pycThermopack/thermo.py`, is the core of the ThermoPack Python interface. All equation of state classes inherit from `thermo`. This is the class that contains the interface to all practical calculations that can be done from the python-side of ThermoPack. Derived classes only implement specific functions for parameter handling etc.

## Table of contents
  * [TV-property interfaces](#TV-property-interfaces)
    * [chemical_potential_tv](#chemical_potential_tvself-temp-volume-n-dmudtNone-dmudvNone-dmudnNone-property_flagIR)
    * [enthalpy_tv](#enthalpy_tvself-temp-volume-n-dhdtNone-dhdvNone-dhdnNone-property_flagIR)
    * [entropy_tv](#entropy_tvself-temp-volume-n-dsdtNone-dsdvNone-dsdnNone-property_flagIR)
    * [fugacity_tv](#fugacity_tvself-temp-volume-n-dlnphidtNone-dlnphidvNone-dlnphidnNone)
    * [helmholtz_tv](#helmholtz_tvself-temp-volume-n-dadtNone-dadvNone-dadnNone-property_flagIR)
    * [internal_energy_tv](#internal_energy_tvself-temp-volume-n-dedtNone-dedvNone-dednNone-property_flagIR)
    * [pressure_tv](#pressure_tvself-temp-volume-n-dpdtNone-dpdvNone-dpdnNone-property_flagIR)
  * [Tp-property interfaces](#Tp-property-interfaces)
    * [enthalpy](#enthalpyself-temp-press-x-phase-dhdtNone-dhdpNone-dhdnNone-residualFalse)
    * [entropy](#entropyself-temp-press-x-phase-dsdtNone-dsdpNone-dsdnNone-residualFalse)
    * [idealenthalpysingle](#idealenthalpysingleself-temp-j-dhdtNone)
    * [idealentropysingle](#idealentropysingleself-temp-press-j-dsdtNone-dsdpNone)
    * [specific_volume](#specific_volumeself-temp-press-x-phase-dvdtNone-dvdpNone-dvdnNone)
    * [speed_of_sound](#speed_of_soundself-temp-press-x-y-z-betaV-betaL-phase)
    * [thermo](#thermoself-temp-press-x-phase-dlnfugdtNone-dlnfugdpNone-dlnfugdnNone-ophaseNone-vNone)
    * [zfac](#zfacself-temp-press-x-phase-dzdtNone-dzdpNone-dzdnNone)
  * [TVp-property interfaces](#TVp-property-interfaces)
    * [enthalpy_tvp](#enthalpy_tvpself-temp-volume-n-dhdtNone-dhdpNone-dhdnNone-property_flagIR)
    * [entropy_tvp](#entropy_tvpself-temp-volume-n-dsdtNone-dsdpNone-dsdnNone-property_flagIR)
    * [thermo_tvp](#thermo_tvpself-temp-v-n-phase-dlnfugdtNone-dlnfugdpNone-dlnfugdnNone)
  * [Flash interfaces](#Flash-interfaces)
    * [guess_phase](#guess_phaseself-temp-press-z)
    * [set_ph_tolerance](#set_ph_toleranceself-tol)
    * [two_phase_phflash](#two_phase_phflashself-press-z-enthalpy-tempNone)
    * [two_phase_psflash](#two_phase_psflashself-press-z-entropy-tempNone)
    * [two_phase_tpflash](#two_phase_tpflashself-temp-press-z)
    * [two_phase_uvflash](#two_phase_uvflashself-z-specific_energy-specific_volume-tempNone-pressNone)
  * [Saturation interfaces](#Saturation-interfaces)
    * [bubble_pressure](#bubble_pressureself-temp-z)
    * [bubble_temperature](#bubble_temperatureself-press-z)
    * [dew_pressure](#dew_pressureself-temp-z)
    * [dew_temperature](#dew_temperatureself-press-z)
    * [get_binary_pxy](#get_binary_pxyself-temp-maximum_pressure150000000-minimum_pressure1000000-maximum_dz0003-maximum_dlns001)
    * [get_binary_txy](#get_binary_txyself-pressure-minimum_temperaturte00-maximum_dz0003-maximum_dlns0005)
    * [get_bp_term](#get_bp_termself-i_term)
    * [get_envelope_twophase](#get_envelope_twophaseself-initial_pressure-z-maximum_pressure150000000-minimum_temperatureNone-step_sizeNone-calc_vFalse)
    * [global_binary_plot](#global_binary_plotself-maximum_pressure150000000-minimum_pressure1000000-minimum_temperature1500-maximum_temperature5000-include_azeotropesFalse)
    * [solid_envelope_plot](#solid_envelope_plotself-initial_pressure-z-maximum_pressure150000000-minimum_temperature1700-calc_esvFalse)
  * [Isolines](#Isolines)
    * [get_isenthalp](#get_isenthalpself-enthalpy-z-minimum_pressure1000000-maximum_pressure150000000-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isentrope](#get_isentropeself-entropy-z-minimum_pressure1000000-maximum_pressure150000000-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isobar](#get_isobarself-press-z-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isotherm](#get_isothermself-temp-z-minimum_pressure1000000-maximum_pressure150000000-nmax100)
  * [Stability interfaces](#Stability-interfaces)
    * [critical](#criticalself-n-temp00-v00-tol1e-07)
    * [critical_pressure](#critical_pressureself-i)
    * [critical_temperature](#critical_temperatureself-i)
  * [Virial interfaces](#Virial-interfaces)
    * [binary_third_virial_matrix](#binary_third_virial_matrixself-temp)
    * [second_virial_matrix](#second_virial_matrixself-temp)
    * [virial_coeffcients](#virial_coeffcientsself-temp-n)
  * [Joule-Thompson interface](#Joule-Thompson-interface)
    * [joule_thompson_inversion](#joule_thompson_inversionself-z-nmax1000)
  * [Utility methods](#Utility-methods)
    * [acentric_factor](#acentric_factorself-i)
    * [compmoleweight](#compmoleweightself-comp)
    * [get_comp_name](#get_comp_nameself-index)
    * [get_ideal_enthalpy_reference_value](#get_ideal_enthalpy_reference_valueself-j)
    * [get_ideal_entropy_reference_value](#get_ideal_entropy_reference_valueself-j)
    * [get_phase_flags](#get_phase_flagsself)
    * [get_phase_type](#get_phase_typeself-i_phase)
    * [get_pmax](#get_pmaxself)
    * [get_pmin](#get_pminself)
    * [get_tmax](#get_tmaxself)
    * [get_tmin](#get_tminself)
    * [getcompindex](#getcompindexself-comp)
    * [redefine_critical_parameters](#redefine_critical_parametersself-silentTrue-Tc_initialsNone-vc_initialsNone)
    * [set_ideal_enthalpy_reference_value](#set_ideal_enthalpy_reference_valueself-j-h0)
    * [set_ideal_entropy_reference_value](#set_ideal_entropy_reference_valueself-j-s0)
    * [set_pmax](#set_pmaxself-press)
    * [set_pmin](#set_pminself-press)
    * [set_tmax](#set_tmaxself-temp)
    * [set_tmin](#set_tminself-temp)
  * [Internal methods](#Internal-methods)
    * [\_\_del\_\_](#__del__self)
    * [\_\_init\_\_](#__init__self)
    * [activate](#activateself)
    * [add_eos](#add_eosself)
    * [delete_eos](#delete_eosself)
    * [get_export_name](#get_export_nameself-module-method)
    * [get_model_id](#get_model_idself)
    * [init_peneloux_volume_translation](#init_peneloux_volume_translationself-parameter_referenceDefault)
    * [init_solid](#init_solidself-scomp)
    * [init_thermo](#init_thermoself-eos-mixing-alpha-comps-nphases-liq_vap_discr_methodNone-csp_eosNone-csp_ref_compNone-kij_refDefault-alpha_refDefault-saft_refDefault-b_exponentNone-TrendEosForCpNone-cptypeNone-silentNone)

## TV-property interfaces

Computing properties as a function of temperature and volume. Derivatives returned by methods in this section are computed as functions of (T, V, n).

### Table of contents
  * [TV-property interfaces](#TV-property-interfaces)
    * [chemical_potential_tv](#chemical_potential_tvself-temp-volume-n-dmudtNone-dmudvNone-dmudnNone-property_flagIR)
    * [enthalpy_tv](#enthalpy_tvself-temp-volume-n-dhdtNone-dhdvNone-dhdnNone-property_flagIR)
    * [entropy_tv](#entropy_tvself-temp-volume-n-dsdtNone-dsdvNone-dsdnNone-property_flagIR)
    * [fugacity_tv](#fugacity_tvself-temp-volume-n-dlnphidtNone-dlnphidvNone-dlnphidnNone)
    * [helmholtz_tv](#helmholtz_tvself-temp-volume-n-dadtNone-dadvNone-dadnNone-property_flagIR)
    * [internal_energy_tv](#internal_energy_tvself-temp-volume-n-dedtNone-dedvNone-dednNone-property_flagIR)
    * [pressure_tv](#pressure_tvself-temp-volume-n-dpdtNone-dpdvNone-dpdnNone-property_flagIR)


### `chemical_potential_tv(self, temp, volume, n, dmudt=None, dmudv=None, dmudn=None, property_flag='IR')`
Calculate chemical potential given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dmudt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dmudv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dmudn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Chemical potential (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally chemical potential differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `enthalpy_tv(self, temp, volume, n, dhdt=None, dhdv=None, dhdn=None, property_flag='IR')`
Calculate enthalpy given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dhdt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enthalpy (J)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally enthalpy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `entropy_tv(self, temp, volume, n, dsdt=None, dsdv=None, dsdn=None, property_flag='IR')`
Calculate entropy given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dsdt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Entropy (J/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally entropy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `fugacity_tv(self, temp, volume, n, dlnphidt=None, dlnphidv=None, dlnphidn=None)`
Calculate natural logarithm of fugacity given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dlnphidt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnphidv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnphidn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarry:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Natural logarithm of fugacity

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `helmholtz_tv(self, temp, volume, n, dadt=None, dadv=None, dadn=None, property_flag='IR')`
Calculate Helmholtz energy given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dadt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dadv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dadn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Helmholtz energy (J)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally energy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `internal_energy_tv(self, temp, volume, n, dedt=None, dedv=None, dedn=None, property_flag='IR')`
Calculate internal energy given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dedt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dedv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dedn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual ('R'), ideal ('I') or total ('IR') internal energy. Defaults to 'IR'.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Energy (J)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally energy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `pressure_tv(self, temp, volume, n, dpdt=None, dpdv=None, dpdn=None, property_flag='IR')`
Calculate pressure given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dpdt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dpdv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dpdn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual ('R'), ideal ('I') or total ('IR') pressure. Defaults to 'IR'.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally pressure differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Tp-property interfaces

Computing properties as a function of temperature and pressure. Derivatives returned by methods in this section are computed as functions of (T, p, n).

### Table of contents
  * [Tp-property interfaces](#Tp-property-interfaces)
    * [enthalpy](#enthalpyself-temp-press-x-phase-dhdtNone-dhdpNone-dhdnNone-residualFalse)
    * [entropy](#entropyself-temp-press-x-phase-dsdtNone-dsdpNone-dsdnNone-residualFalse)
    * [idealenthalpysingle](#idealenthalpysingleself-temp-j-dhdtNone)
    * [idealentropysingle](#idealentropysingleself-temp-press-j-dsdtNone-dsdpNone)
    * [specific_volume](#specific_volumeself-temp-press-x-phase-dvdtNone-dvdpNone-dvdnNone)
    * [speed_of_sound](#speed_of_soundself-temp-press-x-y-z-betaV-betaL-phase)
    * [thermo](#thermoself-temp-press-x-phase-dlnfugdtNone-dlnfugdpNone-dlnfugdnNone-ophaseNone-vNone)
    * [zfac](#zfacself-temp-press-x-phase-dzdtNone-dzdpNone-dzdnNone)


### `enthalpy(self, temp, press, x, phase, dhdt=None, dhdp=None, dhdn=None, residual=False)`
Calculate specific single-phase enthalpy Note that the order of the output match the default order of input for the differentials. Note further that dhdt, dhdp and dhdn only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; **dhdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate enthalpy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate enthalpy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **residual (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual enthalpy. Defaults to False.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific enthalpy (J/mol), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `entropy(self, temp, press, x, phase, dsdt=None, dsdp=None, dsdn=None, residual=False)`
Calculate specific single-phase entropy Note that the order of the output match the default order of input for the differentials. Note further that dsdt, dhsp and dsdn only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; **dsdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate entropy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate entropy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate entropy differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **residual (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual entropy. Defaults to False.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific entropy (J/mol/K), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `idealenthalpysingle(self, temp, j, dhdt=None)`
Calculate specific ideal enthalpy Note that the order of the output match the default order of input for the differentials. Note further that dhdt only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **dhdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate ideal enthalpy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific ideal enthalpy (J/mol), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `idealentropysingle(self, temp, press, j, dsdt=None, dsdp=None)`
Calculate specific ideal entropy Note that the order of the output match the default order of input for the differentials. Note further that dhdt, and dhdp only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **dsdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate ideal entropy differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate ideal entropy differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific ideal entropy (J/mol/K), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `specific_volume(self, temp, press, x, phase, dvdt=None, dvdp=None, dvdn=None)`
Calculate single-phase specific volume Note that the order of the output match the default order of input for the differentials. Note further that dvdt, dvdp and dvdn only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; **dvdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate molar volume differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dvdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate molar volume differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dvdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate molar volume differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific volume (m3/mol), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `speed_of_sound(self, temp, press, x, y, z, betaV, betaL, phase)`
Calculate speed of sound for single phase or two phase mixture assuming mechanical, thermal and chemical equilibrium.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **y (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Gas molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Overall molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **betaV (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar gas phase fraction

&nbsp;&nbsp;&nbsp;&nbsp; **betaL (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar liquid phase fraction

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Speed of sound (m/s)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `thermo(self, temp, press, x, phase, dlnfugdt=None, dlnfugdp=None, dlnfugdn=None, ophase=None, v=None)`
Calculate logarithm of fugacity coefficient given composition, temperature and pressure. Note that the order of the output match the default order of input for the differentials. Note further that dlnfugdt, dlnfugdp, dlnfugdn and ophase only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar composition (.)

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **ophase (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Phase flag. Only set when phase=MINGIBBSPH.

&nbsp;&nbsp;&nbsp;&nbsp; **v (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific volume (m3/mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  fugacity coefficient (-), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `zfac(self, temp, press, x, phase, dzdt=None, dzdp=None, dzdn=None)`
Calculate single-phase compressibility Note that the order of the output match the default order of input for the differentials. Note further that dzdt, dzdp and dzdn only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **x (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calcualte root for specified phase

&nbsp;&nbsp;&nbsp;&nbsp; **dzdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate compressibility differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dzdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate compressibility differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dzdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate compressibility differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Compressibility (-), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## TVp-property interfaces

Computing properties given Temperature, volume and mole numbers, but evaluate derivatives as functions of (T, p, n). See [Advanced Usage => The different property interfaces](https://github.com/thermotools/thermopack/wiki/Advanced-usage#the-different-property-interfaces-tv--tp--and-tvp-) for further explanation.

### Table of contents
  * [TVp-property interfaces](#TVp-property-interfaces)
    * [enthalpy_tvp](#enthalpy_tvpself-temp-volume-n-dhdtNone-dhdpNone-dhdnNone-property_flagIR)
    * [entropy_tvp](#entropy_tvpself-temp-volume-n-dsdtNone-dsdpNone-dsdnNone-property_flagIR)
    * [thermo_tvp](#thermo_tvpself-temp-v-n-phase-dlnfugdtNone-dlnfugdpNone-dlnfugdnNone)


### `enthalpy_tvp(self, temp, volume, n, dhdt=None, dhdp=None, dhdn=None, property_flag='IR')`
Calculate enthalpy given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dhdt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdp (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dhdn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enthalpy (J)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally enthalpy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `entropy_tvp(self, temp, volume, n, dsdt=None, dsdp=None, dsdn=None, property_flag='IR')`
Calculate entropy given temperature, pressure and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dsdt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdp (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dsdn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **property_flag (integer, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate residual (R) and/or ideal (I) entropy. Defaults to IR.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Entropy (J/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally entropy differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `thermo_tvp(self, temp, v, n, phase, dlnfugdt=None, dlnfugdp=None, dlnfugdn=None)`
Calculate logarithm of fugacity coefficient given molar numbers, temperature and pressure. Note that the order of the output match the default order of input for the differentials. Note further that dlnfugdt, dlnfugdp, dlnfugdn and ophase only are flags to enable calculation.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **v (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdt (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to temperature while pressure and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdp (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to pressure while temperature and composition are held constant. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **dlnfugdn (logical, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate fugacity coefficient differentials with respect to mol numbers while pressure and temperature are held constant. Defaults to None.

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  fugacity coefficient (-), and optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Flash interfaces

Methods for flash calculations.

### Table of contents
  * [Flash interfaces](#Flash-interfaces)
    * [guess_phase](#guess_phaseself-temp-press-z)
    * [set_ph_tolerance](#set_ph_toleranceself-tol)
    * [two_phase_phflash](#two_phase_phflashself-press-z-enthalpy-tempNone)
    * [two_phase_psflash](#two_phase_psflashself-press-z-entropy-tempNone)
    * [two_phase_tpflash](#two_phase_tpflashself-temp-press-z)
    * [two_phase_uvflash](#two_phase_uvflashself-z-specific_energy-specific_volume-tempNone-pressNone)


### `guess_phase(self, temp, press, z)`
If only one root exsist for the equation of state the phase type can be determined from either the psedo-critical volume or a volume ratio to the co-volume

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **int:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Phase int (VAPPH or LIQPH)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ph_tolerance(self, tol)`
Set tolerance of isobaric-isentalpic (PH) flash

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **tol (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Tolerance

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `two_phase_phflash(self, press, z, enthalpy, temp=None)`
Do isenthalpic-isobaric (HP) flash

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Overall molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **enthalpy (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific enthalpy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **FlashResult :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Struct holding the result of the flash (Temperature, phase fractions,

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; phase compositions and phase identifier)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `two_phase_psflash(self, press, z, entropy, temp=None)`
Do isentropic-isobaric (SP) flash

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Overall molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **entropy (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific entropy (J/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **FlashResult :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Struct holding the result of the flash (Temperature, phase fractions,

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; phase compositions and phase identifier)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `two_phase_tpflash(self, temp, press, z)`
Do isothermal-isobaric (TP) flash

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Overall molar composition

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **FlashResult :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Struct holding the result of the flash (phase fractions, phase compositions and phase identifier)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `two_phase_uvflash(self, z, specific_energy, specific_volume, temp=None, press=None)`
Do isoenergetic-isochoric (UV) flash

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Overall molar composition

&nbsp;&nbsp;&nbsp;&nbsp; **specific_energy (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific energy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **specific_volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific volume (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **press (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **FlashResult :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Struct holding the result of the flash (Temperature, pressure, phase fractions,

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; phase compositions and phase identifier)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Saturation interfaces

Bubble- and dew point calculations and phase envelopes.

### Table of contents
  * [Saturation interfaces](#Saturation-interfaces)
    * [bubble_pressure](#bubble_pressureself-temp-z)
    * [bubble_temperature](#bubble_temperatureself-press-z)
    * [dew_pressure](#dew_pressureself-temp-z)
    * [dew_temperature](#dew_temperatureself-press-z)
    * [get_binary_pxy](#get_binary_pxyself-temp-maximum_pressure150000000-minimum_pressure1000000-maximum_dz0003-maximum_dlns001)
    * [get_binary_txy](#get_binary_txyself-pressure-minimum_temperaturte00-maximum_dz0003-maximum_dlns0005)
    * [get_bp_term](#get_bp_termself-i_term)
    * [get_envelope_twophase](#get_envelope_twophaseself-initial_pressure-z-maximum_pressure150000000-minimum_temperatureNone-step_sizeNone-calc_vFalse)
    * [global_binary_plot](#global_binary_plotself-maximum_pressure150000000-minimum_pressure1000000-minimum_temperature1500-maximum_temperature5000-include_azeotropesFalse)
    * [solid_envelope_plot](#solid_envelope_plotself-initial_pressure-z-maximum_pressure150000000-minimum_temperature1700-calc_esvFalse)


### `bubble_pressure(self, temp, z)`
Calculate bubble pressure given temperature and composition

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Raises:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Exception:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Faild to calculate

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Incipient phase composition

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `bubble_temperature(self, press, z)`
Calculate bubble temperature given pressure and composition

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Raises:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Exception:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Faild to calculate

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Incipient phase composition

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `dew_pressure(self, temp, z)`
Calculate dew pressure given temperature and composition

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **z (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Compositon (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Raises:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Exception:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Not able to solve for dew point

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Incipient phase composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `dew_temperature(self, press, z)`
Calculate dew temperature given pressure and composition

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Compositon (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Raises:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Exception:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Not able to solve for dew point

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Incipient phase composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_binary_pxy(self, temp, maximum_pressure=15000000.0, minimum_pressure=100000.0, maximum_dz=0.003, maximum_dlns=0.01)`
Calculate binary three phase envelope

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on maximum pressure (Pa). Defaults to 1.5e7.

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on minimum pressure (Pa). Defaults to 1.0e5.

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_dz (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum composition step. Defaults to 0.003.

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_dlns (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **tuple of arrays:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  LLE, L1VE, L2VE

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **LLE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 1 - Liquid 2 Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[0] -> Liquid 1 composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[1] -> Liquid 2 composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; **L1VE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 1 - Vapour Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[0] -> Bubble line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[1] -> Dew line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; **L2VE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 2 - Vapour Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[0] -> Bubble line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[1] -> Dew line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; If one or more of the equilibria are not found the corresponding tuple is (None, None, None)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_binary_txy(self, pressure, minimum_temperaturte=0.0, maximum_dz=0.003, maximum_dlns=0.005)`
Calculate binary isobaric three phase envelope

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **pressure (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on minimum temperature (K).

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_dz (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum composition step. Defaults to 0.003.

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_dlns (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum step in most sensitive envelope variable (the specification variable), see `doc/memo/binaryxy` for details on usage. Defaults to 0.01.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **tuple of arrays:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  LLE, L1VE, L2VE

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **LLE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 1 - Liquid 2 Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[0] -> Liquid 1 composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[1] -> Liquid 2 composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; LLE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; **L1VE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 1 - Vapour Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[0] -> Bubble line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[1] -> Dew line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L1VE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; **L2VE :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Liquid 2 - Vapour Equilibrium

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[0] -> Bubble line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[1] -> Dew line composition (mole fraction of component 1)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; L2VE[2] -> Pressure [Pa]

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; If one or more of the equilibria are not found the corresponding tuple is (None, None, None)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_bp_term(self, i_term)`
Get error description for binary plot error

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i_term (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  binary plot error identifyer

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **str:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Error message

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_envelope_twophase(self, initial_pressure, z, maximum_pressure=15000000.0, minimum_temperature=None, step_size=None, calc_v=False)`
Get the phase-envelope at a given composition

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **initial_pressure (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Start mapping form dew point at initial pressure (Pa).

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float , optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on maximum pressure (Pa). Defaults to 1.5e7.

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float , optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on minimum pressure (Pa). Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **step_size (float , optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Tune step size of envelope trace. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **calc_v (bool, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate specifc volume of saturated phase? Defaults to False

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature values (K)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure values (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray (optional, if `calc_v=True`):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific volume (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `global_binary_plot(self, maximum_pressure=15000000.0, minimum_pressure=100000.0, minimum_temperature=150.0, maximum_temperature=500.0, include_azeotropes=False)`
Calculate global binary phase envelope

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on maximum pressure (Pa). Defaults to 1.5e7.

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on minimum pressure (Pa). Defaults to 1.0e5.

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Terminate phase line traceing at minimum temperature. Defaults to 150.0 K.

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Terminate phase line traceing at maximum temperature. Defaults to 500.0 K.

&nbsp;&nbsp;&nbsp;&nbsp; **include_azeotropes (bool, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include azeotropic lines. Defaults to False.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; tuple of arrays

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `solid_envelope_plot(self, initial_pressure, z, maximum_pressure=15000000.0, minimum_temperature=170.0, calc_esv=False)`
Calculate phase envelope including solid lines

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **initial_pressure (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Start mapping from initial pressure (Pa).

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float , optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exit on maximum pressure (Pa). Defaults to 1.5e7.

&nbsp;&nbsp;&nbsp;&nbsp; **calc_esv (bool, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate specifc volume of saturated phase? Defaults to False

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; tuple of arrays

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Isolines

Computing isolines.

### Table of contents
  * [Isolines](#Isolines)
    * [get_isenthalp](#get_isenthalpself-enthalpy-z-minimum_pressure1000000-maximum_pressure150000000-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isentrope](#get_isentropeself-entropy-z-minimum_pressure1000000-maximum_pressure150000000-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isobar](#get_isobarself-press-z-minimum_temperature2000-maximum_temperature5000-nmax100)
    * [get_isotherm](#get_isothermself-temp-z-minimum_pressure1000000-maximum_pressure150000000-nmax100)


### `get_isenthalp(self, enthalpy, z, minimum_pressure=100000.0, maximum_pressure=15000000.0, minimum_temperature=200.0, maximum_temperature=500.0, nmax=100)`
Get isenthalpic line at specified enthalpy. Use as `T, p, v, s = get_isenthalp(h, z)`, where `(T, p, v, s)` is the temperature, pressure, specific volume and specific entropy along the isenthalp with specific enthalpy `h` and molar composition `z`.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **enthalpy (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enthalpy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum pressure. Defaults to 1.0e5. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum pressure. Defaults to 1.5e7. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum temperature. Defaults to 200.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum temperature. Defaults to 500.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **nmax (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum number of points on isenthalp. Defaults to 100.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **(tuple of arrays) :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Corresponding to (temperature, pressure, specific volume, specific entropy) along the

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; isenthalp.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_isentrope(self, entropy, z, minimum_pressure=100000.0, maximum_pressure=15000000.0, minimum_temperature=200.0, maximum_temperature=500.0, nmax=100)`
Get isentrope at specified entropy. Use as `T, p, v, h = get_isenthalp(s, z)`, where `(T, p, v, h)` is the temperature, pressure, specific volume and specific enthalpy along the isentrope with specific entropy `s` and molar composition `z`.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **entropy (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Entropy (J/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum pressure. Defaults to 1.0e5. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum pressure. Defaults to 1.5e7. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum temperature. Defaults to 200.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum temperature. Defaults to 500.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **nmax (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum number of points on isentrope. Defaults to 100.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **(tuple of arrays) :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Corresponding to (temperature, pressure, specific volume, specific enthalpy) along the

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; isentrope.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_isobar(self, press, z, minimum_temperature=200.0, maximum_temperature=500.0, nmax=100)`
Get isobar at specified pressure. Use as `T, v, s, h = get_isobar(p, z)`, where `(T, v, s, h)` is the temperature, specific volume, specific entropy and specific enthalpy along the isobar with pressure `p` and molar composition `z`.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum temperature. Defaults to 200.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum temperature. Defaults to 500.0. (K)

&nbsp;&nbsp;&nbsp;&nbsp; **nmax (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum number of points on iso-bar. Defaults to 100.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **(tuple of arrays) :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Corresponding to (temperature, specific volume, specific entropy, specific enthalpy)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; along the isobar.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_isotherm(self, temp, z, minimum_pressure=100000.0, maximum_pressure=15000000.0, nmax=100)`
Get iso-therm at specified temperature

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **z (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Composition (-)

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Map to minimum pressure. Defaults to 1.0e5. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **maximum_pressure (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Map to maximum pressure. Defaults to 1.5e7. (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **nmax (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum number of points on iso-therm. Defaults to 100.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Multiple numpy arrays.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Stability interfaces

Critical point solver.

### Table of contents
  * [Stability interfaces](#Stability-interfaces)
    * [critical](#criticalself-n-temp00-v00-tol1e-07)
    * [critical_pressure](#critical_pressureself-i)
    * [critical_temperature](#critical_temperatureself-i)


### `critical(self, n, temp=0.0, v=0.0, tol=1e-07)`
Calculate critical point in variables T and V

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for temperature (K). Defaults to 0.0.

&nbsp;&nbsp;&nbsp;&nbsp; **v (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial guess for volume (m3). Defaults to 0.0.

&nbsp;&nbsp;&nbsp;&nbsp; **tol (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Error tolerance (-). Defaults to 1.0e-8.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Raises:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **Exception:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Failure to solve for critcal point

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `critical_pressure(self, i)`
Get critical pressure of component i

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  component FORTRAN index (first index is 1)

#### returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  critical pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `critical_temperature(self, i)`
Get critical temperature of component i

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  component FORTRAN index (first index is 1)

#### returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  critical temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Virial interfaces

Retrieve various virial coefficients.

### Table of contents
  * [Virial interfaces](#Virial-interfaces)
    * [binary_third_virial_matrix](#binary_third_virial_matrixself-temp)
    * [second_virial_matrix](#second_virial_matrixself-temp)
    * [virial_coeffcients](#virial_coeffcientsself-temp-n)


### `binary_third_virial_matrix(self, temp)`
Calculate composition-independent virial coefficients C, defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0. Including cross coefficients Currently the code only support binary mixtures

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  C - Third virial coefficient matrix (m6/mol2)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `second_virial_matrix(self, temp)`
Calculate composition-independent virial coefficients B, defined as P = RT*rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0. Including cross coefficients.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  B - Second virial coefficient matrix (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `virial_coeffcients(self, temp, n)`
Calculate (composition-dependent) virial coefficients B and C, defined as P/RT = rho + B*rho**2 + C*rho**3 + O(rho**4) as rho->0.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  B (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  C (m6/mol2)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Joule-Thompson interface

Joule-Thompson inversion curves.

### Table of contents
  * [Joule-Thompson interface](#Joule-Thompson-interface)
    * [joule_thompson_inversion](#joule_thompson_inversionself-z-nmax1000)


### `joule_thompson_inversion(self, z, nmax=1000)`
Calculate Joule-Thompson inversion curve

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **z (array like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Compozition

&nbsp;&nbsp;&nbsp;&nbsp; **nmax (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Array size

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  temp - Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  press - Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  vol - Volume (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Methods for setting ... and getting ...

### Table of contents
  * [Utility methods](#Utility-methods)
    * [acentric_factor](#acentric_factorself-i)
    * [compmoleweight](#compmoleweightself-comp)
    * [get_comp_name](#get_comp_nameself-index)
    * [get_ideal_enthalpy_reference_value](#get_ideal_enthalpy_reference_valueself-j)
    * [get_ideal_entropy_reference_value](#get_ideal_entropy_reference_valueself-j)
    * [get_phase_flags](#get_phase_flagsself)
    * [get_phase_type](#get_phase_typeself-i_phase)
    * [get_pmax](#get_pmaxself)
    * [get_pmin](#get_pminself)
    * [get_tmax](#get_tmaxself)
    * [get_tmin](#get_tminself)
    * [getcompindex](#getcompindexself-comp)
    * [redefine_critical_parameters](#redefine_critical_parametersself-silentTrue-Tc_initialsNone-vc_initialsNone)
    * [set_ideal_enthalpy_reference_value](#set_ideal_enthalpy_reference_valueself-j-h0)
    * [set_ideal_entropy_reference_value](#set_ideal_entropy_reference_valueself-j-s0)
    * [set_pmax](#set_pmaxself-press)
    * [set_pmin](#set_pminself-press)
    * [set_tmax](#set_tmaxself-temp)
    * [set_tmin](#set_tminself-temp)


### `acentric_factor(self, i)`
Get acentric factor of component i Args: i (int) component FORTRAN index returns: float: acentric factor 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `compmoleweight(self, comp)`
Get component mole weight (g/mol)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comp (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component FORTRAN index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component mole weight (g/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_comp_name(self, index)`
Get component name

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **index (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component FORTRAN index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **comp (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component name

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_ideal_enthalpy_reference_value(self, j)`
Get specific ideal enthalpy reference value

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **j (integer):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific ideal enthalpy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_ideal_entropy_reference_value(self, j)`
Get specific ideal entropy reference value

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **j (integer):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Specific ideal entropy (J/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_phase_flags(self)`
Get phase identifiers used by thermopack

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **int:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Phase int  identifiers

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_phase_type(self, i_phase)`
Get phase type

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i_phase (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Phase flag returned by thermopack

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **str:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Phase type

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_pmax(self)`
Get minimum pressure in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 100 MPa.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_pmin(self)`
Get minimum pressure in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 10 Pa.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_tmax(self)`
Get maximum temperature in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 999 K.

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_tmin(self)`
Get minimum temperature in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 80 K.

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `getcompindex(self, comp)`
Get component index

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comp (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component name

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **int:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component FORTRAN index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `redefine_critical_parameters(self, silent=True, Tc_initials=None, vc_initials=None)`
Recalculate critical properties of pure fluids

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **silent (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Ignore warnings? Defaults to True

&nbsp;&nbsp;&nbsp;&nbsp; **Tc_initials (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial value for pure fluid critical temperatures (K). Negative values will trigger use of SRK values from data base.

&nbsp;&nbsp;&nbsp;&nbsp; **vc_initials (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Initial value for pure fluid critical volumes (m3/mol). Negative values will trigger use of SRK values from data base.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ideal_enthalpy_reference_value(self, j, h0)`
Set specific ideal enthalpy reference value

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **j (integer):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **h0 (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Ideal enthalpy reference (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ideal_entropy_reference_value(self, j, s0)`
Set specific ideal entropy reference value

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **j (integer):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **s0 (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Ideal entropy reference (J/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pmax(self, press)`
Set minimum pressure in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 100 MPa.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pmin(self, press)`
Set minimum pressure in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 10 Pa.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **press (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Pressure (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_tmax(self, temp)`
Set maximum temperature in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 999 K.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_tmin(self, temp)`
Set minimum temperature in Thermopack. Used to limit search domain for numerical solvers. Default value set on init is 80 K.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Internal methods

Methods for handling communication with the Fortran library.

### Table of contents
  * [Internal methods](#Internal-methods)
    * [\_\_del\_\_](#__del__self)
    * [\_\_init\_\_](#__init__self)
    * [activate](#activateself)
    * [add_eos](#add_eosself)
    * [delete_eos](#delete_eosself)
    * [get_export_name](#get_export_nameself-module-method)
    * [get_model_id](#get_model_idself)
    * [init_peneloux_volume_translation](#init_peneloux_volume_translationself-parameter_referenceDefault)
    * [init_solid](#init_solidself-scomp)
    * [init_thermo](#init_thermoself-eos-mixing-alpha-comps-nphases-liq_vap_discr_methodNone-csp_eosNone-csp_ref_compNone-kij_refDefault-alpha_refDefault-saft_refDefault-b_exponentNone-TrendEosForCpNone-cptypeNone-silentNone)


### `__del__(self)`
Delete FORTRAN memory allocated for this instance 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `__init__(self)`
Load libthermopack.(so/dll) and initialize function pointers 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `activate(self)`
Activate this instance of thermopack parameters for calculation 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `add_eos(self)`
Allocate FORTRAN memory for this class instance 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `delete_eos(self)`
de-allocate FORTRAN memory for this class instance 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_export_name(self, module, method)`
Generate library export name based on module and method name

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **module (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Name of module

&nbsp;&nbsp;&nbsp;&nbsp; **method (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Name of method

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **str:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Library export name

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_model_id(self)`
Get model identification

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **str:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Eos name

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init_peneloux_volume_translation(self, parameter_reference='Default')`
Initialialize Peneloux volume translations

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  String defining parameter set, Defaults to "Default"

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init_solid(self, scomp)`
Initialize pure solid

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **scomp (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component name

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init_thermo(self, eos, mixing, alpha, comps, nphases, liq_vap_discr_method=None, csp_eos=None, csp_ref_comp=None, kij_ref='Default', alpha_ref='Default', saft_ref='Default', b_exponent=None, TrendEosForCp=None, cptype=None, silent=None)`
Initialize thermopack

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation of state

&nbsp;&nbsp;&nbsp;&nbsp; **mixing (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mixture model for cubic eos

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Alpha formulations for cubic EOS

&nbsp;&nbsp;&nbsp;&nbsp; **comps (string):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of components

&nbsp;&nbsp;&nbsp;&nbsp; **nphases (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Maximum number of phases considered during multi-phase flash calculations

&nbsp;&nbsp;&nbsp;&nbsp; **liq_vap_discr_method (int, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Method to discriminate between liquid and vapor in case of an undefined single phase. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **csp_eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Corrensponding state equation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **csp_ref_comp (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  CSP reference component. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **kij_ref (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Data set identifiers. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ref (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Data set identifiers. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **saft_ref (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Data set identifiers. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **b_exponent (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Exponent used in co-volume mixing. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **TrendEosForCp (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Option to init trend for ideal gas properties. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **cptype (int array, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation type number for Cp. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **silent (bool, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Supress messages during init?. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

