---
layout: default
version: 2.1.0
title: Methods in the saft class
permalink: /v2.1.0/saft_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.063440
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
saft class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `saft` class, found in `addon/pycThermopack/thermopack/saft.py`, is an "abstract" class, that is inherited
by the `saftvrmie`, `pcsaft` and `saftvrqmie` classes. It contains some generic utility methods to
compute quantities of interest when investigating SAFT-type equations of state.

## Table of contents
  * [Utility methods](#utility-methods)
    * [a_dispersion](#a_dispersionself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [a_hard_sphere](#a_hard_sphereself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [a_soft_repulsion](#a_soft_repulsionself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [adjust_mass_to_de_boer_parameter](#adjust_mass_to_de_boer_parameterself-c-de_boer)
    * [alpha](#alphaself-temperature)
    * [calc_bmcsl_gij_fmt](#calc_bmcsl_gij_fmtself-n_alpha-mu_ij-calc_g_ij_nfalse-mu_ij_tnone)
    * [de_boer_parameter](#de_boer_parameterself-c)
    * [de_broglie_wavelength](#de_broglie_wavelengthself-c-temp)
    * [epsilon_eff_ij](#epsilon_eff_ijself-i-j-temperature)
    * [epsilon_ij](#epsilon_ijself-i-j)
    * [fmt_energy_density](#fmt_energy_densityself-n_alpha-phi_nfalse-phi_nnfalse-fmt_modelwb)
    * [fres_polar](#fres_polarself-temp-volume-n-qqtrue-ddtrue-dqtrue)
    * [hard_sphere_diameter_ij](#hard_sphere_diameter_ijself-i-j-temp)
    * [hard_sphere_diameters](#hard_sphere_diametersself-temp)
    * [polar_model_control](#polar_model_controlself-qq-dd-dq)
    * [potential](#potentialself-ic-jc-r-temp)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [sigma_eff_ij](#sigma_eff_ijself-i-j-temperature)
    * [sigma_ij](#sigma_ijself-i-j)
    * [test_fmt_compatibility](#test_fmt_compatibilityself)
    * [truncation_correction](#truncation_correctionself-enable_truncation_correction-enable_shift_correction-reduced_radius_cut35)
  * [Internal methods](#internal-methods)
    * [\_\_init\_\_](#__init__self)

## Utility methods

Methods for computing specific parameters and contributions to the residual
Helmholtz energy for SAFT-type equations of state

### Table of contents
  * [Utility methods](#utility-methods)
    * [a_dispersion](#a_dispersionself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [a_hard_sphere](#a_hard_sphereself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [a_soft_repulsion](#a_soft_repulsionself-temp-volume-n-a_tnone-a_vnone-a_nnone-a_ttnone-a_vvnone-a_tvnone-a_tnnone-a_vnnone-a_nnnone)
    * [adjust_mass_to_de_boer_parameter](#adjust_mass_to_de_boer_parameterself-c-de_boer)
    * [alpha](#alphaself-temperature)
    * [calc_bmcsl_gij_fmt](#calc_bmcsl_gij_fmtself-n_alpha-mu_ij-calc_g_ij_nfalse-mu_ij_tnone)
    * [de_boer_parameter](#de_boer_parameterself-c)
    * [de_broglie_wavelength](#de_broglie_wavelengthself-c-temp)
    * [epsilon_eff_ij](#epsilon_eff_ijself-i-j-temperature)
    * [epsilon_ij](#epsilon_ijself-i-j)
    * [fmt_energy_density](#fmt_energy_densityself-n_alpha-phi_nfalse-phi_nnfalse-fmt_modelwb)
    * [fres_polar](#fres_polarself-temp-volume-n-qqtrue-ddtrue-dqtrue)
    * [hard_sphere_diameter_ij](#hard_sphere_diameter_ijself-i-j-temp)
    * [hard_sphere_diameters](#hard_sphere_diametersself-temp)
    * [polar_model_control](#polar_model_controlself-qq-dd-dq)
    * [potential](#potentialself-ic-jc-r-temp)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [sigma_eff_ij](#sigma_eff_ijself-i-j-temperature)
    * [sigma_ij](#sigma_ijself-i-j)
    * [test_fmt_compatibility](#test_fmt_compatibilityself)
    * [truncation_correction](#truncation_correctionself-enable_truncation_correction-enable_shift_correction-reduced_radius_cut35)


### `a_dispersion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None, a_tv=None, a_tn=None, a_vn=None, a_nn=None)`
Calculate dispersion contribution given temperature, volume and mol numbers. $a = A_{disp}/(nRT)$

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **a_t (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_v (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_n (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_vv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_vn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_nn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarry:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `a_hard_sphere(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None, a_tv=None, a_tn=None, a_vn=None, a_nn=None)`
Calculate hard-sphere contribution given temperature, volume and mol numbers. a = A_hs/(nRT) Args: temp (float): Temperature (K) volume (float): Volume (m3) n (array_like): Mol numbers (mol) a_t (No type, optional): Flag to activate calculation. Defaults to None. a_v (No type, optional): Flag to activate calculation. Defaults to None. a_n (No type, optional): Flag to activate calculation. Defaults to None. a_tt (No type, optional): Flag to activate calculation. Defaults to None. a_vv (No type, optional): Flag to activate calculation. Defaults to None. a_tv (No type, optional): Flag to activate calculation. Defaults to None. a_tn (No type, optional): Flag to activate calculation. Defaults to None. a_vn (No type, optional): Flag to activate calculation. Defaults to None. a_nn (No type, optional): Flag to activate calculation. Defaults to None.

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarry:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `a_soft_repulsion(self, temp, volume, n, a_t=None, a_v=None, a_n=None, a_tt=None, a_vv=None, a_tv=None, a_tn=None, a_vn=None, a_nn=None)`
Calculate soft repuslion contribution given temperature, volume and mol numbers. a = A_soft_rep/(nRT)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **a_t (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_v (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_n (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_vv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_tn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_vn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **a_nn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarry:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `adjust_mass_to_de_boer_parameter(self, c, de_boer)`
Adjust mass in order to get specified de Boer parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **de_boer (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  de Boer parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `alpha(self, temperature)`
Get dimensionless van der Waals energy

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temperature (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (ndarray):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Dimensionless van der Waals energy (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `calc_bmcsl_gij_fmt(self, n_alpha, mu_ij, calc_g_ij_n=False, mu_ij_T=None)`
Calculate g_ij at contact according to Yu and Wu: 10.1063/1.1463435

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **n_alpha (np.ndarray):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Weighted densities (n0, n1, n2, n3, nV1, nV2)

&nbsp;&nbsp;&nbsp;&nbsp; **mu_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  mu_ij = d_i*d_j/(d_i+d_j)

&nbsp;&nbsp;&nbsp;&nbsp; **mu_ij_T (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature differential of mu_ij

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  g_ij

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  g_ij_n

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  g_ij_T

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `de_boer_parameter(self, c)`
Get de Boer parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **Results:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **de_boer (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  de Boer parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `de_broglie_wavelength(self, c, temp)`
Calculate de Broglie wavelength

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  de Broglie wavelength (m)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `epsilon_eff_ij(self, i, j, temperature)`
Effective well depth divided by Boltzmann constant for i-j interaction for Feynman-Hibbs corrected Mie potentials. For classical (not quantum-corrected models), returns the sigma parameter.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **temperature (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **Results:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **epsilon_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Effective well depth divided by Boltzmann constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `epsilon_ij(self, i, j)`
Well depth divided by Boltzmann constant for i-j interaction

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **Results:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **epsilon_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `fmt_energy_density(self, n_alpha, phi_n=False, phi_nn=False, fmt_model='WB')`
Calculate FMT reduced energy density

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **n_alpha (np.ndarray):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Weighted densities (n0, n1, n2, n3, nV1, nV2) for the entire grid

&nbsp;&nbsp;&nbsp;&nbsp; **phi_n (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate first order differetnials?

&nbsp;&nbsp;&nbsp;&nbsp; **phi_nn (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate second order differetnials?

&nbsp;&nbsp;&nbsp;&nbsp; **fmt_model (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  FMT model (RF (Rosenfeld), WB (White Bear), WBII (White Bear Mark II))

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  phi

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  phi_n

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  phi_nn

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `fres_polar(self, temp, volume, n, qq=True, dd=True, dq=True)`
Calculate reduced Helmholtz energy contribution from polar model

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **qq (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include quadrupole-quadrupole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; **dd (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include dipole-dipole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; **dq (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include dipole-quadrupole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reduced Helmholtz energy contribution (mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `hard_sphere_diameter_ij(self, i, j, temp)`
Calculate non-additive hard-sphere diameter for i-j interaction given temperature.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Hard-sphere diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature differential of hard-sphere diameter (m/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `hard_sphere_diameters(self, temp)`
Calculate hard-sphere diameters given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Hard-sphere diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature differential of hard-sphere diameter (m/K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `polar_model_control(self, qq, dd, dq)`
Dictate what terms are included with for polar model

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **qq (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include quadrupole-quadrupole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; **dd (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include dipole-dipole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; **dq (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Include dipole-quadrupole contribution?

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `potential(self, ic, jc, r, temp)`
Get potential energy divided by Boltzmann constant as a function of r

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic, jc (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component indices (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **r (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Distance between particles (m)

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **array_like:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Potential energy divided by Boltzmann constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `print_saft_parameters(self, c)`
Print saft parameters for component c

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `sigma_eff_ij(self, i, j, temperature)`
Get effective size parameter for i-j interaction for Feynman-Hibbs corrected Mie potentials. For classical (not quantum-corrected models), returns the sigma parameter.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **temperature (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **Results:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **sigma_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Size paramater (m)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `sigma_ij(self, i, j)`
Get size parameter for i-j interaction

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **j (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **Results:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; **sigma_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Size paramater (m)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `test_fmt_compatibility(self)`
Test if model setup is comaptible with the Fundamental Measure Theory (FMT)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **bool:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Is model FMT consistent?

&nbsp;&nbsp;&nbsp;&nbsp; **bool:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Is non-additive hard-sphere term used?

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `truncation_correction(self, enable_truncation_correction, enable_shift_correction, reduced_radius_cut=3.5)`
Enable/disable truncation corrections

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **enable_truncation_correction (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable long range truncation correction

&nbsp;&nbsp;&nbsp;&nbsp; **enable_shift_correction (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable potential shift correction

&nbsp;&nbsp;&nbsp;&nbsp; **reduced_radius_cut (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reduced length cut-off

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Internal methods

Methods for handling communication with the Fortran library.

### Table of contents
  * [Internal methods](#internal-methods)
    * [\_\_init\_\_](#__init__self)


### `__init__(self)`
Initialize SAFT specific function pointers 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

