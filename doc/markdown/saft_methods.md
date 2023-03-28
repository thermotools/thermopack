<!--- 
Generated at: 2023-03-28T18:08:02.603454
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
saft class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

# Methods in the saft class (`saft.py`)

The `saft` class, found in `addon/pycThermopack/saft.py`, is an "abstract" class, that is inherited
by the `saftvrmie`, `pcsaft` and `saftvrqmie` classes. It contains some generic utility methods to
compute quantities of interest when investigating SAFT-type equations of state.

## Table of contents
  * [Utility methods](#Utility-methods)
    * [a_dispersion](#a_dispersionself-temp-volume-n-a_tNone-a_vNone-a_nNone-a_ttNone-a_vvNone-a_tvNone-a_tnNone-a_vnNone-a_nnNone)
    * [de_broglie_wavelength](#de_broglie_wavelengthself-c-temp)
    * [fres_polar](#fres_polarself-temp-volume-n-qqTrue-ddTrue-dqTrue)
    * [hard_sphere_diameters](#hard_sphere_diametersself-temp)
    * [polar_model_control](#polar_model_controlself-qq-dd-dq)
    * [print_saft_parameters](#print_saft_parametersself-c)
  * [Internal methods](#Internal-methods)
    * [\_\_init\_\_](#__init__self)

## Utility methods

Methods for computing specific parameters and contributions to the residual
Helmholtz energy for SAFT-type equations of state

### Table of contents
  * [Utility methods](#Utility-methods)
    * [a_dispersion](#a_dispersionself-temp-volume-n-a_tNone-a_vNone-a_nNone-a_ttNone-a_vvNone-a_tvNone-a_tnNone-a_vnNone-a_nnNone)
    * [de_broglie_wavelength](#de_broglie_wavelengthself-c-temp)
    * [fres_polar](#fres_polarself-temp-volume-n-qqTrue-ddTrue-dqTrue)
    * [hard_sphere_diameters](#hard_sphere_diametersself-temp)
    * [polar_model_control](#polar_model_controlself-qq-dd-dq)
    * [print_saft_parameters](#print_saft_parametersself-c)


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

### `hard_sphere_diameters(self, temp)`
Calculate hard-sphere diameters given temperature, volume and mol numbers.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **float:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Hard-sphere diameter (m)

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

### `print_saft_parameters(self, c)`
Print saft parameters for component c

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Internal methods

Methods for handling communication with the Fortran library.

### Table of contents
  * [Internal methods](#Internal-methods)
    * [\_\_init\_\_](#__init__self)


### `__init__(self)`
Initialize saft specific function pointers 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

