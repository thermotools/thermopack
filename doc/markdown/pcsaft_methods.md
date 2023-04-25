<!--- 
Generated at: 2023-03-28T18:31:24.714733
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
pcsaft class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

# Methods in the pcsaft class (`pcsaft.py`)

The `pcsaft` class, found in `addon/pycThermopack/pcsaft.py`, is the interface to the 
PC-SAFT Equation of State. This class inherits the `saft` class, which in turn inherits the
`thermo` class. This class implements utility methods to access mixing parameters etc.

## Table of contents
  * [Constructor](#Constructor)
    * [\_\_init\_\_](#__init__self-compsNone-parameter_referenceDefault-simplifiedFalse-polarFalse)
    * [init](#initself-comps-parameter_referenceDefault-simplifiedFalse-polarFalse)
  * [Utility methods](#Utility-methods)
    * [get_kij](#get_kijself-c1-c2)
    * [get_pure_params](#get_pure_paramsself-c)
    * [lng_ii](#lng_iiself-temp-volume-n-i-lng_tNone-lng_vNone-lng_nNone-lng_ttNone-lng_vvNone-lng_tvNone-lng_tnNone-lng_vnNone-lng_nnNone)
    * [set_kij](#set_kijself-c1-c2-kij)
    * [set_pure_params](#set_pure_paramsself-c-m-sigma-eps_div_kb-eps00-beta00)

## Constructor

Methods to initialise PC-SAFT model.

### Table of contents
  * [Constructor](#Constructor)
    * [\_\_init\_\_](#__init__self-compsNone-parameter_referenceDefault-simplifiedFalse-polarFalse)
    * [init](#initself-comps-parameter_referenceDefault-simplifiedFalse-polarFalse)


### `__init__(self, comps=None, parameter_reference='Default', simplified=False, polar=False)`
Initialize PC-SAFT model in thermopack If no components are specified, model must be initialized for specific components later by direct call to 'init'. Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **simplified (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use simplified PC-SAFT (sPC-SAFT: 10.1021/ie020753p) (Default False)

&nbsp;&nbsp;&nbsp;&nbsp; **polar (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use dipole and quadrupole contributions PCP-SAFT (10.1002/aic.10502, 10.1002/aic.10683 and 10.1021/jp072619u) (Default False)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, parameter_reference='Default', simplified=False, polar=False)`
Initialize PC-SAFT model in thermopack

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **simplified (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use simplified PC-SAFT (sPC-SAFT: 10.1021/ie020753p) (Default False)

&nbsp;&nbsp;&nbsp;&nbsp; **polar (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use dipole and quadrupole contributions PCP-SAFT (10.1002/aic.10502, 10.1002/aic.10683 and 10.1021/jp072619u) (Default True)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Set- and get methods for interaction parameters and pure fluid parameters.

### Table of contents
  * [Utility methods](#Utility-methods)
    * [get_kij](#get_kijself-c1-c2)
    * [get_pure_params](#get_pure_paramsself-c)
    * [lng_ii](#lng_iiself-temp-volume-n-i-lng_tNone-lng_vNone-lng_nNone-lng_ttNone-lng_vvNone-lng_tvNone-lng_tnNone-lng_vnNone-lng_nnNone)
    * [set_kij](#set_kijself-c1-c2-kij)
    * [set_pure_params](#set_pure_paramsself-c-m-sigma-eps_div_kb-eps00-beta00)


### `get_kij(self, c1, c2)`
Get binary well depth interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **kij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_pure_params(self, c)`
Get pure fluid PC-SAFT parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **m (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mean number of segments

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Segment diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_kb (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; **eps (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Association energy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **beta (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Association volume (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `lng_ii(self, temp, volume, n, i, lng_t=None, lng_v=None, lng_n=None, lng_tt=None, lng_vv=None, lng_tv=None, lng_tn=None, lng_vn=None, lng_nn=None)`
Calculate logarithm of the radial distribution function at contact given temperature, volume and mol numbers. Differentials are computed as functions of (T, V, n).

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

&nbsp;&nbsp;&nbsp;&nbsp; **i (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  FORTRAN component index

&nbsp;&nbsp;&nbsp;&nbsp; **lng_t (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_v (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_n (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_tt (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_vv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_tv (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_tn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_vn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; **lng_nn (No type, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Flag to activate calculation. Defaults to None.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ndarry:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Optionally differentials

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_kij(self, c1, c2, kij)`
Set binary well depth interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **kij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pure_params(self, c, m, sigma, eps_div_kb, eps=0.0, beta=0.0)`
Set pure fluid PC-SAFT parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; **m (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mean number of segments

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Segment diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_kb (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; **eps (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Association energy (J/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **beta (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Association volume (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 
