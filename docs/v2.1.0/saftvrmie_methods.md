---
layout: default
version: 2.1.0
title: Methods in the saftvrmie class
permalink: /v2.1.0/saftvrmie_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.065297
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
saftvrmie class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `saftvrmie` class, found in `addon/pycThermopack/thermopack/saftvrmie.py`, is the interface to the 
SAFT-VR Mie Equation of State. This class inherits the `saft` class, which in turn inherits the
`thermo` class. This class implements methods for modifying fluid parameters and which terms
are included in the model.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-parameter_referencedefault)
    * [init](#initself-comps-parameter_referencedefault)
  * [Utility methods](#utility-methods)
    * [a1](#a1self-temp-volume-n)
    * [a2](#a2self-temp-volume-n)
    * [a3](#a3self-temp-volume-n)
    * [a_hs](#a_hsself-temp-volume-n)
    * [get_eps_kij](#get_eps_kijself-c1-c2)
    * [get_lr_gammaij](#get_lr_gammaijself-c1-c2)
    * [get_pure_fluid_param](#get_pure_fluid_paramself-ic)
    * [get_sigma_lij](#get_sigma_lijself-c1-c2)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [set_eps_kij](#set_eps_kijself-c1-c2-kij)
    * [set_lr_gammaij](#set_lr_gammaijself-c1-c2-gammaij)
    * [set_pure_fluid_param](#set_pure_fluid_paramself-ic-m-sigma-eps_div_kb-lambda_a-lambda_r)
    * [set_sigma_lij](#set_sigma_lijself-c1-c2-lij)
  * [Model control](#model-control)
    * [model_control_a1](#model_control_a1self-active)
    * [model_control_a2](#model_control_a2self-active)
    * [model_control_a3](#model_control_a3self-active)
    * [model_control_chain](#model_control_chainself-active)
    * [model_control_hard_sphere](#model_control_hard_sphereself-active)
    * [set_hard_sphere_reference](#set_hard_sphere_referenceself-reference-exact_binary_dhsnone-enable_hs_extranone)
  * [Model performance](#model-performance)
    * [enable_temperature_cache](#enable_temperature_cacheself-enabletrue)

## Constructor

Methods to initialise SAFT-VR Mie model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-parameter_referencedefault)
    * [init](#initself-comps-parameter_referencedefault)


### `__init__(self, comps=None, parameter_reference='Default')`
Initialize SAFT-VR Mie model in thermopack. If no components are specified, model must be initialized for specific components later by direct call to 'init' Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, parameter_reference='Default')`
Initialize SAFT-VR Mie model in thermopack

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Set- and get methods for interaction parameters and pure fluid parameters.

### Table of contents
  * [Utility methods](#utility-methods)
    * [a1](#a1self-temp-volume-n)
    * [a2](#a2self-temp-volume-n)
    * [a3](#a3self-temp-volume-n)
    * [a_hs](#a_hsself-temp-volume-n)
    * [get_eps_kij](#get_eps_kijself-c1-c2)
    * [get_lr_gammaij](#get_lr_gammaijself-c1-c2)
    * [get_pure_fluid_param](#get_pure_fluid_paramself-ic)
    * [get_sigma_lij](#get_sigma_lijself-c1-c2)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [set_eps_kij](#set_eps_kijself-c1-c2-kij)
    * [set_lr_gammaij](#set_lr_gammaijself-c1-c2-gammaij)
    * [set_pure_fluid_param](#set_pure_fluid_paramself-ic-m-sigma-eps_div_kb-lambda_a-lambda_r)
    * [set_sigma_lij](#set_sigma_lijself-c1-c2-lij)


### `a1(self, temp, volume, n)`
Get a1 term

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **a1 (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  First order perturbation (K/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `a2(self, temp, volume, n)`
Get a2 term

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **a2 (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Second order perturbation (K^2/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `a3(self, temp, volume, n)`
Get a3 term

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **a3 (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Second order perturbation (K^3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `a_hs(self, temp, volume, n)`
Get hardsphere term

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **temp (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature (K)

&nbsp;&nbsp;&nbsp;&nbsp; **volume (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume (m3)

&nbsp;&nbsp;&nbsp;&nbsp; **n (array like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mol numbers (mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **a_hs (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Second order perturbation (1/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_eps_kij(self, c1, c2)`
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

### `get_lr_gammaij(self, c1, c2)`
Get the interaction parameter gammaij for the lambda_r combining rule

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **gammaij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Repulsive exponent interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_pure_fluid_param(self, ic)`
Set pure fluid parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Returns;

&nbsp;&nbsp;&nbsp;&nbsp; **m (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mean number of segments.

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature-independent segment diameter [m].

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_k (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's const. [K].

&nbsp;&nbsp;&nbsp;&nbsp; **lambda_a (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Attractive exponent of the Mie potential

&nbsp;&nbsp;&nbsp;&nbsp; **lambda_r (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Repulsive exponent of the Mie potential

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_sigma_lij(self, c1, c2)`
Get the interaction parameter lij for the sigma combining rule (controlling non-additivity)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **lij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Sigma interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `print_saft_parameters(self, c)`
Print saft parameters for component c

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_eps_kij(self, c1, c2, kij)`
Set binary well depth interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **kij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_lr_gammaij(self, c1, c2, gammaij)`
Set the interaction parameter gammaij for the lambda_r combining rule

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **gammaij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Repulsive exponent interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pure_fluid_param(self, ic, m, sigma, eps_div_kb, lambda_a, lambda_r)`
Set pure fluid parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **m (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mean number of segments.

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Temperature-independent segment diameter [m].

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_kb (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's const. [K].

&nbsp;&nbsp;&nbsp;&nbsp; **lambda_a (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Attractive exponent of the Mie potential

&nbsp;&nbsp;&nbsp;&nbsp; **lambda_r (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Repulsive exponent of the Mie potential

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_sigma_lij(self, c1, c2, lij)`
Set the interaction parameter lij for the sigma combining rule (controlling non-additivity)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **lij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Sigma interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Model control

Control which contributions to the residual Helmholtz energy are included,
and the hard-sphere reference term.

### Table of contents
  * [Model control](#model-control)
    * [model_control_a1](#model_control_a1self-active)
    * [model_control_a2](#model_control_a2self-active)
    * [model_control_a3](#model_control_a3self-active)
    * [model_control_chain](#model_control_chainself-active)
    * [model_control_hard_sphere](#model_control_hard_sphereself-active)
    * [set_hard_sphere_reference](#set_hard_sphere_referenceself-reference-exact_binary_dhsnone-enable_hs_extranone)


### `model_control_a1(self, active)`
Enable/disable first dispersion term.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **active (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable first dispersion term

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `model_control_a2(self, active)`
Enable/disable second dispersion term.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **active (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable second dispersion term

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `model_control_a3(self, active)`
Enable/disable third dispersion term.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **active (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable third dispersion term

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `model_control_chain(self, active)`
Enable/disable chain term.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **active (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable chain term

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `model_control_hard_sphere(self, active)`
Enable/disable hard-sphere term.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **active (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable hard-sphere dispersion term

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_hard_sphere_reference(self, reference, exact_binary_dhs=None, enable_hs_extra=None)`
Set hard-sphere reference.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **reference (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  "LAFITTE", "ADDITIVE", "NON-ADDITIVE"

&nbsp;&nbsp;&nbsp;&nbsp; **exact_binary_dhs (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Calculate d_ij from sigma_ij and epsilon_ij

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; or simply as d_ij = (d_ii + d_jj)/2

&nbsp;&nbsp;&nbsp;&nbsp; **enable_hs_extra (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Correction of A_HS due to non-additive d_ij

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Model performance

Methods to tune computation efficiency etc.

### Table of contents
  * [Model performance](#model-performance)
    * [enable_temperature_cache](#enable_temperature_cacheself-enabletrue)


### `enable_temperature_cache(self, enable=True)`
Enable/disable temperature cache.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **enable (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Enable/disable temperature cache

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

