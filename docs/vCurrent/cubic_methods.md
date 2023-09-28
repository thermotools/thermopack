---
layout: default
version: 
title: Methods in the cubic class
permalink: /vcurrent/cubic_methods.html
---

<!--- 
Generated at: 2023-09-28T21:56:34.121404
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
cubic class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `cubic` class, found in `addon/pycThermopack/thermopack/cubic.py`, is the interface to the 
Cubic Equation of State. This class implements utility methods to access mixing parameters etc.

The sections [Initialiser keys](#initialiser-keys), [Pure fluid &alpha;](#pure-fluid-&alpha;), [&alpha; mixing rules](#&alpha;-mixing-rules) and [&beta; mixing rules](#&beta;-mixing-rules) summarise the various valid input keys that can be used to modify mixing rules, the &alpha -parameter and the underlying EoS.

Documentation for the methods in the cubic class is found in the remaining sections, summarised in the table of contents below.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init](#initself-comps-eos-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
  * [Utility methods](#utility-methods)
    * [get_ci](#get_ciself-cidx)
    * [get_covolumes](#get_covolumesself)
    * [get_energy_constants](#get_energy_constantsself)
    * [get_hv_param](#get_hv_paramself-c1-c2)
    * [get_kij](#get_kijself-c1-c2)
    * [get_lij](#get_lijself-c1-c2)
    * [get_ws_param](#get_ws_paramself-c1-c2)
    * [set_ci](#set_ciself-cidx-cia-cib00-cic00-ci_type1)
    * [set_hv_param](#set_hv_paramself-c1-c2-alpha_ij-alpha_ji-a_ij-a_ji-b_ij-b_ji-c_ij-c_ji)
    * [set_kij](#set_kijself-c1-c2-kij)
    * [set_lij](#set_lijself-c1-c2-lij)
    * [set_ws_param](#set_ws_paramself-c1-c2-alpha_ij-alpha_ji-k_ij-k_ji-tau_ij-tau_ji)

<!---This file (`cubic_keys.md`) is prepended when generating the file `cubic_methods.md`, 
and is not intended to be rendered on its own--->

# Initialiser keys

| Model                    | Input key   |
| ------------------------ | ----------- |
| Van der Waal             | VdW         |
| Soave Redlich Kwong      | SRK         |
| Peng Robinson            | PR          |
| Schmidt-Wensel           | SW          |
| Patel Teja               | PT          |
| Translated consistent PR | tcPR        |


## Pure fluid &alpha;

| Model                    | Input key   |
| ------------------------ | ----------- |
| Model default (*)        | Classic         |
| Twu-Coon-Bluck-Cunninghan       | TWU         |
| Mathias-Copeman | MC          |
| [Graboski and Daubert](https://doi.org/10.1021/i260068a009) | GD           |
| [Redlich-Kwong](https://doi.org/10.1021/cr60137a013)              | RK           |
| Soave | Soave        |
| [Peng Robinson 76](https://doi.org/10.1021/i160057a011)           | PR          |
| [UMR &alpha; formulation](https://doi.org/10.1021/ie049580p)           | UMR          |
| Peng Robinson 78           | PR78          |
| Van der Waal             | VdW         |
| Schmidt-Wensel           | SW          |
| Patel Teja               | PT          |

(*) Will use original &alpha; for specified EOS. E.g. SRK will use Soave &alpha;, Peng-Robinson will use PR &alpha; etc.


## &alpha; mixing rules

* van der Waals
* Huron-Vidal/NRTL
* [UNIFAC.pdf](/thermopack/memo/unifac/unifac.pdf)
* [WongSandler.pdf](/thermopack/memo/wongsandler/wongsandler.pdf)

| Model                    | Input key      |
| ------------------------ | -------------- |
| Van der Waal             | Classic or vdW |
| Huron-Vidal              | HV or HV2      |
| Wong-Sandler             | WS             |
| NRTL                     | NRTL           |
| UNIFAC                   | UNIFAC         |


## &beta; mixing rules

* ....

## Constructor

Methods to initialise Cubic model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init](#initself-comps-eos-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)


### `__init__(self, comps=None, eos=None, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Initialize cubic model in thermopack Unless both 'comps' and 'eos' parameters are specified, model must be initialized for specific components later by direct call to 'init'. Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation of state (SRK, PR, ...)

&nbsp;&nbsp;&nbsp;&nbsp; **mixing (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mixture model. Defaults to "vdW".

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Alpha model. Defaults to "Classic".

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, eos, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Initialize cubic model in thermopack

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation of state (SRK, PR, ...)

&nbsp;&nbsp;&nbsp;&nbsp; **mixing (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mixture model. Defaults to "vdW".

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Alpha model. Defaults to "Classic".

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Set- and get methods for interaction parameters, mixing parameters ...

### Table of contents
  * [Utility methods](#utility-methods)
    * [get_ci](#get_ciself-cidx)
    * [get_covolumes](#get_covolumesself)
    * [get_energy_constants](#get_energy_constantsself)
    * [get_hv_param](#get_hv_paramself-c1-c2)
    * [get_kij](#get_kijself-c1-c2)
    * [get_lij](#get_lijself-c1-c2)
    * [get_ws_param](#get_ws_paramself-c1-c2)
    * [set_ci](#set_ciself-cidx-cia-cib00-cic00-ci_type1)
    * [set_hv_param](#set_hv_paramself-c1-c2-alpha_ij-alpha_ji-a_ij-a_ji-b_ij-b_ji-c_ij-c_ji)
    * [set_kij](#set_kijself-c1-c2-kij)
    * [set_lij](#set_lijself-c1-c2-lij)
    * [set_ws_param](#set_ws_paramself-c1-c2-alpha_ij-alpha_ji-k_ij-k_ji-tau_ij-tau_ji)


### `get_ci(self, cidx)`
Get volume correction parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **cidx (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **ciA (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **ciB (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; **ciC (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol/K^2)

&nbsp;&nbsp;&nbsp;&nbsp; **ci_type (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_covolumes(self)`
Get component covolumes (L/mol)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component covolumes (L/mol)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_energy_constants(self)`
Get component energy constants in front of alpha. (Pa*L^2/mol^2)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **np.ndarray:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component energy constants in front of alpha. (Pa*L^2/mol^2)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_hv_param(self, c1, c2)`
Get Huron-Vidal parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **a_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **a_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **b_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  b param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **b_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  b param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **c_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  c param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **c_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  c param j-i

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_kij(self, c1, c2)`
Get attractive energy interaction parameter kij, where aij = sqrt(ai*aj)*(1-kij)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **kij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  i-j interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_lij(self, c1, c2)`
Get co-volume interaction parameter lij, where bij = 0.5*(bi+bj)*(1-lij)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **lij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  i-j interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_ws_param(self, c1, c2)`
Get Wong-Sandler parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **k_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  k param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **k_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  k param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **tau_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  tau param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **tau_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  tau param j-i

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ci(self, cidx, ciA, ciB=0.0, ciC=0.0, ci_type=1)`
Set volume correction parametrs

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **cidx (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **ciA (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **ciB (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol/K)

&nbsp;&nbsp;&nbsp;&nbsp; **ciC (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift param of component cidx (m3/mol/K^2)

&nbsp;&nbsp;&nbsp;&nbsp; **ci_type (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_hv_param(self, c1, c2, alpha_ij, alpha_ji, a_ij, a_ji, b_ij, b_ji, c_ij, c_ji)`
Set Huron-Vidal parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **a_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **a_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **b_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  b param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **b_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  b param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **c_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  c param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **c_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  c param j-i

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_kij(self, c1, c2, kij)`
Set attractive energy interaction parameter kij, where aij = sqrt(ai*aj)*(1-kij)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **kij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  i-j interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_lij(self, c1, c2, lij)`
Set co-volume interaction parameter lij, where bij = 0.5*(bi+bj)*(1-lij)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **lij ([type]):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  [description]

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ws_param(self, c1, c2, alpha_ij, alpha_ji, k_ij, k_ji, tau_ij, tau_ji)`
Set Wong-Sandler parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **alpha_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **k_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  k param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **k_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  k param j-i

&nbsp;&nbsp;&nbsp;&nbsp; **tau_ij (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  tau param i-j

&nbsp;&nbsp;&nbsp;&nbsp; **tau_ji (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  tau param j-i

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

