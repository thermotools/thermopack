---
layout: default
version: 
title: Methods in the cubic class
permalink: /vcurrent/cubic_methods.html
---

<!--- 
Generated at: 2023-10-16T10:01:24.430020
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
cubic class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `cubic` class, found in `addon/pycThermopack/thermopack/cubic.py`, is the interface to the 
Cubic Equation of State. This class implements utility methods to access mixing parameters etc.

In addition to the `cubic` class, there are several convenience classes to give easy access to specific cubic equations of state. The sections [Initialiser keys](#initialiser-keys), [Pure fluid &alpha;](#pure-fluid-&alpha;), [&alpha; mixing rules](#&alpha;-mixing-rules) and [&beta; mixing rules](#&beta;-mixing-rules) summarise the various valid input keys that can be used to modify mixing rules, the &alpha; -parameter and the underlying EoS.

Documentation for the methods in the cubic class is found in the remaining sections, summarised in the table of contents below.

## Input keys
* [Initialiser keys](#initialiser-keys)
* [Pure fluid &alpha;](#pure-fluid-&alpha;)
* [&alpha; mixing rules](#&alpha;-mixing-rules)
* [&beta; mixing rules](#&beta;-mixing-rules)

# Specific cubics

## Table of contents
  * [SoaveRedlichKwong](#soaveredlichkwong)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-parameter_referencedefault-volume_shiftfalse)
  * [RedlichKwong](#redlichkwong)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-alphark-parameter_referencedefault-volume_shiftfalse)
  * [VanDerWaals](#vanderwaals)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
  * [PengRobinson](#pengrobinson)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
  * [PengRobinson78](#pengrobinson78)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-parameter_referencedefault-volume_shiftfalse)
  * [PatelTeja](#patelteja)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
  * [SchmidtWensel](#schmidtwensel)
    * [\_\_init\_\_](#__init__self-comps-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)

# Parent class "cubic"

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init](#initself-comps-eos-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init_pseudo](#init_pseudoself-comps-tclist-pclist-acflist-mwlistnone-mixingvdw-alphaclassic)
  * [Utility methods](#utility-methods)
    * [get_ci](#get_ciself-cidx)
    * [get_covolumes](#get_covolumesself)
    * [get_energy_constants](#get_energy_constantsself)
    * [get_hv_param](#get_hv_paramself-c1-c2)
    * [get_kij](#get_kijself-c1-c2)
    * [get_lij](#get_lijself-c1-c2)
    * [get_ws_param](#get_ws_paramself-c1-c2)
    * [set_alpha_corr](#set_alpha_corrself-ic-corrname-coeffs)
    * [set_beta_corr](#set_beta_corrself-ic-corrname-coeffs)
    * [set_ci](#set_ciself-cidx-cia-cib00-cic00-cid00-cie00-cif00-ci_type1)
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

## SoaveRedlichKwong

Interface to the `SoaveRedlichKwong` EoS

### `__init__(self, comps, mixing='vdW', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='SRK'`. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## RedlichKwong

Interface to the `RedlichKwong` EoS

### `__init__(self, comps, mixing='vdW', alpha='RK', parameter_reference='Default', volume_shift=False)`
Convenience class for Redlich-Kwong, calls the `cubic` constructor. Set `alpha=Soave` in order to get SRK model. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## VanDerWaals

Interface to the `VanDerWaals` EoS

### `__init__(self, comps, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='VdW'`. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## PengRobinson

Interface to the `PengRobinson` EoS

### `__init__(self, comps, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='PR'`. Default `alpha` is the original 1976 correlation. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## PengRobinson78

Interface to the `PengRobinson78` EoS

### `__init__(self, comps, mixing='vdW', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='PR'`. Using the 1978 `alpha` correlation. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## PatelTeja

Interface to the `PatelTeja` EoS

### `__init__(self, comps, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='PT'`. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## SchmidtWensel

Interface to the `SchmidtWensel` EoS

### `__init__(self, comps, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Basic convenience class, calls the `cubic` constructor with `eos='SW'`. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Constructor

Methods to initialise Cubic model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init](#initself-comps-eos-mixingvdw-alphaclassic-parameter_referencedefault-volume_shiftfalse)
    * [init_pseudo](#init_pseudoself-comps-tclist-pclist-acflist-mwlistnone-mixingvdw-alphaclassic)


### `__init__(self, comps=None, eos=None, mixing='vdW', alpha='Classic', parameter_reference='Default', volume_shift=False)`
Initialize cubic model in thermopack

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Unless both 'comps' and 'eos' parameters are specified, model must be initialized for specific components

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; later by direct call to 'init'.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

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

### `init_pseudo(self, comps, Tclist, Pclist, acflist, Mwlist=None, mixing='vdW', alpha='Classic')`
Initialize pseudocomponents of cubic model in thermopack. The cubic init routine must have been called first.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of names for all components

&nbsp;&nbsp;&nbsp;&nbsp; **Tclist (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Critical temperatures (K)

&nbsp;&nbsp;&nbsp;&nbsp; **Pclist (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Critical pressures (Pa)

&nbsp;&nbsp;&nbsp;&nbsp; **acflist (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  acentric factors (-)

&nbsp;&nbsp;&nbsp;&nbsp; **Mwlist (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Molar masses (kg/mol)

&nbsp;&nbsp;&nbsp;&nbsp; **mixing (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mixing rule

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  alpha correlation

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
    * [set_alpha_corr](#set_alpha_corrself-ic-corrname-coeffs)
    * [set_beta_corr](#set_beta_corrself-ic-corrname-coeffs)
    * [set_ci](#set_ciself-cidx-cia-cib00-cic00-cid00-cie00-cif00-ci_type1)
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

### `set_alpha_corr(self, ic, corrname, coeffs)`
Set alpha correlation

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (in):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component number

&nbsp;&nbsp;&nbsp;&nbsp; **corrname (string):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Name of correlation

&nbsp;&nbsp;&nbsp;&nbsp; **coeffs (ndarray):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Coefficients in correlation

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_beta_corr(self, ic, corrname, coeffs)`
Set beta correlation

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (in):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component number

&nbsp;&nbsp;&nbsp;&nbsp; **corrname (string):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Name of correlation

&nbsp;&nbsp;&nbsp;&nbsp; **coeffs (ndarray):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Coefficients in correlation

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_ci(self, cidx, ciA, ciB=0.0, ciC=0.0, ciD=0.0, ciE=0.0, ciF=0.0, ci_type=1)`
Set volume correction parameters

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

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Volume shift type (CONSTANT=1, LINEAR=2, QUADRATIC=3, QUINTIC=6)

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

