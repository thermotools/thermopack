---
layout: default
version: 
title: Methods in the cpa class
permalink: /vcurrent/cpa_methods.html
---

<!--- 
Generated at: 2024-03-08T09:40:29.544213
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
cpa class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `cpa` class, found in `addon/pycThermopack/thermopack/cpa.py`, inherits from the cubic class, and  is the interface to the 
Cubic Plus Association Equation of State. This class implements utility methods to access mixing parameters etc.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)
    * [init](#initself-comps-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)
  * [Utility methods](#utility-methods)
    * [get_kij](#get_kijself-c1-c2)
    * [get_pure_params](#get_pure_paramsself-ic)
    * [print_cpa_report](#print_cpa_reportself)
    * [set_cpa_formulation](#set_cpa_formulationself-simplified-elliot)
    * [set_kij](#set_kijself-c1-c2-kij_a-kij_eps)
    * [set_pure_params](#set_pure_paramsself-ic-params)
    * [use_simplified_cpa](#use_simplified_cpaself-simplified)

## Constructor

Methods to initialise Cubic Plus Association model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)
    * [init](#initself-comps-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)


### `__init__(self, comps=None, eos='SRK', mixing='vdW', alpha='Classic', parameter_reference='Default')`
Initialize cubic plus association model in thermopack

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; If no components are specified, model must be initialized for specific components later by direct call to 'init'

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Cubic equation of state. Defaults to "SRK".

&nbsp;&nbsp;&nbsp;&nbsp; **mixing (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mixture model. Defaults to "vdW".

&nbsp;&nbsp;&nbsp;&nbsp; **alpha (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Alpha model. Defaults to "Classic".

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, eos='SRK', mixing='vdW', alpha='Classic', parameter_reference='Default')`
Initialize cubic plus association model in thermopack

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Cubic equation of state. Defaults to "SRK".

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
    * [get_kij](#get_kijself-c1-c2)
    * [get_pure_params](#get_pure_paramsself-ic)
    * [print_cpa_report](#print_cpa_reportself)
    * [set_cpa_formulation](#set_cpa_formulationself-simplified-elliot)
    * [set_kij](#set_kijself-c1-c2-kij_a-kij_eps)
    * [set_pure_params](#set_pure_paramsself-ic-params)
    * [use_simplified_cpa](#use_simplified_cpaself-simplified)


### `get_kij(self, c1, c2)`
Get attractive energy interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **kij (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  i-j interaction parameter (2 parameters)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `get_pure_params(self, ic)`
Get pure parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **params (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a0 (Pa*L^2/mol^2), b (L/mol), eps (J/mol), beta (-), c1 (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `print_cpa_report(self)`
Print cpa parameters  Parameters printed are the five pure parameters a0, b, epsilon, beta, c1, and the binary parameter for the cubic part, kij_a, and for the association part, kij_eps. 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_cpa_formulation(self, simplified, elliot)`
Set CPA formulation Args: simplified (bool): Use simplified form for rdf in CPA? elliot (bool): use Elliot mixing rule for association Deltas? 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_kij(self, c1, c2, kij_a, kij_eps)`
Set attractive energy interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **kij_a (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  cubic i-j interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; **kij_eps (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  association i-j interaction parameter

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pure_params(self, ic, params)`
Set pure parameters Input a0, b in their conventional (non-SI) units, beta and eps in SI units, c1 dimensionless.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **params (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  a0 (Pa*L^2/mol^2), b (L/mol), eps (J/mol), beta (-), c1 (-)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `use_simplified_cpa(self, simplified)`
Use simplified form for rdf in CPA

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **simplified (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  True if simplified

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

