---
layout: default
version: 2.1.0
title: Methods in the cpa class
permalink: /v2.1.0/cpa_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.070162
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
cpa class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `cpa` class, found in `addon/pycThermopack/thermopack/cpa.py`, inherrits from the cubic class, and  is the interface to the 
Cubic Plus Association Equation of State. This class implements utility methods to access mixing parameters etc.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)
    * [init](#initself-comps-eossrk-mixingvdw-alphaclassic-parameter_referencedefault)
  * [Utility methods](#utility-methods)
    * [get_kij](#get_kijself-c1-c2)
    * [set_kij](#set_kijself-c1-c2-kij)
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
    * [set_kij](#set_kijself-c1-c2-kij)
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

### `set_kij(self, c1, c2, kij)`
Set attractive energy interaction parameter

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c1 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component one

&nbsp;&nbsp;&nbsp;&nbsp; **c2 (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component two

&nbsp;&nbsp;&nbsp;&nbsp; **kij (array_like):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  i-j interaction parameter (2 parameters)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `use_simplified_cpa(self, simplified)`
Use simplified form for rdf in CPA

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **simplified (bool):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  True if simplified

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

