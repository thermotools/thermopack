---
layout: default
version: 
title: Methods in the pets class
permalink: /vcurrent/pets_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.070933
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
pets class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `pets` class, found in `addon/pycThermopack/thermopack/pets.py`, inherrits from the saft class, and  is the interface to the 
PeTS Equation of State. This class implements utility methods to access mixing parameters etc.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-parameter_referencedefault-minimum_temperature20)
    * [init](#initself-parameter_referencedefault-minimum_temperature20)
  * [Utility methods](#utility-methods)
    * [get_pure_params](#get_pure_paramsself)
    * [set_pure_params](#set_pure_paramsself-sigma-eps_div_kb)

## Constructor

Methods to initialise PeTS model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-parameter_referencedefault-minimum_temperature20)
    * [init](#initself-parameter_referencedefault-minimum_temperature20)


### `__init__(self, parameter_reference='Default', minimum_temperature=2.0)`
Initialize pets specific function pointers

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  What parameters to use. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum temperature considered by numerical solvers. Default value 2.0

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, parameter_reference='Default', minimum_temperature=2.0)`
Initialize he PeTS equation of state for the LJ fluid truncated and shifted at 2.5 sigma. Reference: Heier et al. 2018 (10.1080/00268976.2018.1447153)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  What parameters to use. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Minimum temperature considered by numerical solvers. Default value 2.0

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Set- and get methods for interaction parameters, mixing parameters ...

### Table of contents
  * [Utility methods](#utility-methods)
    * [get_pure_params](#get_pure_paramsself)
    * [set_pure_params](#set_pure_paramsself-sigma-eps_div_kb)


### `get_pure_params(self)`
Get pure fluid PeTS parameters

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Segment diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_kb (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_pure_params(self, sigma, eps_div_kb)`
Set pure fluid PeTS parameters

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **sigma (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Segment diameter (m)

&nbsp;&nbsp;&nbsp;&nbsp; **eps_div_kb (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Well depth divided by Boltzmann's constant (K)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

