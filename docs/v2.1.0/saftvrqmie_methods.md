---
layout: default
version: 2.1.0
title: Methods in the saftvrqmie class
permalink: /v2.1.0/saftvrqmie_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.066702
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
saftvrqmie class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `saftvrmie` class, found in `addon/pycThermopack/thermopack/saftvrqmie.py`, is the interface to the 
SAFT-VRQ Mie Equation of State.
*NOTE*: This class inherits the `saftvrmie` class, and thereby has
access to the `model control` and `utility` methods found there. The `saftvrmie` class inherits
the `saft` class, which in turn inherits the `thermo` class.
This class implements utility methods specific to the SAFT-VRQ Mie EoS.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-feynman_hibbs_order1-additive_hard_sphere_referencefalse-parameter_referencedefault-minimum_temperaturenone)
    * [init](#initself-comps-feynman_hibbs_order1-additive_hard_sphere_referencefalse-parameter_referencedefault-minimum_temperaturenone)
  * [Utility methods](#utility-methods)
    * [get_feynman_hibbs_order](#get_feynman_hibbs_orderself-c)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [set_mass](#set_massself-ic-mass)

## Constructor

Methods to initialise SAFT-VRQ Mie model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-feynman_hibbs_order1-additive_hard_sphere_referencefalse-parameter_referencedefault-minimum_temperaturenone)
    * [init](#initself-comps-feynman_hibbs_order1-additive_hard_sphere_referencefalse-parameter_referencedefault-minimum_temperaturenone)


### `__init__(self, comps=None, feynman_hibbs_order=1, additive_hard_sphere_reference=False, parameter_reference='Default', minimum_temperature=None)`
Initialize SAFT-VRQ Mie model in thermopack Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium (doi.org/10.1063/1.5111364 Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium (doi.org/10.1063/1.5136079) If no components are specified, model must be initialized for specific components later by direct call to 'init' Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **feynman_hibbs_order (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Order of Feynman-Hibbs quantum corrections (1 or 2 supported). Defaults to 1.

&nbsp;&nbsp;&nbsp;&nbsp; **additive_hard_sphere_reference (boolean):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use additive hard-sphere reference? Defaults to false.

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional) :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Is passed directly to thermopack::set_tmin()

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, feynman_hibbs_order=1, additive_hard_sphere_reference=False, parameter_reference='Default', minimum_temperature=None)`
Initialize SAFT-VRQ Mie model in thermopack Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium (doi.org/10.1063/1.5111364 Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium (doi.org/10.1063/1.5136079)

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **feynman_hibbs_order (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Order of Feynman-Hibbs quantum corrections (1 or 2 supported). Defaults to 1.

&nbsp;&nbsp;&nbsp;&nbsp; **additive_hard_sphere_reference (boolean):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Use additive hard-sphere reference? Defaults to false.

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Which parameters to use?. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; **minimum_temperature (float, optional) :** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Is passed directly to `thermo.set_tmin()`

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

## Utility methods

Set- and get methods for interaction parameters, mixing parameters ...

### Table of contents
  * [Utility methods](#utility-methods)
    * [get_feynman_hibbs_order](#get_feynman_hibbs_orderself-c)
    * [print_saft_parameters](#print_saft_parametersself-c)
    * [set_mass](#set_massself-ic-mass)


### `get_feynman_hibbs_order(self, c)`
Get Feynman-Hibbs order

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

#### Returns:

&nbsp;&nbsp;&nbsp;&nbsp; **int:** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Feynman-Hibbs order

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `print_saft_parameters(self, c)`
Print saft parameters for component c

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **c (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index (FORTRAN)

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `set_mass(self, ic, mass)`
Set mass

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **ic (int):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Component index

&nbsp;&nbsp;&nbsp;&nbsp; **m (float):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Mass of component ic [kg]

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

