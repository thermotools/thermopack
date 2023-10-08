---
layout: default
version: 
title: Methods in the multiparam class
permalink: /vcurrent/multiparam_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.072302
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
multiparam class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `multiparam` class, found in `addon/pycThermopack/thermopack/multiparam.py`, inherrits from the `thermo` class, and  is the interface to the 
Multiparameter Equations of State. Selection of different multiparameter equations of state is done by passing an identifier string to the constructor. For information on available multiparameter equations of state, see the page on [available equations of state.](/thermopack/vcurrent/method_docs.md)## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-reference_statedefault)
    * [init](#initself-comps-eos-reference_statedefault)

## Constructor

Methods to initialise Multiparameter model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-eosnone-reference_statedefault)
    * [init](#initself-comps-eos-reference_statedefault)


### `__init__(self, comps=None, eos=None, reference_state='DEFAULT')`
Initialize multiparameter EOS

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Unless both parameters are specified, model must be initialized for specific components later by direct call

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; to 'init'.  Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation of state. (NIST_MEOS, MBWR32, MBWR19)

&nbsp;&nbsp;&nbsp;&nbsp; **reference_state (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference state. ("DEFAULT", "IIR", "NBP", "ASHRAE", "IDGAS", "TRIPLE_POINT")

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, eos, reference_state='DEFAULT')`
Initialize multiparameter EOS

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **eos (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Equation of state. (NIST_MEOS, MBWR32, MBWR19, MEOS, GERG2008)

&nbsp;&nbsp;&nbsp;&nbsp; **reference_state (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference state. ("DEFAULT", "IIR", "NBP", "ASHRAE", "IDGAS", "TRIPLE_POINT")

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

