---
layout: default
version: 2.1.0
title: Methods in the ext_csp class
permalink: /v2.1.0/ext_csp_methods.html
---

<!--- 
Generated at: 2023-09-28T21:06:29.071574
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/markdown_from_docstrings.py
The file is created by parsing the docstrings of the methods in the 
ext_csp class. For instructions on how to use the parser routines, see the
file thermopack/addon/pyUtils/docs/markdown_from_docstrings.py--->

The `ext_csp` class, found in `addon/pycThermopack/thermopack/ext_csp.py`, inherrits from the thermo class, and  is the interface to the 
Extended Corresponding states Equation of State. This class implements utility methods to access mixing parameters etc.

## Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-sh_eosnone-sh_alphanone-sh_mixingnone-ref_eosnone-ref_compnone-ref_alphaclassic-parameter_referencedefault)
    * [init](#initself-comps-sh_eos-sh_alpha-sh_mixing-ref_eos-ref_comp-ref_alphaclassic-parameter_referencedefault)

## Constructor

Methods to initialise Extended Corresponding states model.

### Table of contents
  * [Constructor](#constructor)
    * [\_\_init\_\_](#__init__self-compsnone-sh_eosnone-sh_alphanone-sh_mixingnone-ref_eosnone-ref_compnone-ref_alphaclassic-parameter_referencedefault)
    * [init](#initself-comps-sh_eos-sh_alpha-sh_mixing-ref_eos-ref_comp-ref_alphaclassic-parameter_referencedefault)


### `__init__(self, comps=None, sh_eos=None, sh_alpha=None, sh_mixing=None, ref_eos=None, ref_comp=None, ref_alpha='Classic', parameter_reference='Default')`
Initialize extended corredsponding state model model.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Unless all of the optional arguments that default to None are specified, model must be initialized

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; for specific components later by direct call to 'init'.

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; Model can at any time be re-initialized for new components or parameters by direct calls to 'init'

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **sh_eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor equation of state

&nbsp;&nbsp;&nbsp;&nbsp; **sh_alpha (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor alpha

&nbsp;&nbsp;&nbsp;&nbsp; **sh_mixing (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor mixing rules

&nbsp;&nbsp;&nbsp;&nbsp; **ref_eos (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference equation of state

&nbsp;&nbsp;&nbsp;&nbsp; **ref_comp (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference component

&nbsp;&nbsp;&nbsp;&nbsp; **ref_alpha (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos. Defaults to "Classic"

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Identefier for parameters set. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

### `init(self, comps, sh_eos, sh_alpha, sh_mixing, ref_eos, ref_comp, ref_alpha='Classic', parameter_reference='Default')`
Initialize extended corredsponding state model model.

#### Args:

&nbsp;&nbsp;&nbsp;&nbsp; **comps (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Comma separated list of component names

&nbsp;&nbsp;&nbsp;&nbsp; **sh_eos (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor equation of state

&nbsp;&nbsp;&nbsp;&nbsp; **sh_alpha (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor alpha

&nbsp;&nbsp;&nbsp;&nbsp; **sh_mixing (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Shape factor mixing rules

&nbsp;&nbsp;&nbsp;&nbsp; **ref_eos (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference equation of state

&nbsp;&nbsp;&nbsp;&nbsp; **ref_comp (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Reference component

&nbsp;&nbsp;&nbsp;&nbsp; **ref_alpha (str):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Needed if refEos is a cubic eos. Should not be present if one want to use an mbwr reference eos. Defaults to "Classic"

&nbsp;&nbsp;&nbsp;&nbsp; **parameter_reference (str, optional):** 

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;  Identefier for parameters set. Defaults to "Default".

&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp; 

