---
version: 2.2.0
layout: home
title: ThermoPack v2.2.0
permalink: /v2.2.0/home.html
---

ThermoPack 2.2.0 was released in Febuary 2024, and is available on the [Python Packaging Index (PyPI)](https://pypi.org/project/thermopack/). 
This release contains several new features and interfaces, while remaining completely backwards compatible with [ThermoPack 2.1.0](/thermopack/v2.1.0/home.html).

Thermopack is a thermodynamics library for multi-component and
multi-phase thermodynamics developed at [SINTEF Energy
Research](https://www.sintef.no/en/sintef-energy/) and [NTNU
Department of
Chemistry](https://www.ntnu.edu/chemistry/research/thermodynamics). Through
decades of research, we have developed a software that performs
thermodynamic calculations. A large selection of equations of state
have been implemented in this software. Most of these equations of
state have been developed by other research groups around the world,
but some of them have been developed by us. Thermopack has been a
much-appreciated in-house powerhouse.

![](/thermopack/assets/graphics/readme_intro.gif?raw=true)

Thermopack is available for everybody, free of charge under the
MIT/Apache 2.0 open-source licenses. Thermopack is written in FORTRAN
to handle heavy numerical computations associated with process and
computational fluid dynamics (CFD) simulations. The thermodynamic
framework is easily interfaced from C/C++ and also contains a flexible
Python wrapper to make scripting easy.

## Updates in v2.2.0

* New return patterns for more flexible, less error prone, handling of output
  * New return struct for flash- calculations, making it easier and less error prone to access desired results.
  * New return struct for pxy phase envelopes for easier and less error prone access to results.
* Interface for computing Txy-phase envelopes
* Interface for computing the spinodal curve
* Computation of isolines in the metastable region
* More solid property calculations
* Interfaces to extract specific Helmholtz energy contributions to SAFT-type equations of state.
* More interfaces for tuning `cubic` and `cpa` type equations of state
* Convenient wrapper classes for specific equations of state derived from `cubic`, `cpa` and `pcsaft`.
* Accurate reference values for entropies and enthalpies, allowing for reaction equilibria calculations.
