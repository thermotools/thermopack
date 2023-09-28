---
layout: default
version: 
title: Documentation
permalink: /vcurrent/method_docs.html
---

Equations of state in ThermoPack are classes. All classes inherit from the `thermo` class, which holds
the interfaces to most practical calculations, such as computation of thermophysical properties, 
phase equilibria, etc. Inheriting classes may contain utility methods for specific parameter tuning,
methods to compute specific Helmholtz energy contributions, or other methods specific to the EoS.

The documentation for the `thermo` class is found [here](thermo_methods.html)

The available equations of state in ThermoPack are indicated in the table below, with links to the
documentation for the respective class in the cases where the class implements any specific non-inherited functionality.
Information about available species can be found on the [Component identifiers](component-name-mapping.html) page, 
all components are avialable for the cubic equations of state.

The (Python-side) inheritance structure for the classes is shown in the list below the table.

| [Cubic](cubic_methods.md) | Extended Cubic          | [SAFT](saft_methods.html)               | [Multi- parameter](multiparam_methods.html) | Other                                                |
|---------------------------|-------------------------|-----------------------------------------|---------------------------------------------|------------------------------------------------------|
| VdW                       | Quantum cubic           | [SAFT-VR Mie](saftvrmie_methods.html)   | MEOS                                        | [Extended Corresponding state](ext_csp_methods.html) |
| SRK                       | [CPA](cpa_methods.html) | [SAFT-VRQ Mie](saftvrqmie_methods.html) | NIST-MEOS                                   |                                                      |
| PR                        | SRK-CPA                 | [PC-SAFT](pcsaft_methods.html)          | MBWR-16                                     |                                                      |
| Translated- Consistent PR | PR-CPA                  | SPC-SAFT                                | MBWR-32                                     |                                                      |
| Schmidt- Wensel           |                         | PCP-SAFT                                | Gerg-2008                                   |                                                      |
| Patel-Teja                |                         | [PeTS](pets_methods.html)               |                                             |                                                      |
| Lee-Kesler                |                         |                                         |                                             |                                                      |

* [thermo](thermo_methods.html)
  * Lee-Kesler 
  * [Extended Corresponding state]()
  * [cubic]()
    * VdW
    * SRK
    * PR 
    * Translated-Consistent PR
    * Schmidt-Wensel 
    * Patel-Teja
    * Quantum cubic
    * [CPA]()
      * SRK-CPA 
      * PR-CPA
  * [SAFT](saft_methods.html) 
    * [SAFT-VR Mie](saftvrmie_methods.html) 
      * [SAFT-VRQ Mie](saftvrqmie_methods.html)
    * [PC-SAFT](pcsaft_methods.html)
      * [SPC-SAFT]() 
      * [PCP-SAFT]() 
  * [Multiparameter]()
    * MEOS 
    * NIST-MEOS
    * MBWR-16 
    * MBWR-32 
    * Gerg-2008 