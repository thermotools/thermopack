<!--- 
Generated at: 2024-12-13T12:43:28.580207
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/join_docs.py
The file is created by joining the contents of the files
    /Users/vegardjervell/code/thermopack/addon/pyUtils/docs/../../../docs/vCurrent/
        readme_parts/header.md
        readme_parts/pypi_toc.md
        metapages/please_cite.md
        readme_parts/pypi_structure.md
        v2.2.0/getting_started.md
        v2.2.0/more_advanced.md
        v2.2.0/Component-name-mapping.md
--->

# [ThermoPack homepage](https://thermotools.github.io/thermopack/)

For the full documentation and user guide to ThermoPack, see the [ThermoPack homepage.](https://thermotools.github.io/thermopack/)
If you are running ThermoPack installed via `pip`, make sure to check the documentation for the correct version by selecting 
the version number in the sidebar.

## About

Thermopack is a thermodynamics library for multi-component and
multi-phase thermodynamics developed at [SINTEF Energy
Research](https://www.sintef.no/en/sintef-energy/) and [NTNU
Department of
Chemistry](https://www.ntnu.edu/chemistry/research/thermodynamics). Through
decades of research, we have developed a software that performs
thermodynamic calculations. A large selection of equations of state
has been implemented in this software. Most of these equations of
state have been developed by other research groups around the world,
but some of them have been developed by us. Thermopack has been a
much-appreciated in-house powerhouse.

![](https://thermotools.github.io/thermopack/assets/graphics/readme_intro.gif?raw=true)

Thermopack is available for everybody, free of charge under the
MIT/Apache 2.0 open-source licenses. Thermopack is written in FORTRAN
to handle heavy numerical computations associated with process and
computational fluid dynamics (CFD) simulations. The thermodynamic
framework is easily interfaced from C/C++ and also contains a flexible
Python wrapper to make scripting easy.

# Table of contents
  * [Program structure](#program-structure)
  * [Please cite](#please-cite)
  * [Authors and contact persons](#authors-and-contact-persons)
  * [License](#license)
  * [Acknowledgments](#acknowledgments)
  * [Getting started](#getting-started---python)
    * [Initialising an equation of state](#Initialising-an-equation-of-state)
  * [Doing calculations](#doing-calculations)
    * [pVT-properties](#pVT-properties)
    * [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)
    * [Isolines](#Isolines)
    * [Critical point](#critical-point)
  * [Advanced usage](#More-advanced-usage---Python)
    * [Interaction parameters](#Interaction-parameters) 
  * [Component identifiers](#Fluid-name-to-fluid-identifyer-mapping) 

# Please Cite

Thermopack has been developed through many projects, and have produced many
articles. If you are writing academic publications, please cite one or more of
the following articles:

- For general usage:  
[Thermodynamic Modeling with Equations of State: Present Challenges with Established Methods](https://doi.org/10.1021/acs.iecr.7b00317)

- Quantum cubic:  
[Accurate quantum-corrected cubic equations of state for helium, neon, hydrogen, deuterium and their mixtures](https://doi.org/10.1016/j.fluid.2020.112790)

- SAFT-VR Mie and SAFT-VRQ Mie:  
[Equation of state and force fields for Feynman--Hibbs-corrected Mie fluids. I. Application to pure helium, neon, hydrogen, and deuterium](https://doi.org/10.1063/1.5111364)  
[Equation of state and force fields for Feynman–Hibbs-corrected Mie fluids. II. Application to mixtures of helium, neon, hydrogen, and deuterium](https://doi.org/10.1063/1.5136079)  
[Choice of reference, the influence of non-additivity and challenges in thermodynamic perturbation theory for mixtures](https://doi.org/10.1063/1.5142771)

- CPA, PC-SAFT or cubic models with Wong–Sandler, Huron–Vidal or UNIFAC mixing
rules:  
[Thermodynamic models to accurately describe the PVTxy-behavior of water/carbon dioxide mixtures](https://doi.org/10.1016/j.fluid.2017.02.006)

- Using dry-ice and water-ice model or the tc-PR/tc-RK:  
[Depressurization of CO<sub>2</sub>-N<sub>2</sub> and CO<sub>2</sub>-He in a pipe: Experiments and modelling of pressure and temperature dynamics](https://doi.org/10.1016/j.ijggc.2021.103361)

- Energy-density and entropy-density flashes:  
[The influence of CO2 mixture composition and equations of state on simulations of transient pipeline decompression](https://doi.org/10.1016/j.ijggc.2016.07.004)

- Mapping spinodals or critical points:  
[The spinodal of single-and multi-component fluids and its role in the development of modern equations of state](https://doi.org/10.1016/j.fluid.2016.12.018)  
[Predicting triggering and consequence of delayed LNG RPT](https://doi.org/10.1016/j.jlp.2018.06.001)

- Perturbation theories for Lennard-Jones spline fluid:  
[Perturbation theories for fluids with short-ranged attractive forces: A case study of the Lennard-Jones spline fluid](https://doi.org/10.1063/5.0082690)  
[Thermodynamic properties of the 3D Lennard-Jones/spline model](https://doi.org/10.1080/00268976.2019.1664780)

## License
Thermopack is distributed under the [MIT
license](https://github.com/thermotools/thermopack/blob/main/LICENSE-MIT)
and [Apache
2.0](https://github.com/thermotools/thermopack/blob/main/LICENSE-APACHE).

## Acknowledgments
A number of colleagues at SINTEF Energy Research and NTNU have contributed to the
development of thermopack. We gratefully acknowledge their contributions.

# Program structure
The core of ThermoPack is the Fortran module. For more information on that, see the [GitHub page](https://github.com/thermotools/thermopack).
This page contains an introduction to the ThermoPack python-wrapper, which is an object-oriented implementation of a variety of equations of state (EoS), 
with integrated methods for computing thermodynamic properties.

Each EoS in thermopack is a class, which inherits from the `thermopack` class found in `thermo.py`.
This class contains all generic methods used to compute thermodynamic properties, phase equilibria, etc. The inheriting 
classes simply ensure that the correct part of the Fortran-module is linked when performing calculations, and provide 
some extended functionality for handling EoS parameters and such. See the [ThermoPack homepage](https://thermotools.github.io/thermopack/v2.2.0/home.html) 
for more information. 

Fluid parameters are compiled into the Fortran-module, and are not directly accessible through the Python-wrapper. 
The entire fluid parameter database used by thermopack may be found in the [`/fluids` directory](https://github.com/thermotools/thermopack/tree/v2.2.0/fluids) 
in the GitHub repo. In order to model fluids not currently supported in the module available through `pip`, ThermoPack
must be compiled from source with the new parameters. See the [ThermoPack homepage](https://thermotools.github.io/thermopack/v2.2.0/home.html)
for information on how to add new fluids, and a guide on how to compile from source. Please feel free to leave a PR for 
new parameter sets such that these can be included in future releases of thermopack.

# Getting Started

# Compatibility
ThermoPack v2.2.0 is completely backwards compatible with v2.1.0, such that all examples in the [guide for v2.1.0](https://thermotools.github.io/thermopack/v2.1.0/getting_started.html)
will also work with v2.2.0. This guide primarily aims to demonstrate functionality that is new in v2.2.0.

# Getting started - Python
This is a short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/v2.2/addon/pyExamples) in the GitHub repo. Comprehensive documentation for the methods available through the python interface can also be found in the [doc page for the thermo class.](https://thermotools.github.io/thermopack/v2.2.0/thermo_methods.html). For more advanced users, a look at the [more advanced page](https://thermotools.github.io/thermopack/v2.2.0/more_advanced.html) may also be useful.

Equations of State (EoS's) in ThermoPack are classes. To do calculations for a given mixture an EoS object must first be 
initialized for that mixture, as demonstrated in the [Initializing an EoS section](#initialising-an-equation-of-state). 
Then, a wide variety of thermodynamic computations can be done, as demonstrated in the remaining sections.

## Contents
* [Initialising an equation of state](#initialising-an-equation-of-state)
* [pVT properties](#pvt-properties)
  * [Differentials](#differentials)
* [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)
  * [Flash calculations](#flash-calculations)
  * [Phase envelopes](#phase-envelopes)
    * [Tp- and Tv- envelopes](#tp--and-tv--phase-envelopes)
    * [pxy- and Txy- envelopes](#pxy--and-txy--phase-envelopes)
  * [Dew- and bubble points](#dew--and-bubble-points)
* [Isolines](#isolines)
* [Critical point](#critical-point)

## Initialising an equation of state
An overview of available equations of state can be found [here](https://thermotools.github.io/thermopack/v2.2.0/method_docs.html).

An EoS is initialized by passing in the [fluid identifiers](https://thermotools.github.io/thermopack/v2.2.0/Component-name-mapping.html) of the mixture, for example

```Python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('C1,CO2')
```
will initialize a SAFT-VR Mie EoS for a mixture of methane and CO2. The complete list of component identifiers is in the [Fluid identifiers list](https://thermotools.github.io/thermopack/v2.2.0/Component-name-mapping.html). PC-SAFT, SAFT-VRQ Mie and Lee-Kesler EoS are initialized in the same way, as
```Python
from thermopack import saftvrmie, saftvrqmie, pcsaft, lee_kesler
svrm = saftvrmie.saftvrmie('AR,KR') # SAFT-VR Mie EoS for Ar/Kr mixture
svrqm = saftvrqmie.saftvrqmie('HE') # SAFT-VRQ Mie EoS for pure He
pcs = pcsaft.pcsaft('BENZENE,NC6,NC12') # PC-SAFT EoS for ternary benzene/hexane/dodecane mixture
lk = lee_kesler.lee_kesler('N2,O2') # Lee-Kesler EoS for nitrogen/oxygen mixture 
```
For PC-SAFT, both the simplified PC-SAFT (`SPC-SAFT`) and Polar PC-SAFT (`PCP-SAFT`) are available

```Python
from thermopack.pcsaft import SPC_SAFT, PCP_SAFT
spcs = SPC_SAFT('NC6,NC12') # Simplified PC-SAFT
pcps = PCP_SAFT('H2O,MEOH') # Polar PC-SAFT
```

The cubic equations of state are found in the `cubic` module. Available cubic EoS's and more information on the individual cubics, mixing rules, etc. can be found on the [cubic page](https://thermotools.github.io/thermopack/v2.2.0/cubic_methods.html).
```Python
from thermopack.cubic import SoaveRedlichKwong, RedlichKwong, PengRobinson, PengRobinson78
from thermopack.cubic import SchmidtWensel, PatelTeja, VanDerWaals
srk = SoaveRedlichKwong('NH3,C2') # SRK EoS for ammonia/ethane mixture
rk = RedlichKwong('NC6,CO2,NC12') # Redlich-Kwong EoS
pr = PengRobinson('IC4,NC10') # PR EoS for isobutane/decane mixture
pr78 = PengRobinson78('N2,O2,CO2') # # PR-78 EoS for isobutane/decane mixture
vdw = VanDerWaals('C1,C2,C3,N2,O2') # VdW EoS for methane/ethane/propane/nitrogen/oxygen mixture
sw = SchmidtWensel('R11,R12') # Schmidt-Wensel EoS for FCl3C/F2Cl2C mixture
pt = PatelTeja('PRLN') # Patel-Teja EoS for pure propylene
```
In addition to these, the Translated-Consisten Peng-Robinson is available as
```Python
from thermopack.tcPR import tcPR
tcpr = tcPR('F6S,SO2') # Translated-Consistent PR EoS for SF6/SO2 mixture
```
For more fine-tuned control of the cubic EoS, the parent class [`cubic`](https://thermotools.github.io/thermopack/v2.2.0/cubic_methods.html) can be initialised directly, to explicitly 
set mixing rules, alpha-correlation etc.

Cubic-plus association EoS's are available for the SRK and PR EoS through the `cpa` module as
```Python
from thermopack.cpa import SRK_CPA, PR_CPA
srk_cpa = SRK_CPA('H2O,ETOH,PROP1OL') # SRK-CPA EoS for water/ethanol/propanol mixture
```

Several multiparameter EoS's can interfaced through the `multiparameter.multiparam` class. The available multiparameter EoS's are NIST-MEOS, MBWR16 and MBWR32. These are initialized as
```Python
from thermopack.multiparameter import multiparam
nist = multiparam('C3', 'NIST_MEOS') # NIST-MEOS EoS for propane
mbwr19 = multiparam('C1', 'MBWR19') # MBWR19 EoS for methane
mbwr32 = multiparam('C2', 'MBWR32') # MBWR32 EoS for ethane
```
please note that not all fluids are supported for multiparameter equations of state, depending on what parameters are available in the fluid database.

Finally, the Extended-corresponding state EoS is available through the `extended_csp.ext_csp` class as
```Python
from thermopack.extended_csp import ext_csp
eos = ext_csp('C1,C2,C3,NC4', sh_eos='SRK', sh_alpha='Classic',
              sh_mixing='vdW', ref_eos='NIST_MEOS', ref_comp='C3')
```

For more information on the extended-csp EoS please see the [Examples](https://github.com/thermotools/thermopack/tree/v2.2/addon/pyExamples) 
and the [memo](https://thermotools.github.io/thermopack/memo/index.html).

# Doing calculations
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in 
thermopack are the [specific documentation of the `thermo` class](https://thermotools.github.io/thermopack/v2.2.0/thermo_methods.html). 
Here, a small subset of the functionality is demonstrated.

Note that all input/output is in SI units (moles/kelvin/pascal/cubic meters/joule)

## pVT-properties

Specific volume, given temperature, pressure and composition is computed as 
```python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('NC6,NC10') # Hexane/decane mixture
T = 300 # Kelvin
p = 1e5 # Pascal
x = [0.2, 0.8] # Molar composition
vg, = eos.specific_volume(T, p, x, eos.VAPPH) # Molar volume of gas phase (NB: Notice the comma)
vl, = eos.specific_volume(T, p, x, eos.LIQPH) # Molar volume of liquid phase (NB: Notice the comma)
```
where `eos.VAPPH` and `eos.LIQPH` are [phase flags](https://thermotools.github.io/thermopack/v2.1.0/phase_flags.html) used to identify different phases. The commas are necessary because all output from thermopack methods are as tuples. 

Similarly, pressure, internal energy, enthalpy, entropy, etc. and associated differentials can be computed via the methods 
`chemical_potential_tv(T, V, n)`, `internal_energy_tv(T, V, n)`, `enthalpy_tv(T, V, n)`, `helmholtz_tv(T, V, n)`, 
`entropy_tv(T, V, n)`. For a full overview of the available property calculations see the 
[TV-property interfaces](/thermopack/v2.2.0/thermo_methods.html#tv-property-interfaces) and the 
[Tp-property interfaces](/thermopack/v2.2.0/thermo_methods.html#tp-property-interfaces) of the 
[`thermo` class](https://thermotools.github.io/thermopack/v2.2.0/thermo_methods.html).

### Differentials


If we want volume differentials, we use the same method, but set the flags to calculate differentials to `True`:

```python
# Continued 
vg, dvdp = eos.specific_volume(T, p, x, eos.VAPPH, dvdp=True) # Vapour phase molar volume and pressure differential
vl, dvdT = eos.specific_volume(T, p, x, eos.LIQPH, dvdt=True) # Liquid phase molar volume and temperature differential
_, dvdn = eos.specific_volume(T, p, x, eos.LIQPH, dvdn=True) # Liquid phase partial molar volumes
```

Differentials can be computed as functions of $(T, V, n)$ or as functions of $(T, p, n)$. For an overview of the different methods, 
see [Advanced usage: Different property interfaces](https://thermotools.github.io/thermopack/v2.2.0/more_advanced.html). A short example is given here as:

```Python
# Continued
n_tot = 15 # Total number of moles
n = n_tot * x
H, dHdn_TV = eos.enthalpy_tv(T, vg, n, dhdn=True) # Compute enthalpy and derivative of enthalpy wrt. mole numbers at constant (T, V)
h_vap, dhvap_dn_Tp = eos.enthalpy(T, p, x, eos.VAPPH, dhdn=True) # Compute molar vapour phase enthalpy and derivative of molar vapour phase enthalpy wrt. mole numbers at constant (T, p)
h_liq, dliq_dn_Tp = eos.enthalpy(T, p, x, eos.LIQPH, dhdn=True) # Compute molar liquid phase enthalpy and derivative of molar liquid phase enthalpy wrt. mole numbers at constant (T, p)
H, dHdn_Tp = eos.enthalpy_tvp(T, vg, n, dhdn=True) # Compute enthalpy and derivative of enthalpy wrt. mole numbers at constant (T, p)
```

**Please note that heat capacities are not available directly**, but must be computed as derivatives of enthalpy and internal energy, as

```Python
from thermopack.cubic import cubic
eos = cubic('C1,C3,NC6', 'SRK') # SRK EoS for a mixture of methane, propane and n-hexane

T = 300 # Kelvin
p = 1e5 # Pascal
x = [0.2, 0.1, 0.7] # Molar composition
_, Cp_vap = eos.enthalpy(T, p, x, eos.VAPPH, dhdt=True) # Vapour phase heat capacity at constant pressure, computed as (dH/dT)_{p,n}
_, Cp_liq = eos.enthalpy(T, p, x, eos.LIQPH, dhdt=True) # Liquid phase heat capacity at constant pressure, computed as (dH/dT)_{p,n}

vg, = eos.specific_volume(T, p, x, eos.VAPPH) # Computing vapour phase specific volume
vl, = eos.specific_volume(T, p, x, eos.LIQPH) # Liquid phase specific volume

_, Cv_vap = eos.internal_energy_tv(T, vg, x, dedt=True) # Vapour phase heat capacity at constant volume, computed as (dU/dT)_{V,n}
_, Cv_liq = eos.internal_energy_tv(T, vl, x, dedt=True) # Liquid phase heat capacity at constant volume, computed as (dU/dT)_{V,n}
```

## Phase diagrams and Equilibria

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to 
use them is the [documentation of the `thermo` class.](https://thermotools.github.io/thermopack/v2.2.0/thermo_methods.html). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

### Flash calculations
Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`.

See the [Flash interfaces](/thermopack/v2.2.0/thermo_methods.html#flash-interfaces) in the 
[documentation of the `thermo` class](https://thermotools.github.io/thermopack/v2.2.0/thermo_methods.html) for the specifics on the different flash routines.

The result of a flash calculation is returned in a `FlashResult` struct. The specific results of the calculation are 
accessed through the attributes of this object.

An example calculation using `twophase_tpflash()` may be done as
```python
from thermopack.saftvrqmie import saftvrqmie
# SAFT-VRQ Mie for Hydrogen/Helium/Neon mixture 
eos = saftvrqmie('H2,HE,NE', minimum_temperature=20) # NB: Set minimum temperature low enough when working at very low temperatures
T = 35 # Kelvin
p = 3e6 # Pascal (30 bar)
z = [0.1, 0.25, 0.65] # Molar composition
flsh = eos.two_phase_tpflash(T, p, z) # flsh is a FlashResult object
print(flsh)
### Output: ###
# FlashResult object for Tp-flash
# Containing the attributes (description, name, value):
#   	Flash type                     flash_type : Tp  
#   	Total composition              z     : [0.1, 0.25, 0.65]  
#   	Temperature [K]                T     : 35  
#   	pressure [Pa]                  p     : 3000000.0  
#   	Liquid phase composition       x     : [0.05407302 0.03859287 0.90733411]  
#   	Vapour phase composition       y     : [0.14642524 0.46370066 0.3898741 ]  
#   	Vapour fraction                betaV : 0.497302408174766  
#   	Liquid fraction                betaL : 0.5026975918252341  
#   	Phase indentifier index        phase : 0  
```
the result of the flash is accessed from the attributes of the `FlashResult` object, found in 
[`utils.py`](https://github.com/thermotools/thermopack/tree/v2.2/addon/pycThermopack/thermopack/utils.py), as
```Python
# Continued
x = flsh.x # Liquid composition
y = flsh.y # Vapour composition
betaL = flsh.betaL # 
# ... etc
```

The `FlashResult` object returned by the different flash routines all contain the same attributes. 

### Phase envelopes

ThermoPack has interfaces to trace (T,p)-, (T,v)-, (p,x,y)- and (T, x, y)- phase envelopes. For the full documentation, 
see the [docs of the `thermo` class](/thermopack/v2.2.0/thermo_methods.html#saturation-interfaces). 
For more comprehensive examples, see the [Examples](https://github.com/thermotools/thermopack/tree/v2.2/addon/pyExamples).

#### Tp- and Tv- phase envelopes

Phase envelopes can be generated directly with the method `get_envelope_twophase()` as

```python
# Continued
T, p = eos.get_envelope_twophase(1e5, x) # arrays of temperature and pressure for phase envelope, starting at 1 bar.
plt.plot(p, T) # Tp-projection of phase envelope

T, p, v = eos.get_envelope_twophase(1e5, x, calc_v=True) # Also return the specific volume at each point along the phase envelope
plt.plot(1 / v, T) # rho-T projection of the phase envelope
```

#### pxy- and txy- phase envelopes

To compute pxy-type phase envelopes, we use the `get_binary_pxy()` method. This method returns a `XYDiagram` struct, which
holds the composition and pressure / temperature of each equilibrium phase.

The `XYDiagram` struct has the attributes `lle`, `l1ve` and `l2ve`, where
* `lle` holds the composition and pressure / temperature of the Liquid 1 - Liquid 2 equilibria
  * `lle.x1` is the composition of Liquid 1 (mole fraction of species 1)
  * `lle.x2` is the composition of Liquid 2 (mole fraction of species 1)
  * `lle.p` / `lle.T` is the pressure / temperature along the phase boundary
* `l1ve` holds the composition and pressure / temperature of the Liquid 1 - Vapour equilibria
  * `l1ve.x` is the composition of Liquid 1 (mole fraction of species 1)
  * `l1ve.y` is the composition of the vapour (mole fraction of species 1)
  * `l1ve.p` / `l1ve.T` is the pressure / temperature along the phase boundary
* `l2ve` holds the composition and pressure / temperature of the Liquid 2 - Vapour equilibria
  * `l2ve.x` is the composition of Liquid 2 (mole fraction of species 1)
  * `l2ve.y` is the composition of the vapour (mole fraction of species 1)
  * `l2ve.p` / `l2ve.T` is the pressure / temperature along the phase boundary

We get the phase diagram as

```python
from thermopack.cpa import SRK_CPA

eos = SRK_CPA('NC6,H2O')  # CPA-SRK eos for Hexane/water mixture
T = 350
pxy = eos.get_binary_pxy(T)  # Returns a XYDiagram struct
print(pxy)
# Result:
# XYDiagram object with attributes (name : description)
# lle  : Liquid 1 - Liquid 2 Equilibrium
# 	PxyEquilibrium object with attributes (description, name, value)
# 		Type of equilibrium                   type : lle
# 		Liquid 1 mole fraction, species 1     x1   : [4.278e-07 ... 4.145e-07], len(x1) = 457
# 		Liquid 2 mole fraction, species 1     x2   : [9.969e-01 ... 9.974e-01], len(x2) = 457
# 		Pressure                              p    : [1.648e+05 ... 1.500e+07], len(p) = 457
# 	
# l1ve : Liquid 1 - Vapour Equilibrium
# 	PxyEquilibrium object with attributes (description, name, value)
# 		Type of equilibrium                   type : lve
# 		Liquid mole fraction, species 1       x    : [4.278e-07 ... 2.197e-07], len(x) = 66
# 		Vapour mole fraction, species 1       y    : [7.880e-01 ... 6.461e-01], len(y) = 66
# 		Pressure                              p    : [1.648e+05 ... 1.000e+05], len(p) = 66
# 	
# l2ve : Liquid 2 - Vapour Equilibrium 
# 	PxyEquilibrium object with attributes (description, name, value)
# 		Type of equilibrium                   type : lve
# 		Liquid mole fraction, species 1       x    : [9.969e-01 ... 1.000e+00], len(x) = 100
# 		Vapour mole fraction, species 1       y    : [7.880e-01 ... 1.000e+00], len(y) = 100
# 		Pressure                              p    : [1.648e+05 ... 1.289e+05], len(p) = 100
```

For ease of use, the `XYDiagram` struct is iterable, such that we can unpack it efficiently as
```
lle, l1ve, l2ve = pxy
```

Now, we can plot the phase diagram as

```python
# Continued
import matplotlib.pyplot as plt

# Liquid-liquid phase boundaries
# pxy.lle holds the composition and pressure of the liquid phases
plt.plot(pxy.lle.x1, pxy.lle.p, label='Liquid 1 composition') # lle.x1 is the mole fraction of component 1 (NC12) in Liquid 1 along the phase boundary
plt.plot(pxy.lle.x2, pxy.lle.p, label='Liquid 2 composition') # lle.x2 is the mole fraction of component 1 (NC12) in Liquid 2 along the phase boundary

# Liquid 1-vapour phase boundaries
# pxy.l2ve holds composition and pressure along the Liquid 1 - Vapour phase boundary
plt.plot(pxy.l1ve.x, pxy.l1ve.p, label='Liquid 1 bubble line') # l1ve.x is the mole fraction of component 1 (NC12) in Liquid 1 along the Liquid 1 - Vapour phase boundary
plt.plot(pxy.l1ve.y, pxy.l1ve.p, label='Liquid 1 dew line') # l1ve.y is the mole fraction of component 1 (NC12) in the Vapour phase along the Liquid 1 - Vapour phase boundary

# Liquid 2-vapour phase boundaries
# L2VE[2] is the pressure along the Liquid 2 - Vapour phase boundary
plt.plot(pxy.l2ve.x, pxy.l2ve.p, label='Liquid 2 bubble line') # l2ve.x is the mole fraction of component 1 (NC12) in Liquid 2 along the Liquid 2 - Vapour phase boundary
plt.plot(pxy.l2ve.y, pxy.l2ve.p, label='Liquid 2 dew line') # l2ve.y is the mole fraction of component 1 (NC12) in the Vapour phase along the Liquid 2 - Vapour phase boundary

plt.ylabel('Pressure [Pa]') # The third element in each tuple is the pressure along the phase boundary
plt.xlabel('Molar composition')
```
The method `get_binary_txy` works in the same way, only replacing the `p` attribute with `T`, such that we can compute

```python
p = 1e5
lle, l1ve, l2ve = eos.get_binary_txy(p) # Unpacking the XYDiagram 

# Liquid-liquid phase boundaries
# l1ve holds the composition and pressure of Liquid 1 and Vapour along the phase boundary
plt.plot(l1ve.x, l1ve.T, label='Liquid 1 composition') # l1ve.x is the mole fraction of component 1 (NC12) in Liquid 1 along the phase boundary
plt.plot(l1ve.y, l1ve.T, label='Vapour composition') # l1ve.y is the mole fraction of component 1 (NC12) in Vapour along the phase boundary

# ... etc ...
```

In the same way as for [v2.1.0](/thermopack/v2.1.0/getting_started.html#pxy--phase-envelopes), the arrays of values are set to
`None` if an equilibrium is not found, such that we may need to check for the presence of an equilibrium before plotting.

### Dew- and bubble points

We can also compute the bubble-temperature, pressure etc. directly using the methods `bubble_temperature(p, z)`, 
`bubble_pressure(T, z)`, `dew_temperature(p, z)` and `dew_pressure(T, z)`, where `z` is the composition of the mixture, as

```Python
eos = cubic('CO2,C1', 'SRK')
x = [0.5, 0.5] # Total composition of the mixture
p_dew, y_dew = eos.dew_pressure(250, x) # Calculates dew pressure and dew composition at 250 K
T_dew, y_dew = eos.dew_temperature(1e5, x) # Calculates dew temperature and dew composition at 1 bar
p_bub, x_bub = eos.bubble_pressure(230, x) # Calculates bubble pressure and bubble composition at 230 K
T_bub, x_bub = eos.bubble_temperature(1e5, x) # Calculates bubble temperature and bubble composition at 1 bar
```

## Isolines

Various isolines can be computed using the methods `get_isotherm`, `get_isobar`, `get_isentrope` and `get_isenthalp`. 
In the following code snippet, the default values of the keyword arguments are indicated.

```Python
from thermopack.pcsaft import pcsaft
eos = pcsaft('NC6,NC12')
x = [0.2, 0.8]

# Calculate pressure, specific volume, specific entropy and specific enthalpy along the isotherm at 300 K
# from p = minimum_pressure to p = maximum_pressure. Compute at most nmax points.
p_iso_T, v_iso_T, s_iso_T, h_iso_T = eos.get_isotherm(300, x, minimum_pressure=1e5, maximum_pressure=1.5e7, nmax=100)

# Calculate temperature, specific volume, specific entropy and specific enthalpy along the isobar at 1 bar
# from T = minimum_temperature to T = maximum_temperature. Compute at most nmax points.
T_iso_p, v_iso_p, s_iso_p, h_iso_p = eos.get_isobar(1e5, x, minimum_temperature=200, maximum_temperature=500, nmax=100)

# Calculate temperature, pressure, specific volume and specific entropy along the isenthalp at 1 kJ / mol
# Start at the upper of (minimum_pressure, minimum_temperature)
# End at the lower of (maximum_pressure, maximum_temperature)
T_iso_h, p_iso_h, v_iso_h, s_iso_h = eos.get_isenthalp(1e3, x, minimum_pressure=1e5, maximum_pressure=1.5e7,
                                                            minimum_temperature=200, maximum_temperature=500,
                                                            nmax=100)

# Calculate temperature, pressure, specific volume and specific enthalpy along the isentrope at 5 J / mol K
# Start at the upper of (minimum_pressure, minimum_temperature)
# End at the lower of (maximum_pressure, maximum_temperature)
T_iso_s, p_iso_s, v_iso_s, h_iso_s = eos.get_isentrope(5, x, minimum_pressure=1e5, maximum_pressure=1.5e7,
                                                            minimum_temperature=200, maximum_temperature=500,
                                                            nmax=100)
```

In addition to these, we can trace isentropes and isotherms in the metastable region using the methods
[`map_meta_isentrope`](/thermopack/v2.2.0/thermo_methods.html#isolines) and [`map_meta_isotherm`](/thermopack/v2.2.0/thermo_methods.html#isolines).

## Critical point

Thermopack has a critical point solver, which is called as

```Python
eos = saftvrqmie('HE,NE') # Use FH-corrected Mie potentials for Helium calculations!
n = [5, 10]
Tc, Vc, pc = eos.critical(n) # Compute the critical temperature, pressure and volume given mole numbers
vc = Vc / sum(n) # Critical specific volume computed from critical volume and mole numbers.
```

The solver accepts initial guesses for the critical values through the `kwargs` `temp`, and `v`. The error tolerance 
can be set via the `tol` `kwarg` (default is `tol=1e-7`).

To compute only the critical pressure, temperature or molar volume, we can use the methods [`critical_pressure`]

## Metastable region

Methods for computing properties in the metastable region are found in the 
[documentation for stability interfaces](/thermopack/v2.2.0/thermo_methods.html#stability-interfaces).

We can trace the spinodal curve as

```python
import matplotlib.pyplot as plt
from thermopack.tcPR import tcPR

eos = tcPR('CO2,C1')
z = [0.2, 0.8]
Ts, vs, ps = eos.spinodal(z) # Trace the spinodal
Te, pe, ve = eos.get_envelope_twophase(1e3, z, calc_v=True) # Trace the phase envelope
plt.plot(Ts, ps, label='Spinodal')
plt.plot(Te, pe, label='Phase envelope')
plt.xlabel('T [K]')
plt.ylabel('p [Pa]')
plt.show()

plt.plot(1 / vs, Ts, label='Spinodal')
plt.plot(1 / ve, Te, label='Phase envelope')
plt.xlabel(r'$\rho$ [mol m$^{-3}$]')
plt.ylabel('T [K]')
plt.show()
```

We can also find specific points on the spinodal using the `spinodal_point` method.

For density solvers that can be used in the metastable region, refer to the 
[documentation for stability interfaces](/thermopack/v2.2.0/thermo_methods.html#stability-interfaces).

For methods to trace isolines in the metastable region, refer to the [section on isolines](#isolines). 


# More advanced usage

## Interaction parameters

In thermopack we're able to both set and get a wide array of coefficients and parameters depending on the models we are utilizing. 

### Cubic equations of state
#### Setting and getting the attractive energy interaction parameter $k_{ij}$ and co-volume interaction parameter $l_{ij}$
Starting with the attractive energy interaction parameter (kij). The parameter can be set using the function `set_kij` after initialising the equation and state. The function requires that you first write in the number of the components and subsequently the new interaction parameter i.e. (component number 1, component number 2, new kij value). If we're curious as to what parameter the EOS is already using we can see this by using the function `get_kij` which returns the value as a float given the component numbers as input i.e. (component number 1, component number 2).
```Python
cs = cubic('CO2,N2',"SRK","Classic","Classic")
#We set the interaction parameter to be -0.032
cs.set_kij(1,2,-0.032)
#We want to see what the interaction parameter is which returns that kij = -0.032
kij = cs.get_kij(1,2)
```
The procedure for setting and getting co-volume interaction parameters is analogous to the getting and setting of attractive energy parameters. Simply use the functions `set_lij` and `get_lij` instead.
```Python
#We set the parameter to be -0.032
cs.set_lij(1,2,-0.032)
#We want to see what the parameter is which returns that lij = -0.032
lij = cs.get_lij(1,2)
```

## Tuning Cubics
Cubic Equations of state implemented in ThermoPack can be accessed through the generic [`cubic` class](https://thermotools.github.io/thermopack/vcurrent/cubic_methods.html).
This class also offers a variety of methods to tune the alpha-function, mixing rules etc. See the [documentation for 
the `cubic` class](https://thermotools.github.io/thermopack/vcurrent/cubic_methods.html) for more information.

## The different property interfaces (TV-) (Tp-) and (TVp-)

Property calculations in ThermoPack can be done either through the [TV-interfaces](/thermopack/v2.2.0/thermo_methods.html#tv-property-interfaces), the [Tp-interfaces](/thermopack/v2.2.0/thermo_methods.html#tp-property-interfaces) or the [TVp-interfaces](/thermopack/v2.2.0/thermo_methods.html#tvp-property-interfaces).

The difference between the TV- and Tp- interface is only what variables the properties are computed as functions of, and what variables are held constant in the derivatives. TV-interface methods compute properties as functions of $(T, V, n)$, while Tp-interface methods compute properties as functions of $(T, p, n)$. 

The **TVp-interface** methods on the other hand take $(T, V, n)$ as arguments, and evaluate derivatives **as functions of $(T, p, n)$**. To demonstrate with an example:

```Python
import numpy as np
from thermopack.cubic import cubic

eos = cubic('O2,N2', 'PR') # PR EoS for O2/N2 mixture

T = 300 # Kelvin
p = 1e5 # Pascal
x = np.array([0.21, 0.79]) # Molar composition (of air)
n_tot = 10 # Total number of moles
n = n_tot * x # Vector of mole numbers

v, = eos.specific_volume(T, p, x, eos.VAPPH) # Compute specific volume of vapour phase
V = v * n_tot # Total volume


# Computing the TOTAL Enthalpy (J), given (T, V, n)
# The differentials are computed for H = H(T, V, n), with 
# "subscripts" indicating the variables held constant
H_tvn, dHdT_Vn, dHdn_TV = eos.enthalpy_tv(T, V, n, dhdt=True, dhdn=True) 

# Computing the SPECIFIC VAPOUR phase enthalpy (J / mol), given (T, p, n) 
# The differentials are computed for h_vap = h_vap(T, p, n), with the 
# "subscripts" indicating the variables held constant
h_vap_tpn, dh_vap_dt_pn, dh_vap_dn_Tp = eos.enthalpy(T, p, n, eos.VAPPH, dhdt=True, dhdn=True)

# Computing the SPECIFIC LIQUID phase enthalpy (J / mol), given (T, p, n) 
# The differentials are computed for h_liq = h_liq(T, p, n), with the 
# "subscripts" indicating the variables held constant
h_liq_tpn, dh_liq_dt_pn, dh_liq_dn_Tp = eos.enthalpy(T, p, n, eos.LIQPH, dhdt=True, dhdn=True)

# Computing the TOTAL Enthalpy (J), given (T, V, n)
# NOTE : The differentials are computed for H = H(T, p, n)
#        NOT for H = H(T, V, n)
H_tpn, dHdt_pn, dHdn_Tp = eos.enthalpy_tvp(T, V, n, dhdt=True, dhdn=True)
```

Besides `enthalpy_tvp`, there are currently available TVp-interfaces for `entropy_tvp` and `thermo_tvp` (logarithm of fugacity coefficients).


# Component identifiers


<!---
This is an auto-generated file, written by the module at addon/pyUtils/compdatadb.py
Generated at : 2024-02-19T15:48:32.012730
This is the same module that is used to generate the Fortran
component database files.
--->


# Fluid name to fluid identifier mapping
&nbsp;

In order to specify fluids in Thermopack you need to use fluid identifiers as shown in the table below. The 'SAFT-VR', 'PC-SAFT' and 'CPA' columns indicate which fluids SAFT-EoS and CPA parameters are available for.

&nbsp;
You may have to scroll right to view the whole table.

| Fluid name | CAS Number |Fluid identifyer | SAFT-VR | PC-SAFT | CPA |
| ------------------------ | ---- | ----------- | ---- | ---- | ---- |
| 1,1,1,2-Tetrafluoroethane | 811-97-2 | R134a |   |   |   |
| 1,1,1-Trifluoroethane | 420-46-2 | R143a |   |   |   |
| 1,1-Difluoroethane | 75-37-6 | R152a |   |   |   |
| 1,1-Difluoroethylene | 75-38-7 | R1132a |   |   |   |
| 1,2-Dichlorotetrafluoroethane | 76-14-2 | R114 |   |   |   |
| 1,3-Butadiene | 106-99-0 | 13BD |   |   |   |
| 1-Butanol | 71-36-3 | BUT1OL |   | &#10004; | &#10004; |
| 1-Chloro-1,1,2,2-tetrafluoroethane | 354-25-6 | R124a |   |   |   |
| 1-Chloro-1,1-difluoroethane | 75-68-3 | R142b |   |   |   |
| 1-Hexanol | 111-27-3 | HEX1OL |   | &#10004; | &#10004; |
| 1-Pentanol | 71-41-0 | PENT1OL |   | &#10004; | &#10004; |
| 1-Propanol | 71-23-8 | PROP1OL |   | &#10004; | &#10004; |
| 2,3,3,3-Tetrafluoropropene |  754-12-1 | R1234yf |   |   |   |
| 2-Chloro-1,1,1,2-tetrafluoroethane | 2837-89-0 | R124 |   |   |   |
| 2-Methylhexane | 591-76-4 | 2MHX |   |   |   |
| 3-Methylpentane | 96-14-0 | 3MP |   |   |   |
| Acetone | 67-64-1 | ACETONE |   | &#10004; |   |
| Acetylene | 74-86-2 | ACETYLENE |   | &#10004; |   |
| Ammonia | 7664-41-7 | NH3 | &#10004; | &#10004; | &#10004; |
| Argon | 7440-37-1 | AR | &#10004; | &#10004; |   |
| Benzene | 71-43-2 | BENZENE |   | &#10004; |   |
| Butanal | 123-72-8 | BUTANAL |   | &#10004; |   |
| Carbon dioxide | 124-38-9 | CO2 | &#10004; | &#10004; | &#10004; |
| Carbon monoxide | 630-08-0 | CO |   |   |   |
| Carbon tetrafluoride | 75-73-0 | R14 |   |   |   |
| Chlorine | 7782-50-5 | CL2 |   | &#10004; |   |
| Chlorodifluoromethane | 75-45-6 | R22 |   |   |   |
| Chloropentafluoroethane | 76-15-3 | R115 |   |   |   |
| Chlorotrifluoromethane | 75-72-9 | R13 |   |   |   |
| Chlorotrifluorosilane | 14049-36-6 | ClF3Si |   |   |   |
| Cyclohexane | 110-82-7 | CYCLOHEX |   | &#10004; |   |
| Cyclopropane | 75-19-4 | C3_1 |   |   |   |
| Deuterium | 7782-39-0 | D2 | &#10004; |   |   |
| Di-methyl ether | 115-10-6 | DME |   | &#10004; |   |
| Di-n-hexyl ether | 112-58-3 | S434 |   |   |   |
| Dichlorodifluoromethane | 75-71-8 | R12 |   |   |   |
| Dichlorofluoromethane | 75-43-4 | R21 |   |   |   |
| Difluoromethane | 75-10-5 | R32 |   |   |   |
| Dinitrogen tetroxide | 10544-72-6 | N2O4 |   |   |   |
| Equilibrium-hydrogen | 1333-74-0 | E-H2 |   |   |   |
| Ethane | 74-84-0 | C2 | &#10004; | &#10004; |   |
| Ethanol | 64-17-5 | ETOH | &#10004; | &#10004; | &#10004; |
| Ethylbenzene | 100-41-4 | EBZN |   |   |   |
| Ethylene glycol | 107-21-1 | MEG |   |   |   |
| Ethylene | 74-85-1 | C2_1 |   | &#10004; |   |
| Helium-4 | 7440-59-7 | HE | &#10004; |   |   |
| Hexafluoroethane | 76-16-4 | R116 |   |   |   |
| Hydrazine | 302-01-2 | N2H4 | &#10004; |   |   |
| Hydrogen peroxide | 7722-84-1 | H2O2 |   |   |   |
| Hydrogen sulfide | 7783-06-4 | H2S | &#10004; | &#10004; |   |
| Hydrogen | 1333-74-0 | H2 | &#10004; |   |   |
| Isobutane | 75-28-5 | IC4 |   | &#10004; |   |
| Isopentane | 78-78-4 | IC5 |   | &#10004; |   |
| Krypton | 7439-90-9 | KR | &#10004; | &#10004; |   |
| Lennard-jones_fluid |  | LJF | &#10004; |   |   |
| Methane | 74-82-8 | C1 | &#10004; | &#10004; |   |
| Methanol | 67-56-1 | MEOH | &#10004; | &#10004; | &#10004; |
| Methyl fluoride | 593-53-3 | R41 |   |   |   |
| Methylcyclopentane | 96-37-7 | MTC5 |   |   |   |
| Neon | 7440-01-9 | NE | &#10004; |   |   |
| Nitric oxide | 10102-43-9 | NO |   |   |   |
| Nitrogen | 7727-37-9 | N2 | &#10004; | &#10004; |   |
| Nitrous oxide | 10024-97-2 | N2O |   |   |   |
| Octafluoropropane | 76-19-7 | R218 |   |   |   |
| Ortho-hydrogen | 1333-74-0 | O-H2 | &#10004; |   |   |
| Oxygen | 7782-44-7 | O2 | &#10004; | &#10004; |   |
| Para-hydrogen | 1333-74-0 | P-H2 | &#10004; |   |   |
| Pentafluoroethane | 354-33-6 | R125 |   |   |   |
| Propadiene | 463-49-0 | ALLENE |   |   |   |
| Propane | 74-98-6 | C3 | &#10004; | &#10004; | &#10004; |
| Propylene | 115-07-1 | PRLN |   |   |   |
| Pseudo | XXX | PSEUDO |   |   |   |
| Sulfur dioxide | 7446-09-5 | SO2 |   |   |   |
| Sulfur hexafluoride | 2551-62-4 | F6S |   |   |   |
| Tetrafluoroethylene | 116-14-3 | R1114 |   |   |   |
| Tetrafluorohydrazine | 10036-47-2 | F4N2 |   |   |   |
| Toluene | 108-88-3 | TOLU |   | &#10004; |   |
| Trans-1,3,3,3-tetrafluoropropene | 29118-24-9 | R1234ze |   |   |   |
| Trichlorofluoromethane | 75-69-4 | R11 |   |   |   |
| Trifluoroamine oxide | 13847-65-9 | F3NO |   |   |   |
| Trifluoromethane | 75-46-7 | R23 |   |   |   |
| Water | 7732-18 | H2O | &#10004; | &#10004; | &#10004; |
| Xenon | 7440-63-3 | XE | &#10004; |   |   |
| m-Xylene | 108-38-3 | MXYL |   |   |   |
| n-Butane | 106-97-8 | NC4 | &#10004; | &#10004; | &#10004; |
| n-Decane | 124-18-5 | NC10 | &#10004; | &#10004; | &#10004; |
| n-Docosane | 629-97-0 | NC22 | &#10004; | &#10004; |   |
| n-Dodecane | 112-40-3 | NC12 |   | &#10004; |   |
| n-Eicosane | 112-95-8 | NC20 | &#10004; | &#10004; |   |
| n-Heneicosane | 629-94-7 | NC21 |   | &#10004; |   |
| n-Heptadecane | 629-78-7 | NC17 |   | &#10004; |   |
| n-Heptane | 142-82-5 | NC7 | &#10004; | &#10004; | &#10004; |
| n-Hexadecane | 544-76-3 | NC16 |   | &#10004; |   |
| n-Hexane | 110-54-3 | NC6 | &#10004; | &#10004; | &#10004; |
| n-Hydrogen | 1333-74-0 | N-H2 |   |   |   |
| n-Nonadecane | 629-92-5 | NC19 |   | &#10004; |   |
| n-Nonane | 111-84-2 | NC9 | &#10004; | &#10004; | &#10004; |
| n-Octadecane | 593-45-3 | NC18 |   | &#10004; |   |
| n-Octane | 111-65-9 | NC8 | &#10004; | &#10004; | &#10004; |
| n-Pentacosane | 629-99-2 | NC25 |   | &#10004; |   |
| n-Pentadecane | 629-62-9 | NC15 | &#10004; | &#10004; |   |
| n-Pentan | 109-66-0 | NC5 | &#10004; | &#10004; | &#10004; |
| n-Tetracosane | 646-31-1 | NC24 |   | &#10004; |   |
| n-Tetradecane | 629-59-4 | NC14 |   | &#10004; |   |
| n-Tricosane | 638-67-5 | NC23 |   | &#10004; |   |
| n-Tridecane | 629-50-5 | NC13 |   | &#10004; |   |
| n-Undecane | 1120-21-4 | NC11 |   | &#10004; |   |
| o-Xylene | 95-47-6 | OXYL |   |   |   |
| p-Xylene | 106-42-3 | PXYL |   |   |   |

