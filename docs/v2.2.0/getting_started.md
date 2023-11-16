---
layout: default
version: 2.2.0
title: Getting Started
permalink: /v2.2.0/getting_started.html
---

# Compatibility
ThermoPack v2.2.0 is completely backwards compatible with v2.1.0, such that all examples in the [guide for v2.1.0](/thermopack/v2.1.0/getting_started.html)
will also work with v2.2.0. This guide primarily aims to demonstrate functionality that is new in v2.2.0.

# Getting started - Python
This is a short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo. Comprehensive documentation for the methods available through the python interface can also be found in the [doc page for the thermo class.](/thermopack/v2.2.0/thermo_methods.html). For more advanced users, a look at the [more advanced page](/thermopack/v2.2.0/more_advanced.html) may also be useful.

Equations of State (EoS's) in ThermoPack are classes. To do calculations for a given mixture an EoS object must first be initialized for that mixture, as demonstrated in the [Initializing an EoS section](#Initialising-an-equation-of-state). Then, a wide variety of thermodynamic computations can be done, as demonstrated in the remaining sections.

## Contents
* [Initialising an equation of state](#initialising-an-equation-of-state)
* [pVT properties](#pvt-properties)
  * [Differentials](#differentials)
* [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)
  * [Flash calculations](#flash-calculations)
  * [Phase envelopes](#phase-envelopes)
    * [Tp- and Tv- envelopes](#tp--and-tv--phase-envelopes)
    * [pxy- and txy- envelopes](#pxy--and-txy--phase-envelopes)
  * [Dew- and bubble points](#dew--and-bubble-points)
* [Isolines](#isolines)
* [Critical point](#critical-point)

## Initialising an equation of state
An overview of available equations of state can be found [here](/thermopack/v2.2.0/method_docs.html).

An EoS is initialized by passing in the [fluid identifiers](/thermopack/v2.2.0/Component-name-mapping.html) of the mixture, for example

```Python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('C1,CO2')
```
will initialize a SAFT-VR Mie EoS for a mixture of methane and CO2. The complete list of component identifiers is in the [Fluid identifiers list](/thermopack/v2.2.0/Component-name-mapping.html). PC-SAFT, SAFT-VRQ Mie and Lee-Kesler EoS are initialized in the same way, as
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

The cubic equations of state are found in the `cubic` module. Available cubic EoS's and more information on the individual cubics, mixing rules, etc. can be found on the [cubic page](/thermopack/v2.2.0/cubic_methods.html).
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
In addition to these, the Translated-Consisten Peng-Robinson is avaiable as
```Python
from thermopack.tcPR import tcPR
tcpr = tcPR('F6S,SO2') # Translated-Consistent PR EoS for SF6/SO2 mixture
```
For more fine-tuned control of the cubic EoS, the parent class [`cubic`](/thermopack/v2.2.0/cubic_methods.html) can be initialised directly, to explicitly 
set mixing rules, alpha-correlation etc.

Cubic-plus association EoS's are available for the SRK and PR EoS through the `cpa` module as
```Python
from thermopack.cpa import SRK_CPA, PR_CPA
srk_cpa = SRK_CPA('H2O,ETOH,PROP1OL') # SRK-CPA EoS for water/ethanol/propanol mixture
pr_cpa = PR_CPA('ACETONE,HEX1OL,CYCLOHEX') # PR-CPA EoS for acetone/hexanol/cyclohexane mixture
```

Several multiparameter EoS's can interfaced through the `multiparameter.multiparam` class. The available multiparameter EoS's are NIST-MEOS, MBWR16 and MBWR32. These are initialized as
```Python
from thermopack.multiparameter import multiparam
nist = multiparam('C3', 'NIST_MEOS') # NIST-MEOS EoS for propane
mbwr16 = multiparam('C1', 'MBWR16') # MBWR16 EoS for methane
mbwr32 = multiparam('C2', 'MBWR32') # MBWR32 EoS for ethane
```
please note that not all fluids are supported for multiparameter equations of state, depending on what parameters are available in the fluid database.

Finally, the Extended-corresponding state EoS is available through the `extended_csp.ext_csp` class as
```Python
from thermopack.extended_csp import ext_csp
eos = ext_csp('C1,C2,C3,NC4', sh_eos='SRK', sh_alpha='Classic',
              sh_mixing='vdW', ref_eos='NIST_MEOS', ref_comp='C3')
```

For more information on the extended-csp EoS please see the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) and the [memo](/thermopack/memo/index.html).

# Doing calculations
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in thermopack are the [specific documentation of the `thermo` class](/thermopack/v2.2.0/thermo_methods.html). Here, a small subset of the functionality is demonstrated.

Note that all input is in SI units (moles/kelvin/pascal/cubic meters/joule)

## pVT-properties

For documentation of new methods introduced in v2.2.0, se the [method docs.](/thermopack/v2.2.0/method_docs.html)

### Differentials

No changes from [v2.1.0](/thermopack/v2.1.0/getting_started.html#differentials)

## Phase diagrams and Equilibria

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to use them is the [documentation of the `thermo` class.](/thermopack/v2.2.0/thermo_methods.html). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

### Flash calculations
Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`.

See the [Flash interfaces](/thermopack/v2.2.0/thermo_methods.html#flash-interfaces) in the [documentation of the `thermo` class](/thermopack/v2.2.0/thermo_methods.html) for the specifics on the different flash routines.

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
flsh = eos.two_phase_tpflash(T, p, x) # flsh is a FlashResult object
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
the result of the flash is accessed from the attributes of the `FlashResult` object, found in [`utils.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/utils.py), as
```Python
# Continued
x = flsh.x # Liquid composition
y = flsh.y # Vapour composition
betaL = flsh.betaL # 
# ... etc
```

The `FlashResult` object returned by the different flash routines all contain the same attributes. 

### Phase envelopes

ThermoPack has interfaces to trace (T,p)-, (T,v)-, (p,x,y)- and (T, x, y)- phase envelopes. For the full documentation, see the [docs of the `thermo` class](/thermopack/v2.2.0/thermo_methods.html#saturation-interfaces). For more comprehensive examples, see the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples).

#### Tp- and Tv- phase envelopes

No changes from [v2.1.0](/thermopack/v2.1.0/getting_started.html#tp--and-tv--phase-envelopes)

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

```
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

No changes from [v2.1.0](/thermopack/v2.1.0/getting_started.html#dew--and-bubble-points)

## Isolines

For new functionality allowing computation of isolines in the metastable region, see the [method docs.](/thermopack/v2.2.0/thermo_methods.html)

## Critical point

For new features in the critical point solvers, see the [method docs.](/thermopack/v2.2.0/thermo_methods.html)

