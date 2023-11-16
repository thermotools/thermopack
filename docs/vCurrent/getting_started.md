---
layout: default
version: 
title: Getting Started
permalink: /vcurrent/getting_started.html
---

# Getting started - Python
This is a short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo. Comprehensive documentation for the methods available through the python interface can also be found in the [doc page for the thermo class.](/thermopack/vcurrent/thermo_methods.html). For more advanced users, a look at the [more advanced page](/thermopack/vcurrent/more_advanced.html) may also be useful.

Equations of State (EoS's) in ThermoPack are classes. To do calculations for a given mixture an EoS object must first be initialized for that mixture, as demonstrated in the [Initializing an EoS section](#Initialising-an-equation-of-state). Then, a wide variety of thermodynamic computations can be done, as demonstrated in the remaining sections.

## Contents
* [Initialising an equation of state](#initialising-an-equation-of-state)
* [pVT properties](#pvt-properties)
  * [Differentials](#differentials)
* [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)
  * [Flash calculations](#flash-calculations)
  * [Phase envelopes](#phase-envelopes)
    * [Tp- and Tv- envelopes](#tp--and-tv--phase-envelopes)
    * [pxy- envelopes](#pxy--phase-envelopes)
  * [Dew- and bubble points](#dew--and-bubble-points)
* [Isolines](#isolines)
* [Critical point](#critical-point)

## Initialising an equation of state
An overview of available equations of state can be found [here](/thermopack/vcurrent/method_docs.html).

An EoS is initialized by passing in the [fluid identifiers](/thermopack/vcurrent/Component-name-mapping.html) of the mixture, for example

```Python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('C1,CO2')
```
will initialize a SAFT-VR Mie EoS for a mixture of methane and CO2. The complete list of component identifiers is in the [Fluid identifiers list](/thermopack/vcurrent/Component-name-mapping.html). PC-SAFT, SAFT-VRQ Mie and Lee-Kesler EoS are initialized in the same way, as
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

The cubic equations of state are found in the `cubic` module. Available cubic EoS's and more information on the individual cubics, mixing rules, etc. can be found on the [cubic page](/thermopack/vcurrent/cubic_methods.html).
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
For more fine-tuned control of the cubic EoS, the parent class [`cubic`](/thermopack/vcurrent/cubic_methods.html) can be initialised directly, to explicitly 
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
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in thermopack are the [specific documentation of the `thermo` class](/thermopack/vcurrent/thermo_methods.html). Here, a small subset of the functionality is demonstrated.

Note that all input is in SI units (moles/kelvin/pascal/cubic meters/joule)

## pVT-properties
Specific volume, given temperature, pressure and composition is computed as 
```python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('NC6,NC12') # Hexane/dodecane mixture
T = 300 # Kelvin
p = 1e5 # Pascal
x = [0.2, 0.8] # Molar composition
vg, = eos.specific_volume(T, p, x, eos.VAPPH) # Molar volume of gas phase (NB: Notice the comma)
vl, = eos.specific_volume(T, p, x, eos.LIQPH) # Molar volume of liquid phase (NB: Notice the comma)
```
where `eos.VAPPH` and `eos.LIQPH` are [phase flags](/thermopack/vcurrent/phase_flags.html) used to identify different phases. The commas are necessary because all output from thermopack methods are as tuples. 

Similarly, pressure, internal energy, enthalpy, entropy, etc. and associated differentials can be computed via the methods `chemical_potential_tv(T, V, n)`, `internal_energy_tv(T, V, n)`, `enthalpy_tv(T, V, n)`, `helmholtz_tv(T, V, n)`, `entropy_tv(T, V, n)`. For a full overview of the available property calculations see the [TV-property interfaces](/thermopack/vcurrent/thermo_methods.html#TV-property-interfaces) and the [Tp-property interfaces](/thermopack/vcurrent/thermo_methods.html#Tp-property-interfaces) of the [`thermo` class](/thermopack/vcurrent/thermo_methods.html#methods-in-the-thermo-class-thermopy).

### Differentials

If we want volume differentials, we use the same method, but set the flags to calculate differentials to `True`:

```python
# Continued 
vg, dvdp = eos.specific_volume(T, p, x, eos.VAPPH, dvdp=True) # Vapour phase molar volume and pressure differential
vl, dvdT = eos.specific_volume(T, p, x, eos.LIQPH, dvdt=True) # Liquid phase molar volume and temperature differential
_, dvdn = eos.specific_volume(T, p, x, eos.LIQPH, dvdn=True) # Liquid phase partial molar volumes
```

Differentials can be computed as functions of $(T, V, n)$ or as functions of $(T, p, n)$. For an overview of the different methods, see [Advanced usage: Different property interfaces](/thermopack/vcurrent/more_advanced.html). A short example is given here as:

```Python
# Continued
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

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to use them is the [documentation of the `thermo` class.](/thermopack/vcurrent/thermo_methods.html). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

### Flash calculations
Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`.

See the [Flash interfaces](/thermopack/vcurrent/thermo_methods.html#flash-interfaces) in the [documentation of the `thermo` class](/thermopack/vcurrent/thermo_methods.html) for the specifics on the different flash routines.

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

ThermoPack has interfaces to trace (T,p)-, (T,v)- and (p,x,y)-phase envelopes. For the full documentation, see the [docs of the `thermo` class](/thermopack/vcurrent/thermo_methods.html#saturation-interfaces). For more comprehensive examples, see the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples).

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

```
p = 1e5
lle, l1ve, l2ve = eos.get_binary_txy(p) # Unpacking the XYDiagram 

# Liquid-liquid phase boundaries
# l1ve holds the composition and pressure of Liquid 1 and Vapour along the phase boundary
plt.plot(l1ve.x, l1ve.T, label='Liquid 1 composition') # l1ve.x is the mole fraction of component 1 (NC12) in Liquid 1 along the phase boundary
plt.plot(l1ve.y, l1ve.T, label='Vapour composition') # l1ve.y is the mole fraction of component 1 (NC12) in Vapour along the phase boundary

# ... etc ...
```

If an equilibrium is not found, for example there is only one vapour-liquid equilibria, and no liquid-liquid equilibria,
the arrays corresponding to the non-existent equilibria are empty, i.e.
```
T = 300
pxy = get_binary_pxy(T) # Some mixture that has only one vapour - liquid equilibria at 300 K
print(len(pxy.lle.x1) == 0) # True
print(len(pxy.lle.x2) == 0) # True
print(len(pxy.lle.p) == 0) # True
print(len(pxy.l1ve.x) == 0) # False
print(len(pxy.l1ve.y) == 0) # False
print(len(pxy.l1ve.p) == 0) # False
print(len(pxy.l2ve.x) == 0) # True
print(len(pxy.l2ve.y) == 0) # True
print(len(pxy.l2ve.p) == 0) # True
```

### Dew- and bubble points

We can also compute the bubble-temperature, pressure etc. directly using the methods `bubble_temperature(p, z)`, `bubble_pressure(T, z)`, `dew_temperature(p, z)` and `dew_pressure(T, z)`, where `z` is the composition of the mixture, as

```Python
eos = cubic('CO2,CH4', 'SRK')
x = [0.5, 0.5] # Total composition of the mixture
p_dew, y_dew = eos.dew_pressure(273, x) # Calculates dew pressure and dew composition at 273 K
T_dew, y_dew = eos.dew_temperature(1e5, x) # Calculates dew temperature and dew composition at 1 bar
p_bub, x_bub = eos.bubble_pressure(273, x) # Calculates bubble pressure and bubble composition at 273 K
T_bub, x_bub = eos.bubble_temperature(1e5, x) # Calculates bubble temperature and bubble composition at 1 bar
```

## Isolines

Various isolines can be computed using the methods `get_isotherm`, `get_isobar`, `get_isentrope` and `get_isenthalp`. In the following code snippet, the default values of the keyword arguments are indicated.

```Python
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

## Critical point

Thermopack has a critical point solver, which is called as

```Python
eos = saftvrqmie('HE,NE') # Use FH-corrected Mie potentials for Helium calculations!
n = [5, 10]
Tc, Vc, pc = eos.critical(n) # Compute the critical temperature, pressure and volume given mole numbers
vc = Vc / sum(n) # Critical specific volume computed from critical volume and mole numbers.
```

The solver accepts initial guesses for the critical values through the `kwargs` `temp`, and `v`. The error tolerance can be set via the `tol` `kwarg` (default is `tol=1e-7`).

