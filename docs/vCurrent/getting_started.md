---
layout: default
version: 
title: Getting Started
permalink: /vcurrent/getting_started.html
---

# Getting started - Python
This is a short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo. Comprehensive documentation for the methods available through the python interface can also be found in the [wiki](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class). For more advanced users, a look at the [more advanced page in the wiki](https://github.com/thermotools/thermopack/wiki/Advanced-usage#more-advanced-usage---python) may also be useful.

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
An EoS is initialized by passing in the [fluid identifiers](https://github.com/thermotools/thermopack/wiki/Component-name-mapping) of the mixture, for example

```Python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('C1,CO2')
```
will initialize a SAFT-VR Mie EoS for a mixture of methane and CO2. The complete list of component identifiers is in the [Fluid identifiers section of the wiki](https://github.com/thermotools/thermopack/wiki/Component-name-mapping). PC-SAFT, SAFT-VRQ Mie and Lee-Kesler EoS are initialized in the same way, as
```Python
from thermopack import saftvrmie, saftvrqmie, pcsaft, lee_kesler
svrm = saftvrmie.saftvrmie('AR,KR') # SAFT-VR Mie EoS for Ar/Kr mixture
svrqm = saftvrqmie.saftvrqmie('HE') # SAFT-VRQ Mie EoS for pure He
pcs = pcsaft.pcsaft('BENZENE,NC6,NC12') # PC-SAFT EoS for ternary benzene/hexane/dodecane mixture
lk = lee_kesler.lee_kesler('N2,O2') # Lee-Kesler EoS for nitrogen/oxygen mixture 
```

The cubic equations of state are all interfaced through the `cubic` class. Available cubic EoS's can are SRK, PR, VdW, SW, PT and tcPR. More information on the individual cubics, mixing rules, etc. can be found in the [wiki](https://github.com/thermotools/thermopack/wiki/Cubic-equations-of-state). The specific cubic EoS to initialize is specified with a string as
```Python
from thermopack.cubic import cubic
srk = cubic('NH3,C2', 'SRK') # SRK EoS for ammonia/ethane mixture
pr = cubic('IC4,NC10', 'PR') # PR EoS for isobutane/decane mixture
vdw = cubic('C1,C2,C3,N2,O2', 'VdW') # VdW EoS for methane/ethane/propane/nitrogen/oxygen mixture
sw = cubic('R11,R12', 'SW') # Schmidt-Wensel EoS for FCl3C/F2Cl2C mixture
pt = cubic('PRLN', 'PT') # Patel-Teja EoS for pure propylene
tcpr = cubic('F6S,SO2', 'tcPR') # Translated-Consistent PR EoS for SF6/SO2 mixture
```

Cubic-plus association EoS's are available for the SRK and PR EoS through the `cpa` class as
```Python
from thermopack.cpa import cpa
srk_cpa = cpa('H2O,ETOH,PROP1OL', 'SRK') # SRK-CPA EoS for water/ethanol/propanol mixture
pr_cpa = cpa('ACETONE,HEX1OL,CYCLOHEX', 'PR') # PR-CPA EoS for acetone/hexanol/cyclohexane mixture
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

For more information on the extended-csp EoS please see the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) and the [wiki](https://github.com/thermotools/thermopack/wiki/Extended_CSP-equations-of-state).

# Doing calculations
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in thermopack are the [specific documentation of the `thermo` class](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class). Here, a small subset of the functionality is demonstrated.

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
where `eos.VAPPH` and `eos.LIQPH` are [phase flags](https://github.com/thermotools/thermopack/wiki/Phase-flags) used to identify different phases. The commas are necessary because all output from thermopack methods are as tuples. 

Similarly, pressure, internal energy, enthalpy, entropy, etc. and associated differentials can be computed via the methods `chemical_potential_tv(T, V, n)`, `internal_energy_tv(T, V, n)`, `enthalpy_tv(T, V, n)`, `helmholtz_tv(T, V, n)`, `entropy_tv(T, V, n)`. For a full overview of the available property calculations see the [TV-property interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#TV-property-interfaces) and the [Tp-property interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#Tp-property-interfaces) of the [`thermo` class](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#methods-in-the-thermo-class-thermopy).

### Differentials

If we want volume differentials, we use the same method, but set the flags to calculate differentials to `True`:

```python
# Continued 
vg, dvdp = eos.specific_volume(T, p, x, eos.VAPPH, dvdp=True) # Vapour phase molar volume and pressure differential
vl, dvdT = eos.specific_volume(T, p, x, eos.LIQPH, dvdt=True) # Liquid phase molar volume and temperature differential
_, dvdn = eos.specific_volume(T, p, x, eos.LIQPH, dvdn=True) # Liquid phase partial molar volumes
```

Differentials can be computed as functions of $(T, V, n)$ or as functions of $(T, p, n)$. For an overview of the different methods, see [Advanced usage: Different property interfaces](https://github.com/thermotools/thermopack/wiki/Advanced-usage#the-different-property-interfaces-tv--tp--and-tvp-). A short example is given here as:

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

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to use them are the [docstrings in `thermo.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

### Flash calculations
Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`.

See the [Flash interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#flash-interfaces) in the [documentation of the `thermo` class](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#methods-in-the-thermo-class-thermopy) for the specifics on the different flash routines.

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
```
# Continued
x = flsh.x # Liquid composition
y = flsh.y # Vapour composition
betaL = flsh.betaL # 
# ... etc
```

The `FlashResult` object returned by the different flash routines all contain the same attributes. 

### Phase envelopes

ThermoPack has interfaces to trace (T,p)-, (T,v)- and (p,x,y)-phase envelopes. For the full documentation, see the [docs of the `thermo` class](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#Saturation-interfaces). For more comprehensive examples, see the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples).

#### Tp- and Tv- phase envelopes

Phase envelopes can be generated directly with the method `get_envelope_twophase()` as

```python
# Continued
T, p = eos.get_envelope_twophase(1e5, x) # arrays of temperature and pressure for phase envelope, starting at 1 bar.
plt.plot(p, T) # Tp-projection of phase envelope

T, p, v = eos.get_envelope_twophase(1e5, x, calc_v=True) # Also return the specific volume at each point along the phase envelope
plt.plot(1 / v, T) # rho-T projection of the phase envelope
```

#### pxy- phase envelopes

To compute pxy-type phase envelopes, we use the `get_binary_pxy()` method. The pxy-phase diagram trace method assumes that there can be up to three phases present, two liquid and one vapour. The method `get_binary_pxy` therefore returns three tuples, that we call `LLE`, `L1VE` and `L2VE`. Each of these tuples corresponds to a phase boundary: The `LLE` tuple corresponds to the (Liquid 1 - Liquid 2) phase boundary, the `L1VE` tuple corresponds to the (Liquid 1 - Vapour) phase boundary, and the `L2VE` tuple corresponds to the (Liquid 2 - Vapour) phase boundary.

Each of these tuples contains three arrays `(x, y, p)`: The first array (`x`) is the liquid composition along the phase boundary (Liquid 1 for `LLE`), the second array (`y`) is the vapour composition along the phase boundary (Liquid 2 for `LLE`), the third array (`p`) is the pressure along the phase boundary.

**Note**: If one or more of the phase boundaries does not exist, the corresponding tuple will be `(None, None, None)`.

**Note**: The compositions returned by `get_binary_pxy` refer to the mole fraction of component 1.

**Note**: The minimum and maximum pressure of the phase envelope trace can be set with the kwargs `minimum_pressure` and `maximum_pressure`. For the full details, see the [docs for the `thermo` class](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#get_binary_pxyself-temp-maximum_pressure150000000-minimum_pressure1000000-maximum_dz0003-maximum_dlns001).

For a mixture with a Liquid-Liquid equilibrium and two Liquid-Vapour equilibria, we can plot the entire pxy-phase diagram as:

```python
import matplotlib.pyplot as plt
from thermopack.cpa import cpa

eos = cpa('NC12,H2O', 'SRK')  # CPA-SRK eos for Dodecane/water mixture
T = 350  # kelvin
LLE, L1VE, L2VE = eos.get_binary_pxy(T)  # Returns three tuples containing three arrays each

# Liquid-liquid phase boundaries
# LLE[2] is the pressure along the liquid-liquid phase boundary
plt.plot(LLE[0], LLE[2], label='Liquid 1 composition') # LLE[0] is the mole fraction of component 1 (NC12) in Liquid 1 along the phase boundary
plt.plot(LLE[1], LLE[2], label='Liquid 2 composition') # LLE[1] is the mole fraction of component 1 (NC12) in Liquid 2 along the phase boundary

# Liquid 1-vapour phase boundaries
# L1VE[2] is the pressure along the Liquid 1 - Vapour phase boundary
plt.plot(L1VE[0], L1VE[2], label='Liquid 1 bubble line') # L1VE[0] is the mole fraction of component 1 (NC12) in Liquid 1 along the Liquid 1 - Vapour phase boundary
plt.plot(L1VE[1], L1VE[2], label='Liquid 1 dew line') # L1VE[1] is the mole fraction of component 1 (NC12) in the Vapour phase along the Liquid 1 - Vapour phase boundary

# Liquid 2-vapour phase boundaries
# L2VE[2] is the pressure along the Liquid 2 - Vapour phase boundary
plt.plot(L2VE[0], L2VE[2], label='Liquid 2 bubble line') # L2VE[0] is the mole fraction of component 1 (NC12) in Liquid 2 along the Liquid 2 - Vapour phase boundary
plt.plot(L2VE[1], L2VE[2], label='Liquid 2 dew line') # L2VE[1] is the mole fraction of component 1 (NC12) in the Vapour phase along the Liquid 2 - Vapour phase boundary

plt.ylabel('Pressure [Pa]') # The third element in each tuple is the pressure along the phase boundary
plt.xlabel('Molar composition')
```

If we are unsure whether one or more of the equilibria exists, we need to check whether the corresponding tuple contains `None`, as:

```Python
import matplotlib.pyplot as plt
from thermopack.cubic import cubic

eos = cubic('C3,NC6', 'PR') # PR EoS for propane/n-hexane mixture

T = 300 # Kelvin

LLE, L1VE, L2VE = eos.get_binary_pxy(T)

if LLE[0] is None: # If there are fewer than two liquid phases, LLE = (None, None, None)
    print('There are fewer than two liquid phases at all evaluated pressures!')
else:
    plt.plot(LLE[0], LLE[2], label='Liquid 1 composition') # LLE[0] is the mole fraction of component 1 (C3) in Liquid 1 along the phase boundary
    plt.plot(LLE[1], LLE[2], label='Liquid 2 composition') # LLE[1] is the mole fraction of component 1 (C3) in Liquid 2 along the phase boundary

if L1VE[0] is None: # If no phase boundaries are found, L1VE = (None, None, None)
    print('There is no Liquid - Vapour equilibria at any of the evaluated pressures!')
else:
    plt.plot(L1VE[0], L1VE[2], label='Liquid 1 bubble line') # L1VE[0] is the mole fraction of component 1 (C3) in Liquid 1 along the Liquid 1 - Vapour phase boundary
    plt.plot(L1VE[1], L1VE[2], label='Liquid 1 dew line') # L1VE[1] is the mole fraction of component 1 (C3) in the Vapour phase along the Liquid 1 - Vapour phase boundary

if L2VE[0] is None: # If LLE = (None, None, None) then L2VE will also be L2VE = (None, None, None), because there is no Liquid 2
    print('There are fewer than two liquid phases at all evaluated pressures!')
else:
    plt.plot(L2VE[0], L2VE[2], label='Liquid 2 bubble line') # L2VE[0] is the mole fraction of component 1 (C3) in Liquid 2 along the Liquid 2 - Vapour phase boundary
    plt.plot(L2VE[1], L2VE[2], label='Liquid 2 dew line') # L2VE[1] is the mole fraction of component 1 (C3) in the Vapour phase along the Liquid 2 - Vapour phase boundary

plt.ylabel('Pressure [Pa]') # The third element in each tuple is the pressure along the phase boundary
plt.xlabel('Molar composition')
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

