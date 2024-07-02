---
layout: default
version: 2.2
title: Utility structs
permalink: /v2.2.0/utility_structs.html
---

Many methods in ThermoPack can return several values. These are commonly packaged in simple structures to facilitate
easy access of the desired properties. This page is an overview of the different structures returned from various ThermoPack methods.

* [Differentials](#differentials)
* [FlashResult](#flashresult)
* [XYDiagram](#xydiagram)
  * [XYEquilibrium](#xyequilibrium)
* BinaryTriplePoint

# Differentials
Property calculations (i.e. [TV-Properties](/thermopack/v2.2.0/thermo_methods.html/#tv-property-interfaces), [TP-Properties](/thermopack/v2.2.0/thermo_methods.html/#tv-property-interfaces),
and [TVP-Properties](/thermopack/v2.2.0/thermo_methods.html/#tvp-property-interfaces)) typically yield a value, and optionally differentials. If any differentials are computed, these 
are returned in a `Differentials` struct, which contains the attributes

* `constant` (`str`) : A string, either `'tvn'` or `'tpn'`, indicating what variables the differentials are computed as functions of.
* `dT` (`float` or `None`) : The temperature derivative (if computed)
* `dp` (`float` or `None`) : The pressure derivative (if computed)
* `dV` (`float` or `None`) : The volume derivative (if computed)
* `dn` (`ndarray[float]` or `None`) : The mole number derivatives (if computed)

Example:
```python
from cubic import PengRobinson
eos = PengRobinson('C1,C3') # PR eos for methane/propane mixture
T = 300 # Kelvin
p = 1e5 # Pascal
n = [1, 5] # mol
h, dh = eos.enthalpy(T, p, n, dhdt=True, dhdn=True) # eos.enthalpy is a TP-property interface
print(h) # Enthalpy (J / mol)
print(dh.constant) # 'tpn'
print(dh.dT) # Derivative wrt. T at constant p, n (i.e. isobaric heat capacity)
print(dh.dV) # None (Not available for TP-properties)
print(dh.dn) # Derivative wrt. mole numbers at constant T, p (i.e. partial molar enthalpies)
print(dh.dp) # None (computation flag not set to True)

V = 1.2 # m^3
h_vap, dh_vap = eos.enthalpy_tv(T, V, n, eos.VAPPH, dhdt=True, dndV=True) # eos.enthalpy_tv is a TV-property interface
print(h_vap) # Vapour phase Enthalpy (J)
print(dh_vap.constant) # 'tvn'
print(dh_vap.dT) # Derivative wrt. T at constant V, n
print(dh_vap.dV) # Derivative wrt. V at constant T, n
print(dh_vap.dn) # None (computation flag not set to True)
print(dh_vap.dp) # None (not available for TV-properties)
```

# FlashResult

There are a variety of flash interfaces in ThermoPack, which all return equivalent `FlashResult` structures, with the attributes

* `flash_type` (`str`) : String indicating the flash variables (`'tp'`, `'ph'`, `'uv'`, etc.)
* `phase` (`int`) : The [phase flag](/thermopack/v2.2.0/phase_flags.html) of the flash result
* `T` (`float`) : Temperature (supplied for TP-flash, computed for other flashes) (Kelvin)
* `p` (`float`) : Pressure (supplied for TP- PH- and PS-flash, computed for others) (Pascal)
* `z` (`ndarray[float]`) : Total molar composition
* `x` (`ndarray[float]`) : Liquid phase molar composition
* `y` (`ndarray[float]`) : Vapour phase molar composition
* `betaV` (`float`) : Equilibrium Vapour fraction
* `betaL` (`float`) : Equilibrium Liquid fraction

# XYDiagram

The methods `get_binary_pxy` and `get_binary_txy` are used to compute the pxy- and Txy- phase diagrams for binary mixtures. The results are returned in
an `XYDiagram` structure, which contains the attributes

* `lle` (`XYEquilibrium`) : Liquid-Liquid equilibrium 
* `l1ve` (`XYEquilibrium`) : Liquid 1 - Vapour equilibrium 
* `l2ve` (`XYEquilibrium`) : Liquid 2 - Vapour equilibrium

The composition and pressure/temperature along each phase boundary is contained in the corresponding `XYEquilibrium` structure.

The `XYDiagram` struct is iterable, and in practice it is often unpacked directly upon being returned, as
```python
from cubic import PengRobinson
eos = PengRobinson('C1,NC6') # PR eos for methane/hexane mixture
lle, l1ve, l2ve = eos.get_binary_pxy(300) # Binary pxy-diagram at 300 K
```

The above is equivalent to

```python
xydiag = eos.get_binary_pxy(300) # Binary pxy-diagram at 300 K
lle = xydiag.lle
l1ve = xydiag.l1ve
l2ve = xydiag.l2ve
```

## XYEquilibrium

The phase boundaries in the binary XY-diagrams are held in `PxyEquilibrium` and `TxyEquilibrium` structures, these contain the attributes

* `type` (`str`) : Either `'lle'`, indicating liquid-liquid equilibrium, or `'lve'`, indicating liquid-vapour equilibrium
* `x1` (`ndarray[float]`) : Mole fraction of component 1 in liquid 1 (only for liquid-liquid equilibria)
* `x2` (`ndarray[float]`) : Mole fraction of component 1 in liquid 2 (only for liquid-liquid equilibria)
* `x` (`ndarray[float]`) : Mole fraction of component 1 in liquid (only for vapour-liquid equilibria)
* `y` (`ndarray[float]`) : Mole fraction of component 1 in vapour (only for vapour-liquid equilibria)
* `p` (`ndarray[float]`) : Pressure (Pascal) (only for `PxyEquilibrium`)
* `T` (`ndarray[float]`) : Temperature (Kelvin) (only for `TxyEquilibrium`)

Thus, to plot the binary phase diagram we can use

```python
lle, l1ve, l2ve = eos.get_binary_pxy(300) # Binary pxy-diagram at 300 K

plt.plot(lle.x1, lle.p) # Liquid 1 miscibility composition
plt.plot(lle.x2, lle.p) # Liquid 2 miscibility composition

plt.plot(l1ve.x, l1ve.p) # Liquid 1 bubble line
plt.plot(l1ve.y, l1ve.p) # Liquid 1 dew line

plt.plot(l2ve.x, l2ve.p) # Liquid 2 bubble line
plt.plot(l2ve.y, l2ve.p) # Liquid 2 dew line
```

And the equivalent for `get_binary_txy`, only replacing `p` for `T`.
