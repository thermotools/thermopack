# Getting started - Python
This is a very short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo.

## Initialising an equation of state
An EoS is initialized by passing in the fluid identifiers of the mixture, for example

```Python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('C1,CO2')
```
will initialize a SAFT-VR Mie EoS for a mixture of methane and CO2. The complete list of component identifiers is in the [wiki](https://github.com/thermotools/thermopack/wiki/Component-name-mapping). PC-SAFT, SAFT-VRQ Mie and Lee-Kesler EoS are initialized in the same way, as
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
pr_cpa = cpa('ACETONE,HEX1OL,CYCLOHEX') # PR-CPA EoS for acetone/hexanol/cyclohexane mixture
```

Several multiparameter EoS's can interfaced through the `multiparameter.multiparam` class. The available multiparameter EoS's are NIST-MEOS, MBWR16 and MBWR32. These are initialized as
```Python
from thermopack.multiparameter import multiparam
nist = multiparam('C3', 'NIST_MEOS') # NIST-MEOS EoS for CO2
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
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in thermopack are the docstrings in the `thermopack` class, [found in `/addon/pycThermopack/thermopack/thermo.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py). Here, a small subset of the functionality is demonstrated.

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
where `eos.VAPPH` and `eos.LIQPH` are [phase flags](https://github.com/thermotools/thermopack/wiki/Phase-flags) used to identify different phases. The commas are necessary because all output from thermopack methods are as tuples. If we want volume differentials, we use the same method:

```python
# Continued 
vg, dvdT = eos.specific_volume(T, p, x, eos.VAPPH, dvdp=True) # Vapour phase molar volume and pressure differential
vl, dvdp = eos.specific_volume(T, p, x, eos.LIQPH, dvdt=True) # Liquid phase molar volume and temperature differential
_, dvdn = eos.specific_volume(T, p, x, eos.LIQPH, dvdn=True) # Liquid phase partial molar volumes
```

Similarly, internal energy, enthalpy, entropy, etc. and associated differentials can be computed via the methods `chemical_potential_tv(T, V, n)`, `internal_energy_tv(T, V, n)`, `enthalpy_tv(T, V, n)`, `helmholtz_tv(T, V, n)`, `entropy_tv(T, V, n)`. 

## Phase diagrams and Equilibria

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to use them are the [docstrings in `thermo.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

### Flash calculations
Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`. They are used as

```python
from thermopack.saftvrqmie import saftvrqmie
eos = saftvrqmie('H2,HE,NE') # Helium/Neon mixture
T = 35 # Kelvin
p = 10e5 # Pascal (10 bar)
x = [0.1, 0.25, 0.65] # Molar composition
x, y, vapfrac, liqfrac, phasekey = eos.two_phase_tpflash(T, p, x) # x and y are vapour and liquid composition respectively
```

Note that some of the methods also output other information, such as the temperature of the flash, see the docstrings of the `thermo` class in `thermo.py` for more details.

### Phase envelopes

Phase envelopes can be generated directly with the method `get_envelope_twophase()` as

```python
# Continued
T, p = eos.get_envelope_twophase(1e5, x) # arrays of temperature and pressure for phase envelope, starting at 1 bar.
plt.plot(p, T) # Tp-projection of phase envelope

T, p, v = eos.get_envelope_twophase(1e5, x, calc_v=True) # Also return the specific volume at each point along the phase envelope
plt.plot(1 / v, T) # rho-T projection of the phase envelope
```

To compute pxy-type phase envelopes, we use the `get_binary_pxy()` method as

```python
import matplotlib.pyplot as plt
from thermopack.cpa import cpa

eos = cpa('NC12,H2O', 'SRK')  # CPA eos for Dodecane/water mixture
T = 350  # kelvin
LLE, L1VE, L2VE = eos.get_binary_pxy(T)  # Returns three tuples containing three arrays each

# Liquid-liquid phase boundaries
plt.plot(LLE[0], LLE[2], label='Liquid 1 composition')
plt.plot(LLE[1], LLE[2], label='Liquid 2 composition')

# Liquid 1-vapour phase boundaries
plt.plot(L1VE[0], L1VE[2], label='Liquid 1 bubble line')
plt.plot(L1VE[1], L1VE[2], label='Liquid 1 dew line')

# Liquid 2-vapour phase boundaries
plt.plot(L2VE[0], L2VE[2], label='Liquid 2 bubble line')
plt.plot(L2VE[1], L2VE[2], label='Liquid 2 dew line')

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

```
eos = saftvrqmie('HE,NE') # Use FH-corrected Mie potentials for Helium calculations!
n = [5, 10]
Tc, pc, Vc = eos.critical(n) # Compute the critical temperature, pressure and volume given mole numbers
vc = Vc / sum(n) # Critical specific volume computed from critical volume and mole numbers.
```

The solver accepts initial guesses for the critical values through the `kwargs` `temp`, and `v`. The error tolerance can be set via the `tol` `kwarg` (default is `tol=1e-7`).
