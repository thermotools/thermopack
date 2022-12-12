# Thermopack

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

Thermopack is available for everybody, free of charge under the
MIT/Apache 2.0 open-source licenses. Thermopack is written in FORTRAN
to handle heavy numerical computations associated with process and
computational fluid dynamics (CFD) simulations. The thermodynamic
framework is easily interfaced from C/C++ and also contains a flexible
Python wrapper to make scripting easy. The Python interface is also a
building block for the Thermopack graphical user interface, where it
is possible to plot thermodynamic phase diagrams with the most
frequently used equations of state. The graphical user interface is
currently running on the Windows and Linux operating systems.

## Table of contents
  * [Program structure](#program-structure)
  * [Please cite](#please-cite)
  * [Authors and contact persons](#authors-and-contact-persons)
  * [License](#license)
  * [Acknowledgments](#acknowledgments)
  * [Getting started](#getting-started)
  * [Doing calculations](#doing-calculations)
  * [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)

## Program structure
The core of ThermoPack is the Fortran module. For more information on that, see the [GitHub page](https://github.com/thermotools/thermopack).
This page contains an introduction to the ThermoPack python-wrapper, which is an object-oriented implementation of a variety of equations of state (EoS), 
with integrated methods for computing thermodynamic properties.

Each EoS in thermopack is a class, which inherits from the `thermopack` class found in `thermo.py`. The primary documentation
for the thermopack python wrapper consists of the [docstrings of the thermopack class](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py).
This class contains all generic methods used to compute thermodynamic properties, phase equilibria, etc. The inheriting 
classes simply ensure that the correct part of the Fortran-module is linked when performing calculations, and provide 
some extended functionality for handling EoS parameters and such. See the [wiki](https://github.com/thermotools/thermopack/wiki/) 
for more information on this. 

Fluid parameters are compiled into the Fortran-module, and are not directly accessible through the Python-wrapper. 
The entire fluid parameter database used by thermopack may be found in the [`/fluids` directory](https://github.com/thermotools/thermopack/tree/main/fluids) 
in the GitHub repo. In order to model fluids not currently supported in the module available through `pip`, thermopack
must be compiled from source with the new parameters. See the [wiki](https://github.com/thermotools/thermopack/wiki/) 
for information on how to add new fluids, and the [GitHub README](https://github.com/thermotools/thermopack) for a guide 
on how to compile from source. Please feel free to leave a PR for new parameter sets such that these can be included in 
future releases of thermopack.

## Please cite
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

## Authors and contact persons
Morten Hammer (morten.hammer@sintef.no)<br>
Ailo Aasen (ailo.aasen@sintef.no)<br>
Øivind Wilhelmsen (oivind.wilhelmsen@sintef.no)

## License
Thermopack is distributed under the [MIT
license](https://github.com/thermotools/thermopack/blob/main/LICENSE-MIT)
and [Apache
2.0](https://github.com/thermotools/thermopack/blob/main/LICENSE-APACHE).

## Acknowledgments
A number of colleagues at SINTEF Energy Research and NTNU have contributed to the
development of thermopack. We gratefully acknowledge their contributions.

## Getting started
This is a very short introduction to thermopack. Once you've gotten started, we recomend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo.

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

## Doing calculations
Now that we have an EoS initialized we can start computing stuff. The primary source on how to use individual methods in thermopack are the docstrings in the `thermopack` class, [found in `/addon/pycThermopack/thermopack/thermo.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py). Here, a small subset of the functionality is demonstrated.

Note that all input is in SI units (moles/kelvin/pascal/cubic meters/joule)

Specific volume, given temperature, pressure and composition is computed as 
```python
from thermopack.saftvrmie import saftvrmie
eos = saftvrmie('NC6,NC12') # Hexane/dodecane mixture
T = 300 # Kelvin
p = 1e5 # Pascal
x = [0.2, 0.8] # Molar composition
vg, = eos.specific_volume(T, p, x, 2) # Molar volume of gas phase (NB: Notice the comma)
vl, = eos.specific_volume(T, p, x, 1) # Molar volume of liquid phase (NB: Notice the comma)
```
where the numbers `2` and `1` are [phase flags](https://github.com/thermotools/thermopack/wiki/Phase-flags) used to identify different phases. The commas are necessary because all output from thermopack methods are as tuples. If we want volume differentials, we use the same method:

```python
# Continued 
vg, dvdT = eos.specific_volume(T, p, x, 2, dvdp=True) # Vapour phase molar volume and pressure differential
vl, dvdp = eos.specific_volume(T, p, x, 1, dvdt=True) # Liquid phase molar volume and temperature differential
_, dvdn = eos.specific_volume(T, p, x, 1, dvdn=True) # Liquid phase partial molar volumes
```

Similarly, internal energy, enthalpy, entropy, etc. and associated differentials can be computed via the methods `chemical_potential_tv(T, V, n)`, `internal_energy_tv(T, V, n)`, `enthalpy_tv(T, V, n)`, `helmholtz_tv(T, V, n)`, `entropy_tv(T, V, n)`. 

## Phase diagrams and Equilibria

As with other calculations, the primary source on how available methods for flash- and equilibria calculations and how to use them are the [docstrings in `thermo.py`](https://github.com/thermotools/thermopack/blob/main/addon/pycThermopack/thermopack/thermo.py). Here we give a short introduction, for more extensive examples see the [pyExamples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) directory.

Flash calculations of several kinds are handled by the methods `twophase_tpflash()`, `twophase_psflash()`, `twophase_phflash()` and `twophase_uvflash()`. They are used as

```python
from thermopack.saftvrqmie import saftvrqmie
eos = saftvrqmie('H2,HE,NE') # Helium/Neon mixture
T = 35 # Kelvin
p = 10e5 # Pascal (10 bar)
x = [0.1, 0.25, 0.65] # Molar composition
x, y, vapfrac, liqfrac, phasekey = eos.two_phase_tpflash(T, p, x) # x and y are vapour and liquid composition respectively
```

Phase envelopes can be generated directly with the methods `get_envelope_twophase()` as

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

We can also compute the bubble-temperature, pressure etc. directly using the methods `bubble_temperature(p, z)`, `bubble_pressure(T, z)`, `dew_temperature(p, z)` and `dew_pressure(T, z)`, where `z` is the composition of the mixture.


