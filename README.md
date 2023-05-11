<!--- 
Generated at: 2023-03-30T18:56:32.136359
This is an auto-generated file, generated using the script at thermopack/addon/pyUtils/docs/join_docs.py
The file is created by joining the contents of the files
    thermopack/doc/markdown/
        header.md
        github_toc.md
        cite_acknowl_licence.md
        structure.md
        source_build.md
        getting_started.md
        more_advanced.md
--->
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
# Table of contents
  * [Please cite](#please-cite)
  * [Authors and contact persons](#authors-and-contact-persons)
  * [License](#license)
  * [Acknowledgments](#acknowledgments)
  * [Program structure](#program-structure)
  * [Building from Source](#Building-from-source)
  * [Getting started - Python](#getting-started---python)
    * [Initialising an equation of state](#Initialising-an-equation-of-state)
  * [Doing calculations](#doing-calculations)
    * [pVT-properties](#pVT-properties)
    * [Phase diagrams and equilibria](#phase-diagrams-and-equilibria)
    * [Isolines](#Isolines)
    * [Critical point](#critical-point)
  * [Advanced usage - Python](#More-advanced-usage---Python)
    * [Interaction parameters](#Interaction-parameters) 
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
Morten Hammer (morten.hammer@sintef.no,morten.hammer@ntnu.no)<br>
Ailo Aasen (ailo.aasen@sintef.no)<br>
Øivind Wilhelmsen (oivind.wilhelmsen@sintef.no)<br>
Vegard Gjeldvik Jervell (vegard.g.jervell@ntnu.no)

## License
Thermopack is distributed under the [MIT
license](https://github.com/thermotools/thermopack/blob/main/LICENSE-MIT)
and [Apache
2.0](https://github.com/thermotools/thermopack/blob/main/LICENSE-APACHE).

## Acknowledgments
A number of colleagues at SINTEF Energy Research and NTNU have contributed to the
development of thermopack. We gratefully acknowledge their contributions.
# Program structure

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

## File System
Brief description of file structure:

- `thermopack/`: Main library folder containing make scripts etc.
- `thermopack/src/`: Main path for Fortran source code
- `thermopack/unittest/`: Test files written for pFUunit
- `thermopack/bin/`: Compiled binaries and libraries
- `thermopack/doc/`: Memos and doxygen documentation
- `thermopack/fluids/`: Pure fluid files
- `thermopack/binaries/`: Files containing binary interaction parameters etc.
- `thermopack/MSVStudio/`: Microsoft Visual Studio project and solution files
- `thermopack/include/`: C/C++ include file
- `thermopack/pyplot/`: Example plot-scripts (Plotting text files generated by Thermopack)
- `thermopack/addon/`: Add-on functionality
- `thermopack/addon/pycThermopack/`: Python interface
- `thermopack/addon/pyUtils/`: Python utilities for generating fortran code and makefile input.
- `thermopack/addon/trend_interface/`: Interface for working with the TREND/EOSCG library developed by Roland Span and Ruhr-Universität Bochum

## Building from source
The following sections show how to fetch, compile and install Thermopack and
the Python frontend pycThermopack. When things are properly installed, it may
be useful to look into the examples provided in the
[addon/pyExamples](addon/pyExamples/README.md).

### pypi
Thermopack has been compiled for Windows, Linux and macOS
and made available on the Python Package Index (pypi), and can be
installed using pip.

```bash
pip3 install --user thermopack
```

### Prerequisites
Thermopack source code can be compiled with the [GNU Fortran
compiler](https://gcc.gnu.org/wiki/GFortran) or [Intel
FORTRAN](https://software.intel.com/content/www/us/en/develop/tools/compilers/fortran-compilers.html)
and is dependent on the [LAPACK](http://www.netlib.org/lapack/) and
[BLAS](http://www.netlib.org/blas/) libraries. On the Windows OS the code can
be compiled using [Microsoft Visual
Studio](https://visualstudio.microsoft.com/vs/). A solution file is found in
[thermopack/MSVStudio](https://github.com/thermotools/thermopack/tree/main/MSVStudio),
assuming that the Intel Fortran compiler is integrated with Microsoft Visual
Studio.

### Linux setup
The Thermopack source code is downloaded by cloning the library to your local
computer. The following commands assume that you have a local installation of
[Git](https://git-scm.com/), [gfortran](https://gcc.gnu.org/fortran/) and
[Python 3](https://www.python.org/) with [pip](https://pypi.org/project/pip/).
To compile using Intel FORTRAN, use `make optim_ifort`.

```bash
# Fetch and compile
git clone https://github.com/thermotools/thermopack.git
cd thermopack
make optim

# Prepare and install pycThermopack, aka "thermopack"
# Remark: On some systems, Python 3 is installed as python, not python3. If so,
# you can replace "python3" with "python" and perhaps also "pip3" with "pip" in
# the below.
cd addon/pycThermopack
python3 makescript.py optim
pip3 install --user .
```

If you are working actively with the Thermopack code, either the Fortran
backend or the Python frontend, then it may be useful to install in editable
mode (aka develop mode). This will install a _link_ to the develop files
instead of copying the files when installing. This can be done with the `-e`
option, i.e.:

```bash
pip3 install -e --user .
```

See also [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
more details on pycThermopack.

### MacOS setup
The easiest way to get started is via [Homebrew](https://brew.sh). After
following the instructions on their website to set it up, install `gcc`
and `make`. Open a terminal (e.g. the default Terminal.app), and type:
```
brew install gcc make
```
Then follow the Linux instructions above, just replace `make` with `gmake`.
Some additional packages like `git` and `python` can also be installed via
Homebrew before you start, but if you use a recent version of MacOS (e.g.
Catalina), then the versions installed by default should be sufficient.

#### MacOS with arm64 architecture (M1, M2 etc.)

Use the alternative `Makefile_arm64` and `Makefile_arm64.code` by renaming the files
```
Makefile => Makefile_x86
Makefile.code => Makefile_x86.code
Makefile_arm64 => Makefile
Makefile_arm64.code => Makefile.code
```


then follow the steps under MacOS setup. Please feel free to leave an issue if you have build problems, the build system for MacOS on Apple Silicon has not been as thouroughly tested as other build systems, and is still somewhat a work in progress.

### Windows setup
Download and compile LAPACK and BLAS libraries (you will need CMake and
a working compiler).

To be compatible with the current settings, you need to compile using Intel
Fortran with Visual Studio, and configure as follows:

```
- Fortran/Data/Default Real KIND = 8
- Fortran/External Procedures/Calling Convention = cref
- Fortran/External Procedures/Name Case Interpretation = lowercase
- Fortran/External Procedures/String Length Argument Parsing = After All Arguments
- Fortran/External Procedures/Append Underscore to External Names = Yes
- Fortran/Floating Point/Floating Point Model = precise
```

Copy LAPACK and BLAS libraries to the paths:

- thermopack\lapack\lib\Debug
- thermopack\lapack\lib\Release

Open thermopack\MSVStudio\thermopack.sln using Visual Studio, and compile the wanted configuration.

See [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
how to install pycThermopack.

#### MSYS2/Mingw-W64 setup
Thermopack can also be compiled using gfortran in the MSYS2 environment. Download MSYS2 from [https://www.msys2.org](https://www.msys2.org), and install and update the package system following the instructions given. Avoid having spaces in the MSYS2 installation directory path, as the Makefile might not work. Having a working MSYS2 installation, thermopack can be compiled after installing the following packages:

```bash
pacman -S git
pacman -S mingw-w64-x86_64-gcc-fortran
pacman -S mingw-w64-x86_64-openblas
pacman -S mingw-w64-x86_64-make
pacman -S mingw-w64-x86_64-dlfcn
```

Open the `MSYS2 MinGW 64-bit` application, and enter the following in the terminal:

```bash
git clone https://github.com/thermotools/thermopack.git
cd thermopack
mingw32-make.exe optim
```

See [addon/pycThermopack/README.md](addon/pycThermopack/README.md) for
how to install pycThermopack for the MSYS2 environment.

### Docker setup
See [addon/docker/README.md](addon/docker/README.md) for
available Dockerfiles to run Thermopack with docker.

### CMake setup
See [thermopack_cmake](https://github.com/morteham/thermopack_cmake) for prototype CMake scripts to compile Thermopack.
# Getting started - Python
This is a short introduction to thermopack. Once you've gotten started, we recommend a look at the [Examples](https://github.com/thermotools/thermopack/tree/main/addon/pyExamples) in the GitHub repo. Comprehensive documentation for the methods available through the python interface can also be found in the [wiki](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class). For more advanced users, a look at the [more advanced page in the wiki](https://github.com/thermotools/thermopack/wiki/Advanced-usage#more-advanced-usage---python) may also be useful.

Equations of State (EoS's) in ThermoPack are classes. To do calculations for a given mixture an EoS object must first be initialized for that mixture, as demonstrated in the [Initializing an EoS section](#Initialising-an-equation-of-state). Then, a wide variety of thermodynamic computations can be done, as demonstrated in the remaining sections.

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

#### pxy-phase envelopes

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


# More advanced usage - Python

## Interaction parameters

In thermopack we're able to both set and get a wide array of coefficients and parameters depending on the models we are utilizing. 

### Cubic equations of state
#### Setting and getting the attractive energy interaction parameter $k_{ij}$ and co-volume interaction parameter $l_{ij}$
Starting with the attractive energy interaction parameter (kij). The parameter can be set using the function `set_kij` after intialising the equation and state. The function requires that you first write in the number of the components and subsequently the new interaction parameter i.e. (component number 1, component number 2, new kij value). If we're curious as to what parameter the EOS is already using we can see this by using the function `get_kij` which returns the value as a float given the component numbers as input i.e. (component number 1, component number 2).
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

## The different property interfaces (TV-) (Tp-) and (TVp-)

Property calculations in ThermoPack can be done either through the [TV-interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#tv-property-interfaces), the [Tp-interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#Tp-property-interfaces) or the [TVp-interfaces](https://github.com/thermotools/thermopack/wiki/Methods-in-the-thermo-class#TVp-property-interfaces).

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
