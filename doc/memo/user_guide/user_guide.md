# Adding new fluids

The fluid database consists of a set of
`.json`-files in the
`fluids` directory. These files are
are used to auto-generate the FORTRAN-files
`compdatadb.f90` and
`saftvrmie_datadb.f90` by running
the respective python scripts
`compdata.py` and
`saftvrmie.py` found in the
directory `addon/pyUtils/datadb/`.
The files are generated in the current working directory and must be
copied to the `src`-directory
before recompiling ThermoPack to make the fluids available.

A `<fluid\>.json` file must
contain a minimal set of data to be valid. This includes the critical
point, accentric factor, mole weight and ideal gas heat capacity.

## Ideal gas heat capacity

Several different correlations for the heat capacity are available,
selected by the "correlation"-key in the "ideal-heat-capacity-1" field
of the fluid files. These are summarized in the table below.


```
Ideal gas heat capacity correlations, and the corresponding keys used in the fluid-database.
```
| Key | Correlation | Equation |  | Unit |
|------|--------------|-----------|--|------|
| 1 | Sherwood, Reid & Prausnitz(a) | A+BT+CT^2 +DT^3 | | cal g−^1 mol−^1 K−^1 |
| 2 | API-Project | 44 | | - |
| 3 | Hypothetic components | - | | - |
| 4 | Sherwood, Reid & Prausniz(b) | A+BT+CT^2 +DT^3 | | J mol−^1 K−^1|
| 5 | Ici (Krister Strøm) | A+BT+CT^2 +DT^3 +ET−^2 | | J g−^1 K−^1 |
| 6 | Chen, Bender (Petter Nekså) | A+BT+CT^2 +DT^3 +ET^4 | | J g−^1 K−^1
| 7 | Aiche, Daubert & Danner(c) | A+B [ (C / T) sinh(C/T) ]^2 + D[ (E / T) cosh(E / T) ]^2 | | J kmol−^1 K−^1 |
| 8 | Poling, Prausnitz & O’Connel(d)     | R ( A+BT+CT^2 +DT^3 +ET^4 ) | | J mol−^1 K−^1 |
| 9 | Linear function and fraction | A+BT+TC+D | | J mol−^1 K−^1 |
| 10 | Leachman & Valenta for H2 | - | | - |
| 11 | Use TREND model | - | | - |
| 12 | Shomate equation∗ | A+BTs+CTs^2 +DTs^3 +ETs−^2 | | J mol−^1 K−^1 |

(a)3rd ed.(c)DIPPR-database

(b)4th ed.(d)5th ed.

∗Note:Ts= 10− (^3) T
