---
layout: default
version: 
title: Adding new fluids
permalink: /vcurrent/new_fluids.html
---

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

| Key | Correlation                         | Equation                                                      | Unit                  |
|-----|-------------------------------------|---------------------------------------------------------------|-----------------------|
| 1   | Sherwood, Reid & Prausnitz(a)       | $A + BT + CT^2 + DT^3$                                        | $cal g^-1 mol^-1 K^-1$ |
| 2   | API-Project                         | 44                                                            | -                     |
| 3   | Hypothetic components               | -                                                             | -                     |
| 4   | Sherwood, Reid & Prausnitz(b)       | $A + BT + CT^2 + DT^3$                                        | $J mol^-1 K^-1$       |
| 5   | Ici (Krister Strøm)                | $A + BT + CT^2 + DT^3 + ET^-2$                                | $J g^-1 K^-1$         |
| 6   | Chen, Bender (Petter Nekså)         | $A + BT + CT^2 + DT^3 + ET^4$                                 | $J g^-1 K^-1$         |
| 7   | Aiche, Daubert & Danner(c)          | $A + B [ (C / T) sinh(C/T) ]^2 + D [ (E / T) cosh(E / T) ]^2$ | $J kmol^-1 K^-1$      |
| 8   | Poling, Prausnitz & O’Connel(d)     | $R ( A + BT + CT^2 + DT^3 + ET^4 )$                           | $J mol^-1 K^-1$       |
| 9   | Linear function and fraction       | $A + BT + TC + D$                                             | $J mol^-1 K^-1$       |
| 10  | Leachman & Valenta for H2           | -                                                             | -                     |
| 11  | Use TREND model                     | -                                                             | -                     |
| 12  | Shomate equation∗                   | $A + B Ts + C Ts^2 + D Ts^3 + E Ts^-2$                        | $J mol^-1 K^-1$       |


(a)3rd ed.(c)DIPPR-database

(b)4th ed.(d)5th ed.

∗Note:Ts= 10− (^3) T


## Melting and sublimation curve correlations

$T_{\rm{reducing}} (K), p_{\rm{reducing}} (Pa), \mathbf{a}, \mathbf{c}, n, n_1, n_2$ and $n_3$ are read from the `<fluid\>.json` file, while $n_4 = n-n_1- n_2-n_3$. Currently  a maximum of 6 paramaters can be given, $n \leq 6$. The correlation type is defined by a four character string with the format **XX-X**, where **ML-X** and **SL-X** are the default melting curve ($\sigma_{\rm{melt}}$) and sublimation curve ($\sigma_{\rm{sub}}$) correlations, respectively. See the `Methane.json` file for an working example of both the *melting_curve* and *sublimation_curve* parameters.

The reduced temperature used in the correlations is  defined as

$$ \tau = \frac{T}{T_{\rm{reducing}}}. $$

The last character in the correlation string defines how the reducing pressure combines with $\sigma$ to give the melting/sublimation pressure,

$$
p(\sigma) = p_{\rm{reducing}} \times
\begin{cases} 
\sigma & \text{correlation is XX-1} \\\\
\exp(\sigma)  & \text{correlation is XX-2}\\\\
\exp(\frac{\sigma}{\tau})  & \text{correlation is XX-3}
\end{cases}
$$

For the melting curve calculation $\sigma$ is calculated from

$$ \sigma_{\rm{melt}} = \sum_{i=1}^{n_1} a_i \tau^{c_i}  + \sum_{j=1}^{n_2} a_j (\tau-1)^{c_j} + \sum_{k=1}^{n_3} a_k (\ln \tau)^{c_k} + \sum_{l=1}^{n_4} a_l (\tau^{c_l} - 1)  $$

For the sublimation curve calculation $\sigma$ is calculated from

$$ \sigma_{\rm{sub}} = \sum_{i=1}^{n_1} a_i \tau^{c_i}  + \sum_{j=1}^{n_2} a_j (1-\tau)^{c_j} + \sum_{k=1}^{n_3} a_k (\ln \tau)^{c_k} + \sum_{l=1}^{n_4} a_l (\tau^{c_l} - 1)  $$

The melting/sublimation curves can be scaled to match the saturation pressure at the triple temperature, $p_{\rm{sat}}(T_{\rm{triple}})$. The scaled pressure, $\tilde{p}(\sigma)$, is then calculated as

$$ \tilde{p}(\sigma) = \frac{p_{\rm{sat}}(T_{\rm{triple}}) }{p(\sigma(T_{\rm{triple}}))} p(\sigma)$$
