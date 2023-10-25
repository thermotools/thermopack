---
version: 2.1.0
layout: default
title: More advanced usage
permalink: /v2.1.0/more_advanced.html
---

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
