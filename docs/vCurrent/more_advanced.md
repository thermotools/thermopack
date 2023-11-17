---
layout: default
version: 
title: More advanced usage
permalink: /vcurrent/more_advanced.html
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