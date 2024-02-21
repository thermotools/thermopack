#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.cubic import cubic
# Importing Numpy (math, arrays, etc...)
import numpy as np

# Instantiate PR object for a methane-ethane mixture
eos = cubic("C1,C2", "PR", mixing="vdW")
cindices = range(1,eos.nc+1)
Tclist = np.array([eos.critical_temperature(i) for i in cindices])
Pclist = np.array([eos.critical_pressure(i) for i in cindices])
acflist = np.array([eos.acentric_factor(i) for i in cindices])
Mwlist = np.array([eos.compmoleweight(i) * 1e-3 for i in cindices]) # kg/mol
kijmat = [[eos.get_kij(i, j) for i in cindices] for j in cindices]
p1, dp1dt, dp1dv = eos.pressure_tv(temp=150, volume=1e-2, n=[1.0, 2.0], dpdt=True, dpdv=True)
print('pressure : ', p1)
#print(dp1.dT)

# Recreate the same mixture using pseudo functionality. Two initializations
# required: The first one tells thermopack which components are
# pseudocomponents. The second initializes the pseudocomponents using Tc, Pc,
# acf, Mw. The real components are left untouched. NB: Ideal gas heat capacities
# are not implemented for pseudocomponents, so only residual caloric properties
# are available.
eos = cubic("C1,PSEUDO", "PR", mixing="HV")
eos.init_pseudo(comps="C1,ArbitraryName", Tclist=Tclist, Pclist=Pclist, acflist=acflist, Mwlist=Mwlist)
_ = [[eos.set_kij(i, j, kijmat[i - 1][j - 1]) for i in cindices] for j in cindices]
p2, dp2dt, dp2dv = eos.pressure_tv(temp=150, volume=1e-2, n=[1.0, 2.0], dpdt=True, dpdv=True)
print('pressure : ', p2)
#print(dp2.dT)
