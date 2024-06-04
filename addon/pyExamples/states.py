#!/usr/bin/python3
import matplotlib.pyplot as plt
import numpy as np
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
from thermopack.saftvrqmie import saftvrqmie
from thermopack.cubic import cubic
from thermopack.thermopack_state import State, PhaseDiagram
import numpy as np
import matplotlib.pyplot as plt

# Instantiate and init SAFT-VRQ Mie object
qSAFT = saftvrqmie("H2,Ne", minimum_temperature=2.0)

dia = PhaseDiagram.binary_isotherm_vle(qSAFT, T=35.0, maximum_pressure=0.5e7)
plt.plot(1 / dia.liquid.specific_volumes * 1e-3, dia.liquid.pressures * 1e-6)
plt.plot(1 / dia.vapour.specific_volumes * 1e-3, dia.vapour.pressures * 1e-6)
plt.xlabel(r"$\rho$ (L/mol)")
plt.ylabel(r"$P$ (MPa)")
plt.title("Plotting using state wrappers")

# Txy diagram
srk = cubic("BENZENE,TOLU", "SRK")
dia = PhaseDiagram.binary_isobar_vle(srk, 1.01325e5, minimum_temperature=200.0)
plt.figure()
plt.plot(dia.liquid.molefracs[:, 0], dia.temperatures, color="b",
         label="$P$=1 Atm", ls= "-")
plt.plot(dia.vapour.molefracs[:, 0], dia.temperatures, color="b",
         ls= "-")
plt.xlabel(r"$x_{\rm{Benzene}}/y_{\rm{Benzene}}$")
plt.ylabel(r"$T (K)$")
plt.title(r"Benzene - Toluene at P=1 atm")
plt.show()
