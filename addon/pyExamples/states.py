#!/usr/bin/python3
import matplotlib.pyplot as plt
import numpy as np
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
from pyctp.saftvrqmie import saftvrqmie
from pyctp.thermopack_state import State, PhaseDiagram
import numpy as np
import matplotlib.pyplot as plt

# Instanciate and init SAFT-VRQ Mie object
qSAFT = saftvrqmie("H2,Ne", minimum_temperature=2.0)

dia = PhaseDiagram.binary_isotherm_vle(qSAFT, T=35.0, maximum_pressure=0.5e7)
plt.plot(1/dia.liquid.specific_volumes*1e-3,dia.liquid.pressures*1e-6)
plt.plot(1/dia.vapour.specific_volumes*1e-3,dia.vapour.pressures*1e-6)
plt.xlabel(r"$\rho$ (L/mol)")
plt.ylabel(r"$P$ (MPa)")
plt.title("Plotting using state wrappers")
plt.show()
