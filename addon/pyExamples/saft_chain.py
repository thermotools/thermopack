import matplotlib.pyplot as plt
import numpy as np
from thermopack.pcsaft import pcsaft

eos = pcsaft('C2')

T = 300
n = 1
p_lst = np.linspace(0.1, 500) * 1e5
v1, = eos.specific_volume(T, 0.1e5, [n], eos.VAPPH)
v2, = eos.specific_volume(T, 500e5, [n], eos.LIQPH)

rho1 = 1 / v1
rho2 = 1 / v2

rho_lst = np.linspace(rho1, rho2)
V_lst = 1 / rho_lst
a_chain = np.empty_like(p_lst)
for i, p in enumerate(p_lst):
    V, = eos.specific_volume(T, p, [n], eos.VAPPH)
    a_chain[i] = eos.a_chain(T, V, [n])[0]

plt.plot(rho_lst, a_chain)
plt.show()