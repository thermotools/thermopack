#!/usr/bin/python

# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
# Importing plot utilities
from plotutils import loadFile, getPoint

data = loadFile("../solid_envelope.dat")
Tc1, Pc1 = getPoint("../solid_envelope.dat","#Critical point 1")
Tc2, Pc2 = getPoint("../solid_envelope.dat","#Critical point 2")


colors = [ "black", "blue", "red", "green", "grey", "cyan", "navy"]
linestyles = [ "-", "--", ":", "-."]
p_conv=1.0e-5 # Pa -> Bar

for i in range(int(data.shape[1]/2)):
    iT = 2*i
    label=None
    env, = plt.plot(data[:,iT],data[:,iT+1]*p_conv,color=colors[i],linestyle=linestyles[0],label=label)


if Tc1 > 1.0:
    crit, = plt.plot(Tc1,Pc1*p_conv,'ko')
if Tc2 > 1.0:
    crit, = plt.plot(Tc2,Pc2*p_conv,'ko')

plt.xlabel(r"$T$ (K)")
plt.ylabel(r"$P$ (bar)")
plt.grid(b=True, which='major', color='k', linestyle='--')
leg = plt.legend(loc="best",numpoints=1)
leg.get_frame().set_linewidth(0.0)
filename="solid_envelope_PT.pdf"
plt.savefig(filename)
plt.show()
plt.clf()
