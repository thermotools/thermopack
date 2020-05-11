#!/usr/bin/python

# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
# Importing plot utilities
from plotutils import loadFile, getFloat


T=getFloat("../binary.dat","#Pxy")
data = loadFile("../binary.dat")

if T > 0:
    legend="T="+str(T)
else:
    legend=None

env, = plt.plot(data[:,0],data[:,2]*1e-5,color="r",label=legend)
env, = plt.plot(data[:,1],data[:,2]*1e-5,color="r")


plt.xlabel(r"$x$ ($-$)")
plt.ylabel(r"$P$ (Bar)")
plt.grid(b=True, which='major', color='k', linestyle='--')
if legend is not None:
    leg = plt.legend(loc="best",numpoints=1)
    leg.get_frame().set_linewidth(0.0)
#plt.ylim((0,5))
plt.savefig("binary.pdf")
plt.show()
