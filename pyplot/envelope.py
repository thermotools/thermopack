#!/usr/bin/python

# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
# Importing plot utilities
from plotutils import loadFile, getPoint

data = loadFile("../envelope.dat")
Tc, Pc = getPoint("../envelope.dat","#Critical")
Tcb, Pcb = getPoint("../envelope.dat","#Cricondenbar")
Tct, Pct = getPoint("../envelope.dat","#Cricondentherm")

env, = plt.plot(data[:,0]-273.15,data[:,1],label="Envelope")
if Tc > 1.0:
    crit, = plt.plot(Tc-273.15,Pc,'ko',label="Crit")
if Tcb > 1.0:
    criconBar, = plt.plot(Tcb-273.15,Pcb,'ro',label="Cricondenbar")
if Tct > 1.0:
    criconTherm, = plt.plot(Tct-273.15,Pct,'go',label="Cricondentherm")


plt.xlabel(u"$T$ (\N{DEGREE SIGN}C)")
plt.ylabel(r"$P$ (bar)")
plt.grid(b=True, which='major', color='k', linestyle='--')
leg = plt.legend(loc="lower right",numpoints=1)
leg.get_frame().set_linewidth(0.0)
#plt.xlim((-50,40))
plt.savefig("envelope.pdf")
plt.show()
