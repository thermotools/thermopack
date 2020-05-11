#!/usr/bin/python

# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
# Importing plot utilities
from plotutils import loadFile, getInteger, getBinary



data = loadFile("../global_binary.dat")
diagram_type = getInteger("../global_binary.dat","#Global phase diagram of type:")
nCrit = getInteger("../global_binary.dat","#Number of critical lines:")
nLLVE = getInteger("../global_binary.dat","#Number of LLVE lines:")
nAZ = getInteger("../global_binary.dat","#Number of AZ lines:")
comp1, comp2 = getBinary("../global_binary.dat","#Binary system:")
title="Type " + str(diagram_type) + " binary: " + comp1 + " " + comp2
base_filename = "global_binary_" + comp1 + "_" + comp2

colors = [ "black", "blue", "red", "green", "grey", "cyan", "navy"]
linestyles = [ "-", "--", ":", "-."]
p_conv=1.0e-5 # Pa -> Bar

# Plot saturation lines
env, = plt.plot(data[:,0],data[:,1]*p_conv,color=colors[0],linestyle=linestyles[0],label="Vapour pressure")
env, = plt.plot(data[:,4],data[:,5]*p_conv,color=colors[0],linestyle=linestyles[0])

# Critical lines
label="Critical"
for i in range(nCrit):
    iT = (i+2)*4
    env, = plt.plot(data[:,iT],data[:,iT+1]*p_conv,color=colors[1],linestyle=linestyles[1],label=label)
    label=None

# LLVE lines
label="LLVE"
for i in range(nLLVE):
    iT = (2+nCrit)*4 + i*8
    env, = plt.plot(data[:,iT],data[:,iT+1]*p_conv,color=colors[2],linestyle=linestyles[2],label=label)
    label=None

# AZ lines
label="AZ"
for i in range(nAZ):
    iT = (2+nCrit)*4 + nLLVE*8 + i*5
    env, = plt.plot(data[:,iT],data[:,iT+1]*p_conv,color=colors[3],linestyle=linestyles[3],label=label)
    label=None

plt.xlabel(r"$T$ (K)")
plt.ylabel(r"$P$ (bar)")
plt.grid(b=True, which='major', color='k', linestyle='--')
leg = plt.legend(loc="best",numpoints=1)
leg.get_frame().set_linewidth(0.0)
plt.ylim((0,300))
plt.title(title)
filename=base_filename + "_PT.pdf"
plt.savefig(filename)
plt.show()
plt.clf()

# T-x diagram
# Critical lines
label="Critical"
for i in range(nCrit):
    iT = (i+2)*4
    env, = plt.plot(data[:,iT+3],data[:,iT],color=colors[1],linestyle=linestyles[1],label=label)
    label=None

# LLVE lines
label="LLVE"
for i in range(nLLVE):
    iT = (2+nCrit)*4 + i*8
    env, = plt.plot(data[:,iT+5],data[:,iT],color=colors[2],linestyle=linestyles[2],label=label)
    env, = plt.plot(data[:,iT+6],data[:,iT],color=colors[2],linestyle=linestyles[2])
    env, = plt.plot(data[:,iT+7],data[:,iT],color=colors[2],linestyle=linestyles[2])
    label=None

# AZ lines
label="AZ"
for i in range(nAZ):
    iT = (2+nCrit)*4 + nLLVE*8 + i*5
    env, = plt.plot(data[:,iT+4],data[:,iT],color=colors[3],linestyle=linestyles[3],label=label)
    label=None

plt.ylabel(r"$T$ (K)")
label="$x_{\\rm{"+comp1+"}}$ (-)"
plt.xlabel(label)
plt.grid(b=True, which='major', color='k', linestyle='--')
leg = plt.legend(loc="best",numpoints=1)
leg.get_frame().set_linewidth(0.0)
#plt.xlim((0.8,1.0))
#plt.ylim((240.0,440.0))
plt.title(title)
filename=base_filename + "_Tx.pdf"
plt.savefig(filename)
plt.show()
plt.clf()

# T-v diagram

# Plot saturation lines
env, = plt.plot(1.0/data[:,2],data[:,0],color=colors[0],linestyle=linestyles[0],label="Pure")
env, = plt.plot(1.0/data[:,3],data[:,0],color=colors[0],linestyle=linestyles[0])
env, = plt.plot(1.0/data[:,6],data[:,4],color=colors[0],linestyle=linestyles[0])
env, = plt.plot(1.0/data[:,7],data[:,4],color=colors[0],linestyle=linestyles[0])


# Critical lines
label="Critical"
for i in range(nCrit):
    iT = (i+2)*4
    env, = plt.plot(1.0/data[:,iT+2],data[:,iT],color=colors[1],linestyle=linestyles[1],label=label)
    label=None

# LLVE lines
label="LLVE"
for i in range(nLLVE):
    iT = (2+nCrit)*4 + i*8
    env, = plt.plot(1.0/data[:,iT+2],data[:,iT],color=colors[2],linestyle=linestyles[2],label=label)
    env, = plt.plot(1.0/data[:,iT+3],data[:,iT],color=colors[2],linestyle=linestyles[2])
    env, = plt.plot(1.0/data[:,iT+4],data[:,iT],color=colors[2],linestyle=linestyles[2])
    label=None

# AZ lines
label="AZ"
for i in range(nAZ):
    iT = (2+nCrit)*4 + nLLVE*8 + i*5
    env, = plt.plot(1.0/data[:,iT+2],data[:,iT],color=colors[3],linestyle=linestyles[3],label=label)
    env, = plt.plot(1.0/data[:,iT+3],data[:,iT],color=colors[3],linestyle=linestyles[3])
    label=None

plt.ylabel(r"$T$ (K)")
label=r"$x_{\\rm{"+comp1+"}}$ (-)"
plt.xlabel(r"$\rho$ (mol/m$^3$)")
plt.grid(b=True, which='major', color='k', linestyle='--')
leg = plt.legend(loc="best",numpoints=1)
leg.get_frame().set_linewidth(0.0)
#plt.xlim((0.8,1.0))
#plt.ylim((240.0,440.0))
plt.title(title)
filename=base_filename + "_Trho.pdf"
plt.savefig(filename)
plt.show()
plt.clf()
