#!/usr/bin/python

# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt
# Importing plot utilities
from plotutils import loadFile, getFloat, file_exists

T = None

if file_exists("../binary_LLE.dat"):
    T_LLE=getFloat("../binary_LLE.dat","#Pxy")
    if T_LLE is not None:
        T = T_LLE
    data = loadFile("../binary_LLE.dat")
    env, = plt.plot(data[:,0],data[:,2]*1e-5,color="b")
    env, = plt.plot(data[:,1],data[:,2]*1e-5,color="b")

if file_exists("../binary_L1VE.dat"):
    T_L1VE=getFloat("../binary_L1VE.dat","#Pxy")
    if T_L1VE is not None:
        T = T_L1VE
    data = loadFile("../binary_L1VE.dat")
    env, = plt.plot(data[:,0],data[:,2]*1e-5,color="g")
    env, = plt.plot(data[:,1],data[:,2]*1e-5,color="g")

if file_exists("../binary_L2VE.dat"):
    T_L2VE=getFloat("../binary_L2VE.dat","#Pxy")
    if T_L2VE is not None:
        T = T_L2VE
    data = loadFile("../binary_L2VE.dat")
    env, = plt.plot(data[:,0],data[:,2]*1e-5,color="r")
    env, = plt.plot(data[:,1],data[:,2]*1e-5,color="r")

if T > 0:
    plt.title("T="+str(T))
    plt.xlabel(r"$x$ ($-$)")
    plt.ylabel(r"$P$ (Bar)")
    plt.grid(b=True, which='major', color='k', linestyle='--')
    plt.savefig("binary_VLLE.pdf")
    plt.show()
