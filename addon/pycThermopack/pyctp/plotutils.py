# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing math
import math
#
import os

def file_exists(filename):
    exists = os.path.isfile(filename)
    return exists

def loadFile(filename):
    file = open(filename, 'r')
    # Parse header
    nlines = 0
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            nlines = nlines + 1
        else:
            break
    data = np.loadtxt(filename, skiprows=nlines)
    return data

def getBinary(filename,pointLabel):
    file = open(filename, 'r')
    # Parse header
    comp1 = ""
    comp2 = ""
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            if pointLabel in line:
                comp1 = words[len(words)-2]
                comp2 = words[len(words)-1]
        else:
            break
    return comp1, comp2

def getFloat(filename,pointLabel):
    file = open(filename, 'r')
    # Parse header
    T = 0.0
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            if words[0] == pointLabel:
                T = float(words[len(words)-1])
        else:
            break
    return T

def getInteger(filename,pointLabel):
    file = open(filename, 'r')
    # Parse header
    n = 0
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            if pointLabel in line:
                n = int(words[len(words)-1])
        else:
            break
    return n

def getPoint(filename,pointLabel):
    file = open(filename, 'r')
    # Parse header
    T = 0.0
    P = 0.0
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            if pointLabel in line:
                T = float(words[len(words)-2])
                P = float(words[len(words)-1])
        else:
            break
    return T, P

def getNaNindices(data):
    n = np.shape(data)[0]
    m = np.shape(data)[1]
    nans = [0]*m
    for i in range(m):
        for j in range(n):
            if math.isnan(data[j,i]):
                nans[i] = j - 1
                break
            else:
                nans[i] = j
    return nans

def get_solid_envelope_data(filename):
    lines = []
    crits = []
    if file_exists(filename):
        data = loadFile(filename)
        Tc1, Pc1 = getPoint(filename, "#Critical point 1")
        if Tc1 > 1.0:
            crits.append([Tc1, Pc1])
        Tc2, Pc2 = getPoint(filename, "#Critical point 2")
        if Tc2 > 1.0:
            crits.append([Tc2, Pc2])
        i_nans = getNaNindices(data)
        m = np.shape(data)[1]
        if m == 12:
            k = 2
        elif m == 30:
            k = 5
        for i in range(6):
            istart = i*k
            i_nan = i_nans[istart]
            if i_nan < 0:
                continue
            line = np.zeros((i_nan, k))
            for j in range(k):
                line[:,j] = data[0:i_nan,j+istart]
            lines.append(line)
    return lines, crits

def get_globa_binary_data(filename):
    VLE = []
    VLLE = []
    CRIT = []
    AZ = []
    KSTYPE = 0
    if file_exists(filename):
        data = loadFile(filename)
        KSTYPE = getInteger(filename, "#Global phase diagram of type:")
        nCrit = getInteger(filename, "#Number of critical lines:")
        nLLVE = getInteger(filename, "#Number of LLVE lines:")
        nAZ = getInteger(filename, "#Number of AZ lines:")
        nVLE = 2
        i_nans = getNaNindices(data)
        for i in range(nVLE):
            istart = i*4
            i_nan = i_nans[istart]
            if i_nan < 0:
                continue
            line = np.zeros((i_nan, 4))
            for j in range(4):
                line[:,j] = data[0:i_nan,j+istart]
            VLE.append(line)
        for i in range(nCrit):
            istart = nVLE*4 + i*4
            i_nan = i_nans[istart]
            if i_nan < 0:
                continue
            line = np.zeros((i_nan, 4))
            for j in range(4):
                line[:,j] = data[0:i_nan,j+istart]
            CRIT.append(line)
        for i in range(nLLVE):
            istart = (nVLE+nCrit)*4 + i*8
            i_nan = i_nans[istart]
            if i_nan < 0:
                continue
            line = np.zeros((i_nan, 8))
            for j in range(8):
                line[:,j] = data[0:i_nan,j+istart]
            VLLE.append(line)
        for i in range(nAZ):
            istart = (nVLE+nCrit+nLLVE*2)*4 + i*5
            i_nan = i_nans[istart]
            if i_nan < 0:
                continue
            line = np.zeros((i_nan, 8))
            for j in range(5):
                line[:,j] = data[0:i_nan,j+istart]
            AZ.append(line)
    return KSTYPE, VLE, VLLE, CRIT, AZ
