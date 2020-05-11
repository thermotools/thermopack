#!/usr/bin/python

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
