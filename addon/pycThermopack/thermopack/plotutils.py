# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing math
import math
#
import os

def file_exists(filename):
    """Test if file exist

    Args:
        filename (str): File path

    Returns:
        bool: Does file with filename exist?
    """
    exists = os.path.isfile(filename)
    return exists

def loadFile(filename):
    """Load data from file

    Args:
        filename (str): File path

    Returns:
        ndarray: Data stored in file
    """
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

def getBinary(filename, pointLabel):
    """Get last two words on line identefied by pointLabel.

    Args:
        filename (str): File path
        pointLabel (str): Searchable string

    Returns:
        str: Last two entries on line containg pointLabel
    """
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

def getFloat(filename, pointLabel):
    """Get float from line identefied by pointLabel. Last entry on line covetered to float.

    Args:
        filename (str): File path
        pointLabel (str): Searchable string

    Returns:
        float: Last entry on line containg pointLabel
    """
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

def getInteger(filename, pointLabel):
    """Get integer from line identefied by pointLabel. Last word on line covetered to int.

    Args:
        filename (str): File path
        pointLabel (str): Searchable string

    Returns:
        int: Last entry on line containg pointLabel
    """
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

def getPoint(filename, pointLabel):
    """Get point (T,P) from line identefied by pointLabel. Last two words on line covetered to floats.

    Args:
        filename (str): File path
        pointLabel (str): Searchable string

    Returns:
        float: Last two entries on line containg pointLabel
    """
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
    """Get location of NaNs in data arrays

    Args:
        data (ndarray): Data containing NaNs

    Returns:
        array_like: Column indices giving first apperance of NaN
    """
    n = np.shape(data)[0]
    m = np.shape(data)[1]
    nans = [n]*m
    for i in range(m):
        for j in range(n):
            if math.isnan(data[j,i]):
                nans[i] = j
                break
    return nans

def get_solid_envelope_data(filename):
    """Read data from solid envelope data.

    Args:
        filename (str): File path

    Returns:
        lists: Lists of phase lines and critical points
    """
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
    """[summary]

    Args:
        filename (str): File path

    Returns:
        [type]: [description]
    """
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
