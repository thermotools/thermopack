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

def getEntries(filename, entry_label):
    """Get point [...] from line identefied by entry_label.

    Args:
        filename (str): File path
        entry_label (str): Searchable string

    Returns:
        list: Entries on line containg pointLabel
    """
    file = open(filename, 'r')
    # Parse header
    entries = []
    for line in file:
        words = line.split()
        if words[0][0] == '#':
            if entry_label in line:
                data = line.split(entry_label)[-1]
                entries = [float(i) for i in data.split()]
        else:
            break
    return entries

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
    triples = []
    if file_exists(filename):
        data = loadFile(filename)
        c1_data = getEntries(filename, "#Critical point 1:")
        if c1_data and c1_data[0] > 0.0:
            crits.append(c1_data)
        c2_data = getEntries(filename, "#Critical point 2:")
        if c2_data and c2_data[0] > 0.0:
            crits.append(c2_data)
        tr1_data = getEntries(filename, "#Triple point 1:")
        if tr1_data and tr1_data[0] > 0.0:
            triples.append(tr1_data)
        tr2_data = getEntries(filename, "#Triple point 2:")
        if tr2_data and tr2_data[0] > 0.0:
            triples.append(tr2_data)
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
    return lines, crits, triples

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
