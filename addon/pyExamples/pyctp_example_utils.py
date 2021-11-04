#!/usr/bin/python
# Support for python2
from __future__ import print_function
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

# Avogadros number
NA = 6.02214076e23
KB = 1.380650524e-23

def calc_reduced_T(Ta, eps):
    """ Calculate reduced temperature
    """
    Tstar = np.zeros_like(Ta)
    Tstar = Ta/eps
    return Tstar

def calc_reduced_rho(rhoa, sigma):
    """ Calculate reduced density
    """
    rhoStar = np.zeros_like(rhoa)
    rhoStar = sigma**3*NA*rhoa
    return rhoStar

def calc_real_T(Tstar, eps):
    """ Calculate temperature from reduced temperature
    """
    Ta = np.zeros_like(Tstar)
    Ta = Tstar*eps
    return Ta

def calc_real_rho(rhoStar, sigma):
    """ Calculate density from reduced density
    """
    rhoa = np.zeros_like(rhoStar)
    rhoa = rhoStar/(sigma**3*NA)
    return rhoa

def calc_reduced_entropy(s):
    """ Calclate reduced entropy
    """
    sStar = np.zeros_like(s)
    sStar = s/(NA*KB)
    return sStar
