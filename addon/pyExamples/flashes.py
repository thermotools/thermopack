#!/usr/bin/python
# Support for python2
from __future__ import print_function
#Modify system path
import sys
sys.path.append('../pycThermopack/')
# Importing pyThermopack
from pyctp import tcPR
# Importing Numpy (math, arrays, etc...)
import numpy as np
# Importing Matplotlib (plotting)
import matplotlib.pyplot as plt

def phase_name(th, ph):
    phase = ""
    if ph == th.VAPPH:
        phase = "Vapor"
    elif ph == th.LIQPH:
        phase = "Liquid"
    elif ph == th.SINGLEPH:
        phase = "Single"
    elif ph == th.TWOPH:
        phase = "Vapor-Liquid"
    return phase

# Instanciate and init tcPR object.

tc_pr = tcPR.tcPR()
tc_pr.init("NH3,N2,H2")

z = np.array([0.85, 0.1, 0.05])
T = 273.15 + 115.0 # K
P = 2.0e7 # Pa
x, y, beta_g, beta_l, phase = tc_pr.two_phase_tpflash(T, P, z)
print("Gas molar fraction: {} (-)".format(beta_g))

s_l, = tc_pr.entropy(T, P, x, tc_pr.LIQPH)
s_g, = tc_pr.entropy(T, P, y, tc_pr.VAPPH)
s = s_g*beta_g + s_l*beta_l

h_l, = tc_pr.enthalpy(T, P, x, tc_pr.LIQPH)
h_g, = tc_pr.enthalpy(T, P, y, tc_pr.VAPPH)
h = h_g*beta_g + h_l*beta_l

v_l, = tc_pr.specific_volume(T, P, x, tc_pr.LIQPH)
v_g, = tc_pr.specific_volume(T, P, y, tc_pr.VAPPH)
v = v_g*beta_g + v_l*beta_l

temp, x, y, beta_g, beta_l, phase = tc_pr.two_phase_psflash(P, z, s, temp=None)
print(u"Temperature from entropy-pressure flash: {} (\N{DEGREE SIGN}C)".format(temp - 273.15))

temp, x, y, beta_g, beta_l, phase = tc_pr.two_phase_phflash(P, z, h, temp=None)
print(u"Temperature from enthalpy-pressure flash: {} (\N{DEGREE SIGN}C)".format(temp - 273.15))

e = h - P*v
p = P*0.75
T = T*2.0
temp, press, x, y, betaV, betaL, phase = tc_pr.two_phase_uvflash(z, e, v, temp=T, press=P)
print(u"Temperature from energy-volume flash: {} (\N{DEGREE SIGN}C)".format(temp - 273.15))

# Supercritical temperature:
T = 273.15 + 500.0 # K
x, y, beta_g, beta_l, phase = tc_pr.two_phase_tpflash(T, P, z)
print("Supercritical phase: {}".format(phase_name(tc_pr, phase)))
phase = tc_pr.guess_phase(T, P, z)
print("Guessing supercritical phase: {}".format(phase_name(tc_pr, phase)))
