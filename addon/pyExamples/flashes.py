#!/usr/bin/python
#Modify system path
import sys
sys.path.insert(0,'../pycThermopack/')
# Importing pyThermopack
from thermopack.tcPR import tcPR
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

tc_pr = tcPR("NH3,N2,H2")

z = np.array([0.85, 0.1, 0.05])
T = 273.15 + 115.0 # K
P = 2.0e7 # Pa
flsh = tc_pr.two_phase_tpflash(T, P, z)
print('Flash Result: ')
print(flsh)
print()
print("Gas molar fraction: {} (-)".format(flsh.betaV))

s_l = tc_pr.entropy(T, P, flsh.x, tc_pr.LIQPH)
s_g = tc_pr.entropy(T, P, flsh.y, tc_pr.VAPPH)
s = s_g * flsh.betaV + s_l * flsh.betaL

h_l = tc_pr.enthalpy(T, P, flsh.x, tc_pr.LIQPH)
h_g = tc_pr.enthalpy(T, P, flsh.y, tc_pr.VAPPH)
h = h_g * flsh.betaV + h_l * flsh.betaL

v_l = tc_pr.specific_volume(T, P, flsh.x, tc_pr.LIQPH)
v_g = tc_pr.specific_volume(T, P, flsh.y, tc_pr.VAPPH)
v = v_g * flsh.betaV + v_l * flsh.betaL

ps_flsh = tc_pr.two_phase_psflash(P, z, s, temp=None)
print(u"Temperature from entropy-pressure flash: {} (\N{DEGREE SIGN}C)".format(ps_flsh.T - 273.15))

ph_flsh = tc_pr.two_phase_phflash(P, z, h, temp=None)
print(u"Temperature from enthalpy-pressure flash: {} (\N{DEGREE SIGN}C)".format(ph_flsh.T - 273.15))

e = h - P*v
p = P*0.75
T = T*2.0
uv_flsh = tc_pr.two_phase_uvflash(z, e, v, temp=T, press=P)
print(u"Temperature from energy-volume flash: {} (\N{DEGREE SIGN}C)".format(uv_flsh.T - 273.15))

# Supercritical temperature:
T = 273.15 + 500.0 # K
tp_flsh = tc_pr.two_phase_tpflash(T, P, z)
print("Supercritical phase: {}".format(phase_name(tc_pr, tp_flsh.phase)))
phase = tc_pr.guess_phase(T, P, z)
print("Guessing supercritical phase: {}".format(phase_name(tc_pr, tp_flsh.phase)))
