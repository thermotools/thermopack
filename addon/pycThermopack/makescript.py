#!/usr/bin/env python
from __future__ import print_function
import os
import sys
import shutil
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("mode", type=str, help="optim or debug")

args = parser.parse_args()
mode = args.mode

thermopack = "../../bin/dynamic/libthermopack_" + mode + "_gfortran_Linux.so"
if os.path.exists(thermopack):
    shutil.copy2(thermopack, "./pyctp/libthermopack.so")
else:
    print(thermopack + " does not exist. Have you compiled thermopack?")
    exit(1)
