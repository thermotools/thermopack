"""Simple script to copy the desired libthermopack.so/libthermopack.dynlib file."""
#!/usr/bin/env python

import argparse
import os
import shutil
import sys
from pathlib import Path
from datetime import datetime
from pyctp import map_platform_specifics

parser = argparse.ArgumentParser()
parser.add_argument("mode", type=str, help="optim or debug")

args = parser.parse_args()
mode = args.mode

# Glob to handle different OS suffixes
libthermo = list(
    Path('../../bin/dynamic').glob(f'libthermopack_{mode}_gfortran*.*'))[0]

if not os.path.exists(libthermo):
    print(f'{libthermo}does not exist. Have you compiled thermopack?')
    sys.exit(1)

shutil.copy2(libthermo, "./pyctp/libthermopack"+libthermo.suffix)

pf_specifics = map_platform_specifics.get_platform_specifics_from_platform()
with open('pyctp/platform_specifics.py', 'w') as file:
    tab = '    '
    file.write('# Module for platform specific stuff. Automatically generated.\n'
                +'# Timestamp : '+ str(datetime.today().isoformat())+'\n\n\n')
    file.write('def get_platform_specifics():\n'
                +tab + 'pf_specifics = {}\n')
    for k, v in pf_specifics.items():
        file.write(tab+'pf_specifics["'+k+'"] = "'+v+'"\n')
    
    file.write(tab+'return pf_specifics')

'''
def get_platform_specifics():
    pf_specifics = {}
    pf_specifics["prefix"] = "__"
    pf_specifics["module"] = "_MOD_"
    pf_specifics["postfix"] = ""
    pf_specifics["postfix_no_module"] = "_"
    pf_specifics["dyn_lib"] = "libthermopack.so"
    pf_specifics["os_id"] = "linux"
    return pf_specifics
'''