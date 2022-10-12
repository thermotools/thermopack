"""Simple script to copy the desired libthermopack.so/libthermopack.dynlib file."""
#!/usr/bin/env python

import argparse
import os
import shutil
import sys
from pathlib import Path
import map_platform_specifics

if __name__ == "__main__":
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

    pf_specifics_path = os.path.join(os.path.dirname(
        __file__), "pyctp", "platform_specifics.py")
    pf_specifics = map_platform_specifics.get_platform_specifics_by_trial_and_error()
    map_platform_specifics.write_platform_specifics_file(
        pf_specifics, pf_specifics_path)
