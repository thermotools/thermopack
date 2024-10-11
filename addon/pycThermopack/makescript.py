"""Simple script to copy the desired libthermopack.so/libthermopack.dylib file."""
#!/usr/bin/env python

import argparse
import os
import shutil
import sys
import warnings
from pathlib import Path
from map_platform_specifics import VERSION_2, VERSION_3, pf_specifics_path
import map_platform_specifics

def windows_make(diffs):
    if diffs != 'v2':
        diffs = 'v3'

    pf_specifics = map_platform_specifics.get_platform_specifics_windows_ifort_whl()
    pf_specifics["diff_return_mode"] = diffs
    map_platform_specifics.write_platform_specifics_file(pf_specifics, pf_specifics_path)
    map_platform_specifics.set_toml_version(diffs)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", type=str, help="optim or debug")
    parser.add_argument("-diffs", default="v3", type=str, help="Old (v2) or new (> v2) return mode for differentials")
    parser.add_argument("--install", action=argparse.BooleanOptionalAction, help="Install thermopack from thermopack/bin/dynamic?")

    args = parser.parse_args()
    mode = args.mode

    if args.install:
        # Glob to handle different OS suffixes
        libthermo = list(
            Path('../../bin/dynamic').glob(f'libthermopack_{mode}_gfortran*.*'))[0]
    
        if not os.path.exists(libthermo):
            print(f'{libthermo}does not exist. Have you compiled thermopack?')
            sys.exit(1)

        if os.path.exists("./thermopack/libthermopack"+libthermo.suffix):
            os.remove("./thermopack/libthermopack"+libthermo.suffix)

        shutil.copy2(libthermo, "./thermopack/libthermopack"+libthermo.suffix)

    pf_specifics = map_platform_specifics.get_platform_specifics_by_trial_and_error()

    if args.diffs == 'v2':
        version = VERSION_2
    elif args.diffs == 'v3':
        version = VERSION_3
    else:
        warnings.warn(f'-diffs={args.diffs} is not a valid value. Valid values are -diffs=[v2/v3], treating as -diffs=v3',
                      Warning)
        version = VERSION_3
        args.diffs = 'v3'

    map_platform_specifics.warn_diff_version(args.diffs)

    pf_specifics['diff_return_mode'] = args.diffs
    map_platform_specifics.write_platform_specifics_file(pf_specifics, pf_specifics_path)
    map_platform_specifics.set_toml_version(version)

    print(f'\033[92mSuccessfully configured ThermoPack {version}\033[0m')
