"""Simple script to copy the desired libthermopack.so/libthermopack.dynlib file."""
#!/usr/bin/env python

import argparse
import os
import shutil
import sys
import warnings
from pathlib import Path
import map_platform_specifics

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", type=str, help="optim or debug")
    parser.add_argument("-diffs", default="v3", type=str, help="Old (v2.1) or new (not v2.1) return mode for differentials", )

    args = parser.parse_args()
    mode = args.mode

    # Glob to handle different OS suffixes
    libthermo = list(
        Path('../../bin/dynamic').glob(f'libthermopack_{mode}_gfortran*.*'))[0]

    if not os.path.exists(libthermo):
        print(f'{libthermo}does not exist. Have you compiled thermopack?')
        sys.exit(1)

    if os.path.exists("./thermopack/libthermopack"+libthermo.suffix):
        os.remove("./thermopack/libthermopack"+libthermo.suffix)

    shutil.copy2(libthermo, "./thermopack/libthermopack"+libthermo.suffix)

    pf_specifics_path = os.path.join(os.path.dirname(
        __file__), "thermopack", "platform_specifics.py")
    pf_specifics = map_platform_specifics.get_platform_specifics_by_trial_and_error()

    if args.diffs == 'v2.1':
        warnings.warn('You are building ThermoPack to use the deprecated return pattern using tuples. Future versions\n'
                      'of ThermoPack will return differentials using the `Differential` struct found in utils.py.\n'
                      'To build ThermoPack to use the new return pattern, run `python makescript.py [optim/debug]`\n\n', DeprecationWarning)
    else:
        warnings.warn('You are building ThermoPack using the "new" return pattern (i.e. the Differential structs found\n'
                      "in utils.py.) THIS IS THE RECOMMENDED BUILD but I'm warning you because it is not backwards compatible.\n"
                      "The old return pattern will probably be discontinued in the future. To build\n"
                      'ThermoPack with the "old" return pattern (using tuples) run `python makescript.py [optim/debug] -diffs=v2.1`\n'
                      'For information on how to adapt old code to the new return pattern, see: XXX\n', FutureWarning)
    pf_specifics['diff_return_mode'] = args.diffs
    map_platform_specifics.write_platform_specifics_file(
        pf_specifics, pf_specifics_path)
