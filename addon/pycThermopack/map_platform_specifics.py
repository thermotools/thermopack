# Module for detecting import settings
import sys
from ctypes import cdll
from os import path
import itertools
# Import sysconfig to detect mingw environment
import sysconfig
from datetime import datetime
import os
import argparse
import warnings

# GNU FORTRAN
G_PREFIX = "__"
G_MODULE = "_MOD_"
G_POSTFIX = ""
G_POSTFIX_NM = "_"
# INTEL FORTRAN (x64)
I_PREFIX = ""
I_MODULE = "_mp_"
I_POSTFIX = "_"
I_POSTFIX_NM = "_"

VERSION_2 = '2.2.4b0'
VERSION_3 = '3.b0'
pf_specifics_path = os.path.join(os.path.dirname( __file__), "thermopack", "platform_specifics.py")

class bcolor:
    green = '\033[92m'
    blue = '\033[34m'
    yellow = '\033[93m'
    red = '\033[31m'
    default = '\033[0m'
    

def get_platform_specifics_from_platform():
    """Get platform specific stuff."""

    # Setting GNU FORTRAN as default
    platform_specifics = dict()
    platform_specifics["os_id"] = ""
    platform_specifics["prefix"] = G_PREFIX
    platform_specifics["module"] = G_MODULE
    platform_specifics["postfix"] = G_POSTFIX
    platform_specifics["postfix_no_module"] = G_POSTFIX_NM
    platform_specifics["dyn_lib"] = ""

    if sys.platform in ("linux", "linux2"):
        platform_specifics["os_id"] = "linux"
        platform_specifics["dyn_lib"] = "libthermopack.so"
    elif sys.platform == "darwin":
        # Darwin means Mac OS X
        # Assuming GNU FORTRAN
        platform_specifics["os_id"] = "darwin"
        platform_specifics["dyn_lib"] = "libthermopack.dylib"
    elif sys.platform == "win32":
        if sysconfig.get_platform() != "mingw":
            # Assuming INTEL FORTRAN
            platform_specifics["prefix"] = I_PREFIX
            platform_specifics["module"] = I_MODULE
            platform_specifics["postfix"] = I_POSTFIX
            platform_specifics["postfix_no_module"] = I_POSTFIX_NM
        platform_specifics["os_id"] = "win"
        platform_specifics["dyn_lib"] = "thermopack.dll"
    elif sys.platform == "win64":
        if sysconfig.get_platform() != "mingw":
            # Assuming INTEL FORTRAN
            platform_specifics["prefix"] = I_PREFIX
            platform_specifics["module"] = I_MODULE
            platform_specifics["postfix"] = I_POSTFIX
            platform_specifics["postfix_no_module"] = I_POSTFIX_NM
        platform_specifics["os_id"] = "win"
        platform_specifics["dyn_lib"] = "thermopack.dll"
    return platform_specifics


def get_platform_specifics_by_trial_and_error():
    """Get platform specific stuff."""

    # Empty as default
    platform_specifics = dict()
    platform_specifics["os_id"] = ""
    platform_specifics["prefix"] = ""
    platform_specifics["module"] = ""
    platform_specifics["postfix"] = ""
    platform_specifics["postfix_no_module"] = ""
    platform_specifics["dyn_lib"] = ""

    dynlibs = ["libthermopack.so", "thermopack.dll", "libthermopack.dll", "libthermopack.dylib"]
    thermopack_dir = path.join(path.dirname(__file__), "thermopack")
    errors = []
    for lib in dynlibs:
        dyn_lib_path = path.join(thermopack_dir, lib)
        try:
            tp = cdll.LoadLibrary(dyn_lib_path)
            platform_specifics["dyn_lib"] = lib
            break
        except OSError as err:
            errors.append(err)
            pass
    else:
        thermopack_contains = os.listdir(thermopack_dir)
        load_errors = '\n\t'.join([str(err) for err in errors])
        raise FileNotFoundError(f'Could not load ThermoPack binary!\n'
                                f'Tried : {[path.join(thermopack_dir, lib) for lib in dynlibs]}.\n'
                                f'{thermopack_dir} contains files : {thermopack_contains}\n'
                                f'Errors when attempting to load :\n\t{load_errors}')
    prefixes = ["__", ""]
    moduletxt = ["_", "_MOD_", "_mp_"]
    postfixes = ["", "_"]

    module = "thermopack_var"
    method = "add_eos"
    for pre, mod, post in itertools.product(prefixes, moduletxt, postfixes):
        export_name = pre + module + mod + method + post
        try:
            getattr(tp, export_name)
            platform_specifics["prefix"] = pre
            platform_specifics["module"] = mod
            platform_specifics["postfix"] = post
            break
        except AttributeError:
            continue
    else:
        raise AttributeError("Could not identify Fortran module name-mangling scheme in thermopack dynamic library!")

    method = "thermopack_getkij"
    for post in postfixes:
        export_name = method + post
        try:
            getattr(tp, export_name)
            platform_specifics["postfix_no_module"] = post
            break
        except AttributeError:
            continue
    else:
        raise AttributeError("Could not identify Fortran global name-mangling scheme in thermopack dynamic library!")

    if sys.platform in ("linux", "linux2"):
        platform_specifics["os_id"] = "linux"
    elif sys.platform == "darwin":
        platform_specifics["os_id"] = "darwin" # Darwin means Mac OS X
    elif sys.platform in ("win32", "win64"):
        platform_specifics["os_id"] = "win"
    else:
        warnings.warn(f"Unexpected platform : {sys.platform}", Warning)

    return platform_specifics


def write_platform_specifics_file(pf_specifics, filename):
    """Write file for platform specifics"""
    lines = list()
    lines.append(
        "# Module for platform specific stuff. Automatically generated.")
    lines.append("# Timestamp : " + str(datetime.today().isoformat()) + "\n\n")
    lines.append('import os')
    lines.append(f"DIFFERENTIAL_RETURN_MODE = '{pf_specifics['diff_return_mode']}'\n\n")
    tab = " "*4
    lines.append("def get_platform_specifics():")
    lines.append(tab + "pf_specifics = {}")

    for k, v in pf_specifics.items():
        lines.append(tab + 'pf_specifics["'+k+'"] = "'+v+'"')

    lines.append('''
    files = os.listdir(os.path.dirname(__file__))
    if not (pf_specifics['dyn_lib'] in files):
        if f'{pf_specifics["dyn_lib"]}.icloud' in files:
            pf_specifics['dyn_lib'] = f'{pf_specifics["dyn_lib"]}.icloud'
        else:
            raise FileNotFoundError(f'ThermoPack binary {pf_specifics["dyn_lib"]} not found in directory {os.path.dirname(__file__)}')
''')
    lines.append(tab + "return pf_specifics")

    with open(filename, "w") as f:
        for line in lines:
            f.write(line)
            f.write("\n")

def set_toml_version(version):
    contents = ''
    with open(f'{os.path.dirname(__file__)}/pyproject.toml', 'r') as ifile:
        line = ifile.readline()
        while line:
            if line == 'version = "0.0.0"\n':
                line = f'version = "{version}"\n'
            contents += line
            line = ifile.readline()
    with open(f'{os.path.dirname(__file__)}/pyproject.toml', 'w') as ofile:
        ofile.write(contents)

def get_platform_specifics_windows_ifort_whl():
    pf_specifics = dict()
    pf_specifics["os_id"] = "win"
    pf_specifics["prefix"] = ""
    pf_specifics["module"] = "_mp_"
    pf_specifics["postfix"] = "_"
    pf_specifics["postfix_no_module"] = "_"
    pf_specifics["dyn_lib"] = "libthermopack.dll"

    thermopack_dir = path.join(path.dirname(__file__), "thermopack")
    dyn_lib_path = path.join(thermopack_dir, pf_specifics['dyn_lib'])
    try:
        cdll.LoadLibrary(dyn_lib_path)
    except OSError as err:
        print('map_platform_specifics : could not load thermopack dynamic library ... exiting ...')
        raise err

    return pf_specifics

def warn_diff_version(diffs):
    if diffs == 'v2':
        warnings.warn(f'{bcolor.yellow}\nYou are building ThermoPack to use the deprecated return pattern using tuples.\n'
                      'Future versions of ThermoPack will return differentials using the `Differential` struct found in utils.py. '
                      'To build ThermoPack to use the new return pattern, run \n\n\t`python map_platform_specifics.py [--diffs=v3 --ifort=False]`\n'
                      'For more information see PR#102 at https://github.com/thermotools/thermopack/pull/102\n\n'
                      f'For more information on configuration options run \n\n\t`python map_platform_specifics.py --help\n\n{bcolor.default}', Warning)
    else:
        print(f'{bcolor.blue}You are building ThermoPack using the "new" return pattern (i.e. the Differential structs found '
                "in utils.py.) \nTHIS IS THE RECOMMENDED BUILD but I'm warning you because it is not backwards compatible.\n"
                "The old return pattern will probably be discontinued in the future. To build "
                'ThermoPack with the "old" return pattern (using tuples) run \n\n\t`python map_platform_specifics.py --diffs=v2 [--ifort=False]`\n\n'
                'For information on how to adapt old code to the new return pattern, see '
                'PR#102 at https://github.com/thermotools/thermopack/pull/102\n\n'
                f'For more information on configuration options run \n\n\t`python map_platform_specifics.py --help\n\n{bcolor.default}')

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--diffs', default='v3', help="Old (v2) or new (v3) return mode for differentials (Default: v3)")
    parser.add_argument('--ifort', default=False, help='Set to True if thermopack has been compiled with intel-fortran (Default: False)')
    args = parser.parse_args()

    pf_specifics_ = get_platform_specifics_by_trial_and_error()
    pf_specifics_['diff_return_mode'] = args.diffs
    pf_specifics_['version'] = VERSION_2 if (args.diffs == 'v2') else VERSION_3

    warn_diff_version(args.diffs)

    write_platform_specifics_file(pf_specifics_, pf_specifics_path)
    set_toml_version(pf_specifics_['version'])

    print(f'{bcolor.green}Successfully configured ThermoPack {pf_specifics_["version"]}{bcolor.default}')
