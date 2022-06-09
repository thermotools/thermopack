# Module for detecting import settings
import sys
from ctypes import cdll
from os import path
import itertools
# Import sysconfig to detect mingw environment
import sysconfig
from datetime import datetime
import os

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


def get_platform_specifics_from_platform():
    """Get platform specific stuff."""

    # Setting GNU FORTRAN as default
    platform_specifics = {}
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
        platform_specifics["dyn_lib"] = "libthermopack.dynlib"
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
    platform_specifics = {}
    platform_specifics["os_id"] = ""
    platform_specifics["prefix"] = ""
    platform_specifics["module"] = ""
    platform_specifics["postfix"] = ""
    platform_specifics["postfix_no_module"] = ""
    platform_specifics["dyn_lib"] = ""

    dynlibs = ["libthermopack.so", "thermopack.dll", "libthermopack.dynlib"]
    for lib in dynlibs:
        dyn_lib_path = path.join(path.dirname(__file__), "pyctp", lib)
        try:
            tp = cdll.LoadLibrary(dyn_lib_path)
        except OSError:
            tp = None
        #print(dyn_lib_path, tp)
        if tp is not None:
            platform_specifics["dyn_lib"] = lib
            break

    prefixes = ["__", ""]
    moduletxt = ["_", "_MOD_", "_mp_"]
    postfixes = ["", "_"]

    module = "thermopack_var"
    method = "add_eos"
    for pre, mod, post in itertools.product(prefixes, moduletxt, postfixes):
        export_name = pre + module + mod + method + post
        try:
            attr = getattr(tp, export_name)
        except AttributeError:
            attr = None
        #print(export_name, attr)
        if attr is not None:
            platform_specifics["prefix"] = pre
            platform_specifics["module"] = mod
            platform_specifics["postfix"] = post
            break

    method = "thermopack_getkij"
    for post in postfixes:
        export_name = method + post
        try:
            attr = getattr(tp, export_name)
        except AttributeError:
            attr = None
        #print(export_name, attr)
        if attr is not None:
            platform_specifics["postfix_no_module"] = post
            break

    if sys.platform in ("linux", "linux2"):
        platform_specifics["os_id"] = "linux"
    elif sys.platform == "darwin":
        # Darwin means Mac OS X
        platform_specifics["os_id"] = "darwin"
    elif sys.platform == "win32" or sys.platform == "win64":
        platform_specifics["os_id"] = "win"

    return platform_specifics


def write_platform_specifics_file(pf_specifics, filename):
    """Write file for platform specifics"""
    lines = []
    lines.append(
        "# Module for platform specific stuff. Automatically generated.")
    lines.append("# Timestamp : " +
                 str(datetime.today().isoformat()) + "\n\n")

    tab = " "*4
    lines.append("def get_platform_specifics():")
    lines.append(tab + "pf_specifics = {}")

    for k, v in pf_specifics.items():
        lines.append(tab + 'pf_specifics["'+k+'"] = "'+v+'"')

    lines.append(tab + "return pf_specifics")

    with open(filename, "w") as f:
        for line in lines:
            f.write(line)
            f.write("\n")


if __name__ == "__main__":
    pf_specifics_path = os.path.join(os.path.dirname(
        __file__), "pyctp", "platform_specifics.py")
    pf_specifics = get_platform_specifics_by_trial_and_error()
    write_platform_specifics_file(pf_specifics, pf_specifics_path)
