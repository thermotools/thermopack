# Module for detecting import settings
import sys
from ctypes import cdll
from os import path
import itertools
# Import sysconfig to detect mingw environment
import sysconfig

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
    os_id = ""
    prefix = ""
    module = ""
    postfix = ""
    postfix_no_module = ""
    dyn_lib = ""
    if sys.platform in ("linux", "linux2"):
        # Assuming GNU FORTRAN
        prefix = G_PREFIX
        module = G_MODULE
        postfix = G_POSTFIX
        postfix_no_module = G_POSTFIX_NM
        dyn_lib = "libthermopack.so"
        os_id = "linux"
    elif sys.platform == "darwin":
        # Darwin means Mac OS X
        # Assuming GNU FORTRAN
        prefix = G_PREFIX
        module = G_MODULE
        postfix = G_POSTFIX
        postfix_no_module = G_POSTFIX_NM
        dyn_lib = "libthermopack.dynlib"
        os_id = "darwin"
    elif sys.platform == "win32":
        if sysconfig.get_platform() == "mingw":
            prefix = G_PREFIX
            module = G_MODULE
            postfix = G_POSTFIX
            postfix_no_module = G_POSTFIX_NM
        else:
            # Assuming INTEL FORTRAN
            prefix = I_PREFIX #"_"
            module = I_MODULE
            postfix = I_POSTFIX
            postfix_no_module = I_POSTFIX_NM
        dyn_lib = "thermopack.dll"
        os_id = "win"
    elif sys.platform == "win64":
        if sysconfig.get_platform() == "mingw":
            prefix = G_PREFIX
            module = G_MODULE
            postfix = G_POSTFIX
            postfix_no_module = G_POSTFIX_NM
        else:
            # Assuming INTEL FORTRAN
            prefix = I_PREFIX
            module = I_MODULE
            postfix = I_POSTFIX
            postfix_no_module = I_POSTFIX_NM
        dyn_lib = "thermopack.dll"
        os_id = "win"
    return prefix, module, postfix, postfix_no_module, dyn_lib, os_id


def get_platform_specifics_by_trial_and_error():
    """Get platform specific stuff."""
    os_id = ""
    prefix = ""
    module = ""
    postfix = ""
    postfix_no_module = ""
    dyn_lib = ""

    dynlibs = ["libthermopack.so", "thermopack.dll", "libthermopack.dynlib"]
    for lib in dynlibs:
        dyn_lib_path = path.join(path.dirname(__file__), lib)
        try:
            tp = cdll.LoadLibrary(dyn_lib_path)
        except OSError:
            tp = None
        #print(dyn_lib_path, tp)
        if tp is not None:
            dyn_lib = lib
            break

    prefixes = ["__", ""]
    moduletxt = ["_", "_MOD_", "_mp_"]
    postfixes = ["", "_"]

    module = "thermopack_var"
    method = "add_eos"
    for pre, mod, post in itertools.product(prefixes,moduletxt,postfixes):
        export_name = pre + module + mod + method + post
        try:
            attr = getattr(tp, export_name)
        except AttributeError:
            attr = None
        #print(export_name, attr)
        if attr is not None:
            prefix = pre
            module = mod
            postfix = post
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
            postfix_no_module = post

    if sys.platform in ("linux", "linux2"):
        os_id = "linux"
    elif sys.platform == "darwin":
        # Darwin means Mac OS X
        os_id = "darwin"
    elif sys.platform == "win32" or sys.platform == "win64":
        os_id = "win"

    return prefix, module, postfix, postfix_no_module, dyn_lib, os_id
