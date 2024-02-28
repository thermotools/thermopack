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
        dyn_lib_path = path.join(path.dirname(__file__), "thermopack", lib)
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
    lines.append(f"DIFFERENTIAL_RETURN_MODE = '{pf_specifics['diff_return_mode']}'\n\n")
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

def write_setup_file(version):
    setup_contents = {'name': "'thermopack'",
                      'version': f"'{version}'",
                      'description': "'Python interface to thermopack'",
                      'long_description': "'readme'",
                      'long_description_content_type': "'text/markdown'",
                      'author': "'Morten Hammer'",
                      'author_email': "'morten.hammer@sintef.no'",
                      'url': "'https://github.com/thermotools/thermopack'",
                      'packages': "['thermopack']",
                      'package_data': "{'thermopack':['*thermopack.*']}",
                      'install_requires' : "['numpy~=1.0']"}

    with open(os.path.dirname(__file__) + '/setup.py', 'w') as file:
        file.write(f"# This file was automatically generated using the function 'write_setup_file' in \n"
                   f"# {__file__} \n"
                   f"# Likely called from {os.path.dirname(__file__)}/makescript.py\n"
                   f"# Timestamp : {datetime.today().isoformat()}\n\n")
        file.write("from distutils.core import setup\n"
                    "from pathlib import Path\n\n"
                    "root_dir = Path(__file__).parent # thermopack root directory\n"
                    "readme = (root_dir / 'README_pypi.md').read_text()\n\n")
        file.write('setup(')

        for i, (k, v) in enumerate(setup_contents.items()):
            if i > 0:
                file.write(',')
            file.write(f"{k}={v}\n\t")

        file.write(')\n')

def write_toml_file(version):
    contents = """[build-system]
requires = ["setuptools>=39.0"]
build-backend = "setuptools.build_meta"

[project]
name = "thermopack"
version = \"""" + version + """\"
authors = [
  { name = "Morten Hammer", email="morten.hammer@ntnu.no" },
]
maintainers = [
  { name = "Morten Hammer", email="morten.hammer@ntnu.no" },
  { name = "Vegard Gjeldvik Jervell", email="vegard.g.jervell@ntnu.no" },
]
description = "Python interface to thermopack"
readme = "README_pypi.md"
requires-python = ">=3.6"
classifiers = [
    "Programming Language :: Python :: 3",
    "Programming Language :: Fortran",
    "Operating System :: MacOS",
    "Operating System :: POSIX :: Linux",
    "Operating System :: Microsoft :: Windows",
    "License :: OSI Approved :: MIT License",
]
keywords = ["physics", "thermodynamics", "equations_of_state", "phase_equilibria", "SAFT"]

[project.urls]
"Homepage" = "https://github.com/thermotools/thermopack"
"Bug Tracker" = "https://github.com/thermotools/thermopack/issues"

[dependencies]
numpy = "^1.1"
"""
    with open(f'{os.path.dirname(__file__)}/pyproject.toml', 'w') as file:
        file.write(contents)


if __name__ == "__main__":
    pf_specifics_path = os.path.join(os.path.dirname(
        __file__), "thermopack", "platform_specifics.py")
    pf_specifics = get_platform_specifics_by_trial_and_error()
    write_platform_specifics_file(pf_specifics, pf_specifics_path)
