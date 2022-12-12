"""Utility functions."""

import re
from subprocess import check_output
import sys
from ctypes import c_int, POINTER

def gcc_major_version_greater_than(GCC_version):
    """Returns if GCC major version number is greater than specefied version

    Args:
        GCC_version (int): Major GCC version

    Returns:
        bool: GCC version is greater than specified version
    """
    is_gt = True
    sys_arr_gcc = re.split('Clang|clang|GCC|gcc', sys.version)
    if len(sys_arr_gcc) > 1:
        out = check_output(["gcc", "-dumpfullversion", "-dumpversion"])
        out_str = out.decode("utf8").split("\n", maxsplit=1)[0]
        match = re.search(r'([0-9]+)\.[0-9]\.[0-9]', out_str)
        gcc_mv_str = match.group(1)
        try:
            gcc_mv = int(gcc_mv_str)
        except ValueError:
            print("Not able to determine GCC major version. Exiting.")
            sys.exit(1)

        is_gt = gcc_mv > GCC_version

    return is_gt

def get_contribution_flag(property_flag):
    prop_flag = property_flag.upper()
    if prop_flag in ("IR", "RI"):
        contribution_c = POINTER(c_int)(c_int(0))
    elif prop_flag == "R":
        contribution_c = POINTER(c_int)(c_int(1))
    elif prop_flag == "I":
        contribution_c = POINTER(c_int)(c_int(2))
    else:
        raise ValueError("property_flag has wrong value."\
                         " Expected I,R or IR, got " + prop_flag)
    return contribution_c
