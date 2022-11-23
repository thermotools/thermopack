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


class FlashResult:
    """
    Object to return the result of a flash calculation
    """
    def __init__(self, flash_tuple, flash_type, z, state_tuple, extra_dict=None):
        self.x, self.y, self.betaV, self.betaL, self.phase_idx = flash_tuple
        self.phase = 'Must be set after initialization'
        self.flash_type = flash_type
        self.state_point = state_tuple
        self.z = z
        self.extra_dict = extra_dict
        if extra_dict is not None:
            for k, v in extra_dict.items():
                self.__setattr__(k, v[1])

    def __repr__(self):
        reprstr = 'Flash result object (attribute identifiers in parentheses) \n'
        reprstr += 'Flash type : ' + self.flash_type + '\n'
        reprstr += 'State point : ' + str(self.state_point) + '\n'
        reprstr += 'Total composition (z) : ' + str(self.z) + '\n'
        reprstr += 'Phase (phase_idx) : ' + self.phase + ' (' + str(self.phase_idx) + ')\n'
        reprstr += 'Vapour fraction (betaV) : ' + str(self.betaV) + '\n'
        reprstr += 'Vapour composition (y) : ' + str(self.y) + '\n'
        reprstr += 'Liquid fraction (betaL) : ' + str(self.betaL) + '\n'
        reprstr += 'Liquid composition (x) : ' + str(self.x) + '\n'
        if self.extra_dict is not None:
            for k, v in self.extra_dict.items():
                reprstr += v[0] + ' (' + k + ') : ' + str(v[1]) + '\n'
        return reprstr

