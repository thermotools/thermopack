"""Utility functions."""

import re
from subprocess import check_output
import sys
import copy
from ctypes import c_int, POINTER, c_double
import numpy as np

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

def get_optional_pointers(optional_flags, optional_arrayshapes):
    """
    Gets a list of pointers to double or nullptr, to pass to fortran for optional arguments.

    Args:
        optional_flags (list) : What optional values to compute, only compute those that are not None
        optional_arrayshapes (list<tuple>) : The shape of each pointer. Use (0,) for singular values.
    Returns:
        (list<c_double>) : Pointers to be passed to Fortran-side to compute optional values
    """
    null_pointer = POINTER(c_double)()
    optional_ptrs = [null_pointer for _ in optional_flags]
    for i, (flag, shape) in enumerate(zip(optional_flags, optional_arrayshapes)):
        if flag is None:
            continue
        if np.product(shape) > 0:
            optional_ptrs[i] = (c_double * np.product(shape))(0.0)
        else:
            optional_ptrs[i] = c_double(0.0)
    return optional_ptrs

def fill_return_tuple(return_tuple, optional_ptrs, optional_flags, optional_arrayshapes):
    """
    Takes the values held py the pointers in the list optional_ptrs, and adds them to the tuple return_tuple. For use
    with get_optional_pointers(). Pass the list of pointers that hold optional return values to this function, to fill
    the return tuple with the values that have been computed.

    Args:
        return_tuple (tuple) : A (possibly empty) tuple of return values
        optional_ptrs (list<c_double>) : Pointers to computed values
        optional_flags (list<bool>) : The flags for what optional values to compute, in the same order as optional_ptrs
        optional_arrayshapes (list<tuple>) : The shape of the return values. Use (0,) for singular values
    Returns:
        (tuple) : The tuple to be returned by the thermopack method
    """
    for i, (flag, shape) in enumerate(zip(optional_flags, optional_arrayshapes)):
        if flag is None:
            continue
        if np.product(shape) > 0:
            # Need to transpose because fortran is column-major
            # Note : Reshape and transpose will do nothing if optional_pointers[i] is 1D
            return_array = np.array(optional_ptrs[i]).reshape(shape).transpose()
            return_tuple += (copy.deepcopy(return_array), )
        else:
            return_tuple += (optional_ptrs[i].value, )

    return return_tuple

class FlashResult:
    """
    Holder struct for the result of a flash calculation. Implements __iter__ and __getitem__ for
    backward compatibility.
    """
    def __init__(self, z, T, p, x, y, betaV, betaL, phase, flash_type):
        self.z = [_ for _ in z]
        self.T, self.x, self.y, self.betaV, self.betaL, \
        self.phase, self.flash_type = T, x, y, betaV, betaL, phase, flash_type

        self.iterable = [T, p, x, y, betaV, betaL, phase]
        self.contents = ['T', 'p', 'x', 'y', 'betaV', 'betaL', 'phase']
        self.descriptions = {'T' : 'Temperature [K]', 'p' : 'pressure [Pa]',  'x' : 'Liquid phase composition',
                             'y' : 'Vapour phase composition', 'betaV' : 'Vapour fraction',
                             'betaL' : 'Liquid fraction', 'phase' : 'Phase indentifier index',
                             'z' : 'Total composition', 'flash_type' : 'Flash type'}

    def __iter__(self):
        if self.flash_type == 'Tp':
            return (_ for _ in self.iterable[2:]) # Exclude T and p
        elif self.flash_type in ('pH', 'pS'):
            return (_ for _ in self.iterable[0:1] + self.iterable[2:]) # Exclude p
        else:
            return (_ for _ in self.iterable)

    def __getitem__(self, item):
        return self.iterable[item]

    def __repr__(self):
        reprstr = 'FlashResult object for ' + self.flash_type + '-flash\n'
        reprstr += 'Containing the attributes (description, name, value):\n'
        for name, val in zip(['flash_type', 'z'], [self.flash_type, self.z]):
            reprstr += f'\t{self.descriptions[name] : <30} {name : <5} : {val}  \n'
        for name, val in zip(self.contents, self.iterable):
            reprstr += f'\t{self.descriptions[name] : <30} {name : <5} : {val}  \n'

        return reprstr

    def __str__(self):
        return self.__repr__()
