"""Utility functions."""

import re
from subprocess import check_output
import sys
import copy
from ctypes import c_int, POINTER, c_double
import numpy as np
from platform_specifics import DIFFERENTIAL_RETURN_MODE

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
            # Note : Reshape will do nothing if optional_pointers[i] is 1D
            return_array = np.array(optional_ptrs[i]).reshape(shape, order='F')
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
        self.T, self.p, self.x, self.y, self.betaV, self.betaL, \
        self.phase, self.flash_type = T, p, x, y, betaV, betaL, phase, flash_type

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
        return tuple(self.__iter__())[item]

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

class BinaryTriplePoint:
    """
    Holder struct for the result of a three phase state. Implements __iter__ and __getitem__.
    """
    def __init__(self, exists, x1, y, x2, p ,T):
        self.exists, self.T, self.p, self.x1, self.y, self.x2 = exists, T, p, x1, y, x2
        self.iterable = [exists, x1, y, x2, p , T]
        self.contents = ['exists', 'x1', 'y', 'x2', 'p', 'T']
        self.descriptions = {'exists': 'Binary triple point excists',
                             'x1' : 'Liquid 1 phase composition',
                             'y' : 'Vapour phase composition',
                             'x2' : 'Liquid 2 phase composition',
                             'p' : 'pressure [Pa]',
                             'T' : 'Temperature [K]'}

    def __iter__(self):
        return (_ for _ in self.iterable)

    def __getitem__(self, item):
        return tuple(self.__iter__())[item]

    def __repr__(self):
        reprstr = 'BinaryTriplePoint object \n'
        reprstr += 'Containing the attributes (description, name, value):\n'
        for name, val in zip(self.contents, self.iterable):
            reprstr += f'\t{self.descriptions[name] : <30} {name : <5} : {val}  \n'

        return reprstr

    def __str__(self):
        return self.__repr__()

class Differentials:

    def __init__(self, diffs, variables):
        self.dT = None
        self.dp = None
        self.dV = None
        self.dn = None

        self.iterable = tuple()
        if variables == 'tvn':
            self.dT, self.dV, self.dn = diffs
            for d in (self.dT, self.dV, self.dn):
                if d is not None:
                    self.iterable += (d,)

        elif variables == 'tpn':
            self.dT, self.dp, self.dn = diffs
            self.iterable = tuple()
            for d in (self.dT, self.dp, self.dn):
                if d is not None:
                    self.iterable += (d,)

    def __iter__(self):
        return (_ for _ in self.iterable)

    def __getitem__(self, item):
        return self.iterable[item]

    def __bool__(self):
        if len(self.iterable) > 0:
            return True
        return False

class Property:

    def __init__(self, val, diffs):
        self.diffs = diffs
        self.val = val

        self.iterable = (self.val,)
        for d in self.diffs:
            self.iterable += (d,)

    def __iter__(self):
        if DIFFERENTIAL_RETURN_MODE == 'old':
            return (_ for _ in self.iterable)
        return (_ for _ in (self.val, self.diffs))

    def __getitem__(self, item):
        if DIFFERENTIAL_RETURN_MODE == 'old':
            return self.iterable[item]
        return [self.val, self.diffs][item]