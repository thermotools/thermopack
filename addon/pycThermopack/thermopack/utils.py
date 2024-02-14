"""Utility functions."""

import re
from subprocess import check_output
import sys
import copy
from ctypes import c_int, POINTER, c_double
import numpy as np
from .platform_specifics import DIFFERENTIAL_RETURN_MODE

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
        self.descriptions = {'exists': 'Binary triple point exists',
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
    """
    Holder struct for differentials, implements some methods to be as backwards compatible as possible.
    """
    def __init__(self, diffs, variables):
        """Constructor

        Args:
            diffs (tuple) : (all three) differentials
            variables (str) : Key (either 'tvn' or 'tpn') indicating what variables are held constant, and what
                            differentials are found in the tuple 'diffs'.
        """
        self.dT = None
        self.dp = None
        self.dV = None
        self.dn = None

        self.iterable = tuple()
        self.constant = variables
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
        else:
            raise KeyError(f'Invalid differential variables key : "{variables}"')

    @staticmethod
    def from_return_tuple(vals, flags, variables):
        """Constructor
        Method to construct a Differentials object from a truncated "old-style" return_tuple. Note: `vals` should be
        return_tuple[1:], such that it does not contain the property value, only differentials.

        Args:
            vals (tuple) : A (possibly empty) tuple of differentials.
            flags (tuple) : The flags passed to the thermopack method (dxdt, dxdv, dxdn) or (dxdt, dxdp, dxdn).
            variables (str) : Key (either 'tvn' or 'tpn') indicating what differentials are expected in `vals`

        Returns:
            Differentials : Constructed from the truncated return_tuple.
        """
        dT, dV, dp, dn = None, None, None, None
        diff_idx = 0
        if flags[0] is not None:
            dT = vals[diff_idx]
            diff_idx += 1

        if variables == 'tvn':
            if flags[1] is not None:
                dV = vals[diff_idx]
                diff_idx += 1
            if flags[2] is not None:
                dn = vals[diff_idx]

            diffs = (dT, dV, dn)
        elif variables == 'tpn':
            if flags[diff_idx] is not None:
                dp = vals[diff_idx]
                diff_idx += 1
            if flags[2] is not None:
                dn = vals[diff_idx]
            diffs = (dT, dp, dn)
        else:
            raise KeyError(f'Invalid differential variables key : "{variables}"')

        return Differentials(diffs, variables)

    def __repr__(self):
        ostr = 'Differentials object containing the attributes (description / name / value)\n'
        ostr += f'\t{"Variables held constant" : <25} constant : {self.constant}\n'
        ostr += f'\t{"Temperature derivative" : <31} dT : {self.dT}\n'
        ostr += f'\t{"Pressure derivative" : <31} dp : {self.dp}\n'
        ostr += f'\t{"Volume derivative" : <31} dV : {self.dV}\n'
        ostr += f'\t{"Mole number derivative" : <31} dn : {self.dn}\n'
        return ostr

    def __str__(self):
        return self.__repr__()

    def __iter__(self):
        return (_ for _ in self.iterable)

    def __getitem__(self, item):
        return self.iterable[item]

    def __bool__(self): # To easily check whether there are any differentials
        if len(self.iterable) > 0:
            return True
        return False

class Property:
    """
    Holder struct to return a property and its derivatives
    """
    def __init__(self, val, diffs):
        """Constructor

        Args:
            val (float or array_like) : The value of the property
            diffs (Differentials) : The derivatives
        """
        self.diffs = diffs
        self.val = val

        if DIFFERENTIAL_RETURN_MODE == 'v2':
            self.__return_tuple__ = (self.val,)
            for d in self.diffs:
                self.__return_tuple__ += (d,)
        else:
            self.__return_tuple__ = "There are no return tuples. Build with 'python makescript.py [optim/debug] -diffs=v2.1\n" \
                                    "to use old style return tuples."

    @staticmethod
    def from_return_tuple(return_tuple, flags, variables):
        """Constructor
        Construct a Property object from an "old-style" return_tuple, along with flags

        Args:
            return_tuple (tuple) : The "old-style" return_tuple
            flags (tuple) : The flags passed to the function generating the return tuple
            variables (str) : Key ('tvn' or 'tpn') indicating what differentials to expect in the return tuple

        Returns:
            Property : Constructed from the return_tuple
        """
        diffs = Differentials.from_return_tuple(return_tuple[1:], flags, variables)
        return Property(return_tuple[0], diffs)

    def __repr__(self):
        ostr = f'Property struct evaluated at constant ({self.diffs.constant})\n'
        ostr += 'Containing the attributes \n'
        ostr += f'Value         : val'
        ostr += f'Differentials : diffs'
        return ostr

    def __str__(self):
        return self.__repr__()

    def __iter__(self):
        """
        Make sure the Property object behaves like the old-style return_tuple if we want that.
        """
        if DIFFERENTIAL_RETURN_MODE == 'v2':
            return (_ for _ in self.__return_tuple__)
        if self.diffs:
            return (_ for _ in (self.val, self.diffs))
        return (_ for _ in [self.val])

    def __getitem__(self, item):
        """
        See: __iter__
        """
        if DIFFERENTIAL_RETURN_MODE == 'v2':
            return self.__return_tuple__[item]
        return [self.val, self.diffs][item]

    def unpack(self):
        """
        Ensure that ThermoPack methods can just `return Property.unpack()`, and then differentiating old-style vs.
        new-style is done here, rather than in every ThermoPack method.
        """
        if self.diffs: # Correct packing for backwards compatibility handled in __iter__
            return tuple(self.__iter__())

        if DIFFERENTIAL_RETURN_MODE == 'v2':
            return self.__return_tuple__

        return self.val

def back_compatible_unpack(prop):
    """
    Use for backwards compatibility with v2. This is a dead simple one-liner, but thermopack_state.py became a lot
    cleaner when this was put in a function, rather than cluttering the whole thing with a bunch of ternaries.
    """
    return prop[0] if DIFFERENTIAL_RETURN_MODE == 'v2' else prop

class XYEquilibrium:

    def __init__(self, eq, key):
        self.x = None
        self.y = None
        self.x1 = None
        self.x2 = None
        if key == 'lle':
            self.x1 = eq[0] if eq[0] is not None else []
            self.x2 = eq[1] if eq[1] is not None else []
        elif key == 'lve':
            self.x = eq[0] if eq[0] is not None else []
            self.y = eq[1] if eq[1] is not None else []
        else:
            raise KeyError(f'Invalid XYEquilibrium key : {key}.')

        self.type = key

    def __iter__(self):
        if self.type == 'lle':
            return (_ for _ in (self.x1, self.x2))
        elif self.type == 'lve':
            return (_ for _ in (self.x, self.y))
        else:
            raise KeyError(f'Invalid XYEquilibrium type : {self.type}.')

    def __getitem__(self, item):
        return [*self][item]

    def __repr__(self):
        ostr = 'XYEquilibrium object with attributes (description, name, value)\n'
        ostr += f'\t{"Type of equilibrium": <37} type : {self.type}\n'
        vals = (self.x, self.y, self.x1, self.x2)
        names = ('x', 'y', 'x1', 'x2')
        descriptions = ('Liquid mole fraction, species 1', 'Vapour mole fraction, species 1',
                        'Liquid 1 mole fraction, species 1', 'Liquid 2 mole fraction, species 1')
        for val, name, desc in zip(vals, names, descriptions):
            if val is None:
                continue
            ostr += f'\t{desc: <37} {name: <4} : '
            if len(val) == 0:
                ostr += f'[], len({name}) = 0\n'
            else:
                ostr += f'[{val[0]:.3e} ... {val[-1]:.3e}], len({name}) = {len(val)}\n'
        return ostr

    def __str__(self):
        return self.__repr__()

    def __bool__(self):
        for x in (self.x, self.y, self.x1, self.x2):
            if len(x) > 0:
                return True
        return False

class PxyEquilibrium(XYEquilibrium):

    def __init__(self, eq, key):
        super().__init__(eq, key)
        self.p = eq[2] if eq[2] is not None else []

    def __iter__(self):
        vals = [*super().__iter__(), self.p]
        if DIFFERENTIAL_RETURN_MODE == 'v2':
            for i in range(len(vals)):
                if len(vals[i]) == 0:
                    vals[i] = None
        return (_ for _ in vals)

    def __getitem__(self, item):
        return [*self][item]

    def __repr__(self):
        ostr = super().__repr__()
        ostr = '\n'.join(ostr.split('\n')[1:])
        ostr = 'PxyEquilibrium object with attributes (description, name, value)\n' + ostr
        ostr += f'\t{"Pressure" : <37} p    : '
        if len(self.p) == 0:
            ostr += f'[], len(p) = 0'
        else:
            ostr += f'[{self.p[0]:.3e} ... {self.p[-1]:.3e}], len(p) = {len(self.p)}'
        return ostr + '\n'

class TxyEquilibrium(XYEquilibrium):

    def __init__(self, eq, key):
        super().__init__(eq, key)
        self.T = eq[2] if eq[2] is not None else []

    def __iter__(self):
        vals = [*super().__iter__(), self.T]
        if DIFFERENTIAL_RETURN_MODE == 'v2':
            for i in range(len(vals)):
                if len(vals[i]) == 0:
                    vals[i] = None
        return (_ for _ in vals)

    def __getitem__(self, item):
        return [*self][item]

    def __repr__(self):
        ostr = super().__repr__()
        ostr = '\n'.join(ostr.split('\n')[1:])
        ostr += f'\t{"Temperature" : <37} T    : '
        if len(self.T) == 0:
            ostr += f'[], len(T) = 0\n'
        else:
            ostr += f'[{self.T[0]:.3e} ... {self.T[-1]:.3e}], len(T) = {len(self.T)}'
        return ostr + '\n'

class XYDiagram:

    def __init__(self, lle, l1ve, l2ve, key):

        if key == 'pxy':
            xyEquilibrium = PxyEquilibrium
        elif key == 'txy':
            xyEquilibrium = TxyEquilibrium
        else:
            raise KeyError(f'Invalid XYDiagram key : {key}')

        self.type = key
        self.lle = xyEquilibrium(lle, 'lle')
        self.l1ve = xyEquilibrium(l1ve, 'lve')
        self.l2ve = xyEquilibrium(l2ve, 'lve')

    def __iter__(self):
        return (_ for _ in (self.lle, self.l1ve, self.l2ve))

    def __getitem__(self, item):
        return [*self][item]

    def __repr__(self):
        ostr = 'XYDiagram object with attributes (name : description)\n'

        headers = ('lle  : Liquid 1 - Liquid 2 Equilibrium', 'l1ve : Liquid 1 - Vapour Equilibrium',
                   'l2ve : Liquid 2 - Vapour Equilibrium ')
        vals = (self.lle, self.l1ve, self.l2ve)
        for head, val in zip(headers, vals):
            ostr += f'{head}\n'
            lines = str(val).split('\n')
            for line in lines:
                ostr += '\t' + line + '\n'
        return ostr

    def __str__(self):
        return self.__repr__()