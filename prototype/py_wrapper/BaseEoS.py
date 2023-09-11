from . import libdemo # The module defined by pybind11
import numpy as np
from collections.abc import Iterable

class BaseEoS:

    def __init__(self):
        self.nc = 0
        self.lib = None # In this variable, we will store an initialized C++-side model.

    def pressure(self, T, V, n):
        p = np.array([0], dtype=np.float32)
        self.lib.pressure(T, V, n, p)
        return p[0]

    def get_empty_arrays(self, flags, dims, dtypes):
        """
        Helper function to generate empty arrays that are used to pass optional, modifiable arguments to the C++ side.

        Note that optional floats that are to be modified must be passed as 1d arrays with a single element.
        """
        if not isinstance(dims, Iterable):
            dims = dims * np.ones_like(flags)
        if not isinstance(dtypes, Iterable):
            dtypes = [dtypes for _ in flags]

        arrs = [np.empty(tuple(dims[fi] for _ in range(dims[fi])), dtype=dtypes[fi]) if (flags[fi] is not None) else
                None
                for fi in range(len(flags))]
        return arrs

    def Fres(self, T, V, n, Ft=None, Fv=None, Fn=None):
        F = np.empty((1,), dtype=np.float32)
        flags = [Ft, Fv, Fn]
        dims = [1, 1, 2]
        dtypes = np.float32
        # These arrays will be modified. If they contain None, Pybind will ensure that nullptr is passed to C++,
        # Such that Fortran never sees them.
        Ft, Fv, Fn = self.get_empty_arrays(flags, dims, dtypes)

        self.lib.Fres(T, V, n, F, Ft, Fv, Fn)

        return F[0], [i for i in Ft] if (Ft is not None) else None, \
                    [i for i in Fv] if (Fv is not None) else None, \
                    [i for i in Fn] if (Fn is not None) else None

class NotIdGas(BaseEoS):

    def __init__(self, ident, ncomps, Tc, Vc):
        self.nc = ncomps
        super().__init__()
        self.lib = libdemo.NotIdGas(ident, ncomps, Tc, Vc) # Initialize the corresponding C++ side model


