"""Wrappers around selected pytp routines applying them to arrays.

Intended to remove the hassle of repeatedly writing loops around routines
taking single floats as input.
"""

from pytp import utils
from pytp import tp


bubble_p = utils.vectorize_1D(tp.bubble_p)
dew_p = utils.vectorize_1D(tp.dew_p)
sat_p_pure_array = utils.vectorize_1D(tp.sat_p_pure)
sat_T_pure_array = utils.vectorize_1D(tp.sat_t_pure)
