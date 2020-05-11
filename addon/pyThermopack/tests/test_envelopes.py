"""Simple set of (unit)tests for pytp."""

from __future__ import print_function

import numpy as np

from pytp import tp
from pytp import utils


def test_get_envelope_twophase():
    """Test utils.get_envelope_twophase"""
    tp.init("Thermopack", "SRK", "Classic", "Classic", 2, "CO2,N2", 3, 1, 1)

    Tarrs = []
    parrs = []
    z2vals = np.linspace(0.01, 0.05, 20)

    T_init = 230.0
    p_init = 10e5

    for z2 in z2vals:
        z = np.array((1.0-z2, z2))
        Tvals, pvals = utils.get_envelope_twophase(T_init, p_init, z)[0:2]
        Tarrs.append(Tvals)
        parrs.append(pvals)

        # All of these should have been able to wrap around.
        assert Tvals[-1] < Tvals[0]
        assert Tvals[-1] < np.max(Tvals)
        assert pvals[-1] < np.max(pvals)


def test_envelope_isentrope_cross_single():
    """Test something else (not sure) - unfinished test!"""
    tp.init("Thermopack", "SRK", "Classic", "Classic", 2, "CO2,N2", 3, 1, 1)

    z = np.array((1.0, 0.0))

    Tvals_above = np.linspace(180.0, 320.0, 10)
    pvals_above = np.ones_like(Tvals_above)*80e5

    pvals_side = np.linspace(1e5, 60e5, 20)
    Tvals_side = np.ones_like(pvals_side)*288.0

    Tvals_below = np.linspace(220.0, 320.0, 10)
    pvals_below = np.ones_like(Tvals_above)*2e5

    Tvals = np.hstack((Tvals_above, Tvals_side, Tvals_below))
    pvals = np.hstack((pvals_above, pvals_side, pvals_below))


    for p0, T0 in zip(pvals, Tvals):
        tp_res = tp.tpflash_twophase(T0, p0, z, 0.5)
        phase = tp_res[2]
        s_spec = tp.specific_entropy(T0, p0, z, phase)

        res = tp.envelope_isentrope_cross_single(p0, s_spec, z, 1e5)
        # has_crossing, p_cross, T_cross, phase_cross, ierr = res
        assert len(res) == 5
