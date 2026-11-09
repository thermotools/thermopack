"""Regression tests for the vectorized properties of phase_state_list."""

import numpy as np
from numpy.testing import assert_array_equal

from thermopack.thermopack_state import State, phase_state_list


def make_state(temperature, enthalpy, molefrac):
    return State(None, temperature, 1.0, np.array(molefrac),
                 p=1e5, h=enthalpy)


def test_properties_remain_bound_to_each_list():
    liquid_states = [make_state(300, 10, [0.2, 0.8]),
                     make_state(310, 11, [0.3, 0.7])]
    liquid = phase_state_list(liquid_states)
    assert_array_equal(liquid.specific_enthalpy, [10, 11])

    vapour = phase_state_list([make_state(400, 20, [0.9, 0.1])])
    assert_array_equal(liquid.specific_enthalpy, [10, 11])
    assert_array_equal(vapour.specific_enthalpy, [20])
    assert_array_equal(liquid.temperature, [300, 310])
    assert_array_equal(liquid.molefrac, [[0.2, 0.8], [0.3, 0.7]])

    empty = phase_state_list([])
    assert_array_equal(empty.specific_enthalpy, [])
    assert_array_equal(liquid.specific_enthalpy, [10, 11])
    assert_array_equal(vapour.specific_enthalpy, [20])

    liquid.append(make_state(320, 12, [0.4, 0.6]))
    assert_array_equal(liquid.specific_enthalpy, [10, 11, 12])
    assert_array_equal(vapour.specific_enthalpy, [20])
    assert_array_equal(empty.specific_enthalpy, [])
