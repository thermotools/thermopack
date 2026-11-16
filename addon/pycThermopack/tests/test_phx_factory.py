import numpy as np
from thermopack.thermopack_state import State,phase_state_list

def test_phx_list_preserves_enthalpy_inputs():
    enthalpies=[12000.,24000.]
    pressures=[1e5,2e5]
    compositions=[np.array([.4,.6]),np.array([.7,.3])]
    states=phase_state_list.new_phx(None,enthalpies,pressures,compositions)
    assert len(states)==2
    for state,h,p,x in zip(states,enthalpies,pressures,compositions):
        reference=State.new_phx(None,h,p,x)
        assert state.temperature is reference.temperature is None
        assert state.specific_enthalpy==reference.specific_enthalpy==h
        assert state.pressure==reference.pressure==p
        np.testing.assert_array_equal(state.molefrac,x)
