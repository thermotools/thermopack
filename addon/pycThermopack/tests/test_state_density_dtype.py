import numpy as np
import pytest
from thermopack import utils
from thermopack.thermopack_state import State


@pytest.mark.parametrize('moles', [[1, 2], [0, 2]])
@pytest.mark.parametrize('dtype', [int, float])
@pytest.mark.parametrize('lazy', [False, True])
def test_partial_density_is_independent_of_mole_number_dtype(moles, dtype, lazy, monkeypatch):
    # Exercise the legacy tuple return path independently of scalar-return fixes.
    monkeypatch.setattr(utils, 'DIFFERENTIAL_RETURN_MODE', 'v2')

    class EOS:
        def specific_volume(self, T, p, x, ph):
            return (2. / sum(moles),)

    state = State(EOS(), 300., None if lazy else 2.,
                  np.array(moles, dtype=dtype), p=1e5, ph=2)
    np.testing.assert_allclose(state.partial_density, np.array(moles) / 2.)
    np.testing.assert_allclose(state.rho.sum(), 1. / state.specific_volume)
    assert np.issubdtype(state.partial_density.dtype, np.floating)
