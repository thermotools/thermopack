import numpy as np
import pytest
from thermopack import utils
from thermopack.thermopack_state import State


@pytest.mark.parametrize('mode', ['v2', 'v3'])
@pytest.mark.parametrize('property_name', ['pressure', 'specific_volume'])
def test_state_pressure_and_volume_return_modes(monkeypatch, mode, property_name):
    monkeypatch.setattr(utils, 'DIFFERENTIAL_RETURN_MODE', mode)

    class EOS:
        def pressure_tv(self, T, V, n):
            return (120000.,) if mode == 'v2' else 120000.

        def specific_volume(self, T, p, x, ph):
            return (0.02,) if mode == 'v2' else 0.02

    x = np.array([0.4, 0.6])
    tv = State(EOS(), 300., 0.02, x)
    tp = State(EOS(), 300., None, x, p=120000., ph=2)
    if property_name == 'pressure':
        assert tv.pressure == 120000.
        assert tv.p == 120000.
    else:
        assert tp.specific_volume == 0.02
        assert tp.v == 0.02
        np.testing.assert_allclose(tp.partial_density, [20., 30.])
