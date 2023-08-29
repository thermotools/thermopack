"""Simple set of (unit)tests for thermopack."""

import numpy as np
from thermopack.thermo import thermo


def test_get_envelope_twophase():
    """Test thermo.get_envelope_twophase"""

    # Instanciate thermopack object
    tp = thermo()
    # Initialize using Peng-Robinson
    tp.init_thermo("PR", "Classic", "Classic", "CO2,C1", 2)
    # Set a compositon
    z = np.array([0.9, 0.1])
    # Map phase envelop
    Tvals, Pvals = tp.get_envelope_twophase(1.0e5, z)
    assert len(Tvals) > 0
