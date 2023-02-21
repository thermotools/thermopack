"""Simple set of (unit)tests for thermopack."""

import numpy as np
from thermopack.pcsaft import pcsaft
from pytest import approx

NA = 6.02214076e23

def test_quadrupole():
    """Test quadrupol term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("CO2", polar=True)
    # Set a compositon
    n = np.array([1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_polar(temp, volume, n)
    assert(f == approx(-0.04385611664750655, rel=1.0e-6))

def test_quadrupole_mix():
    """Test quadrupole term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("CO2,CL2,C2_1", polar=True)
    # Set a compositon
    n = np.array([1.0, 2.0, 3.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_polar(temp, volume, n)
    assert(f == approx(-0.27480174686693515, rel=1.0e-6))

def test_dipole():
    """Test dipol term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("DME", polar=True)
    # Set a compositon
    n = np.array([1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_polar(temp, volume, n)
    assert(f == approx(-1.40501033595417E-002, rel=1.0e-6))

def test_dipole_mix():
    """Test dipol term for mixtures"""

    # Set up PCP-SAFT model
    pcp = pcsaft("ACETONE,BUTANAL,DME", polar=True)
    # Set a compositon
    n = np.array([1.0, 2.0, 3.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_polar(temp, volume, n)
    assert(f == approx(-1.4126308106201688, rel=1.0e-6))

def test_dipole_quadrupole():
    """Test dipole-quadrupole term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("DME,CO2", polar=True)
    # Set a compositon
    n = np.array([1.0, 1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f_qq = pcp.fres_polar(temp, volume, n, qq=True, dd=False, dq=False)
    f_dd = pcp.fres_polar(temp, volume, n, qq=False, dd=True, dq=False)
    f_dq = pcp.fres_polar(temp, volume, n, qq=False, dd=False, dq=True)
    assert((f_qq, f_dd, f_dq) == approx((-0.04201697115890276,
                                         -0.013536175556332925,
                                         -2.2452749809708863E-002), rel=1.0e-6))
