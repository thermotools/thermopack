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
    f = pcp.fres_multipol(temp, volume, n)
    assert(f == approx(-4.38559558854186E-002, rel=1.0e-6))

def test_quadrupole_mix():
    """Test quadrupole term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("co2,cl2,C2_1", polar=True)
    # Set a compositon
    n = np.array([1.0, 2.0, 3.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_multipol(temp, volume, n)
    assert(f == approx(-0.327493924806138, rel=1.0e-6))

def test_dipole():
    """Test dipol term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("DME", polar=True)
    # Set a compositon
    n = np.array([1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_multipol(temp, volume, n)
    assert(f == approx(-1.40501033595417E-002, rel=1.0e-6))

def test_dipole_mix():
    """Test dipol term for mixtures"""

    # Set up PCP-SAFT model
    pcp = pcsaft("acetone,butanal,DME", polar=True)
    # Set a compositon
    n = np.array([1.0, 2.0, 3.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f = pcp.fres_multipol(temp, volume, n)
    assert(f == approx(-1.4126308106201688, rel=1.0e-6))

def test_dipole_quadrupole():
    """Test dipole-quadrupole term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("DME,CO2", polar=True)
    # Set a compositon
    n = np.array([1.0, 1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f_qq = pcp.fres_multipol(temp, volume, n, qq=True, dd=False, dq=False)
    f_dd = pcp.fres_multipol(temp, volume, n, qq=False, dd=True, dq=False)
    f_dq = pcp.fres_multipol(temp, volume, n, qq=False, dd=False, dq=True)
    assert((f_qq, f_dd, f_dq) == approx((-4.20168059082731E-002, -1.35361827881345E-002, -2.2316252638709004E-002), rel=1.0e-6))

def test_dipole_quadrupole():
    """Test dipole-quadrupol term"""

    # Set up PCP-SAFT model
    pcp = pcsaft("DME,CO2", polar=True)
    # Set a compositon
    n = np.array([1.0, 1.0])
    temp = 350.0
    volume = 1.0e-27*NA
    f_qq = pcp.fres_multipol(temp, volume, n, qq=True, dd=False, dq=False)
    f_dd = pcp.fres_multipol(temp, volume, n, qq=False, dd=True, dq=False)
    f_dq = pcp.fres_multipol(temp, volume, n, qq=False, dd=False, dq=True)
    assert((f_qq, f_dd, f_dq) == approx((-4.20168059082731E-002, -1.35361827881345E-002, -2.2316252638709004E-002), rel=1.0e-6))
