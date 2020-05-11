"""Provides a set of nice utility functions."""

from __future__ import print_function
from builtins import range

import numpy as np

from . import tp
from . import const


def beta_to_alpha(beta_i, v_i, v):
    """Convert from molar fraction to volume fraction of a phase

    Arguments
        beta_i      Molar fraction of phase i (-)
        v_i         Molar volume of phase i (m3/mol)
        v           Total molar volume (m3/mol)

    Returns
        Volume fraction of phase i
    """
    return v_i*beta_i/v


def get_envelope_twophase(T_init=220.0, p_init=10e5, z=(0.98, 0.02)):
    """Get the phase-envelope in a robust way

    pytp.tp.init() must have been called first.

    Arguments
        T_init      Initial guess for temperature for p_init
        p_init      Initial pressure
        z           Mixture molar composition

    Returns
        Temperature values (K)
        Pressure values (Pa)
        Molar composition of incipient phase (-)
        Label of incipient phase (-)
    """
    z = np.array(z)
    pmax = 200e5

    # Start from dew-line
    beta = 1.0
    rspec = 1  #1: specify p, 2: specify T
    Tvals, pvals, Kvals, beta_vals, nvals \
            = tp.envelope_twophase(z, T_init, p_init, rspec, beta, pmax)
    Tvals = Tvals[1:nvals-1]
    pvals = pvals[1:nvals-1]
    Kvals = Kvals[1:nvals-1]
    beta_vals = beta_vals[1:nvals-1]

    return Tvals, pvals, Kvals, beta_vals


def get_stability_limit_line_tp(z, p0, Tmin):
    """Get line of meta-stability limit in Tp-space.

    Draws line from starting pressure to the critical pressure.
    OBS: Includes both liquid and vapor metastability limit.

    Arguments
        z           Total molar composition (-)
        p0          Starting pressure (Pa)
        Tmin        Minimum temprature (K)

    Returns
        Temperatures along line (K)
        Pressures along line (Pa)
    """
    T, p, npts, ierr = tp.stability_limit_line_tp(z, p0, Tmin)
    if ierr != 0:
        raise ValueError("Error from stability_limit_line_tp()")
    T = T[:npts]
    p = p[:npts]
    return T, p


def get_stability_limit_liquid_p(p, z, noStop=False):
    """Get liquid stability limit for a given pressure.

    Arguments
        z           Total molar composition (-)
        p           Starting pressure (Pa)
        noStop      Continue in case of error (Boolean)

    Returns
        Temperature for stability limit (K)
    """
    T, v, ierr = tp.stability_limit_liquid_p(z, p)
    if ierr != 0:
        if noStop is True:
            # For some computations we do not want to raise an error
            # but return a dummy value
            T = 0.0
        else:
            raise ValueError("Error from stability_limit_liquid_p()")
    return T, v


def get_stability_limit_vapor_p(p, z, noStop=False):
    """ Get gas stability limit for a given pressure.

    Arguments
        z           Total molar composition (-)
        p           Starting pressure (Pa)
        noStop      Continue in case of error (Boolean)

    Returns
        Temperature for stability limit (K)
    """
    T, v, ierr = tp.stability_limit_vapor_p(z, p)
    if ierr != 0:
        if noStop is True:
            # For some computations we do not want to raise an error
            # but return a dummy value
            T = 0.0
        else:
            print(ierr)
            raise ValueError("Error from stability_limit_vapor_p()")
    return T, v


def get_isentrope_Tp(T_init, p_init, z, p_min=1e5, npts=100):
    """Get properties along isentropic depressurization.

    Assumes no more than two phases.

    Arguments
        T_init      Initial temperature (K)
        p_init      Initial pressure (Pa)
        z           Total molar composition (z)
        p_min       Minimum pressure (Pa)
        npts        Points to calculate along isentrope

    Returns
        Pressure values, ascending (Pa)
        Temperature values (K)
        Density values (kg/m3)
        Speed of sound values (m/s)
        Full-bore outflow speed (m/s)
        Index of full-bore pipe decompression choke point
        Index of saturation-point
    """
    # Get molar weight
    mwi = tp.get_molar_weights(len(z))
    mw = np.sum(z*mwi)

    # Get initial state information
    beta_guess = 0.0
    tpres = tp.tpflash_twophase(T_init, p_init, z, beta_guess)
    beta_init = tpres[0]
    phase_init = tpres[2]
    x_init = tpres[3]
    y_init = tpres[4]
    s0 = tp.specific_entropy_twophase(
        T_init, p_init, z, x_init, y_init, beta_init, phase_init)
    c0 = tp.speed_of_sound_twophase(
        T_init, p_init, z, x_init, y_init, beta_init, phase_init)
    v_molar0 = tp.specific_volume_twophase(
        T_init, p_init, z, x_init, y_init, beta_init, phase_init)
    rho0 = mw/v_molar0

    # Construct arrays of isentrope properties
    pvals = np.linspace(p_min, p_init, npts)
    Tvals = np.zeros(npts)
    rhovals = np.zeros(npts)
    cvals = np.zeros(npts)
    uvals = np.zeros(npts)

    # Set values for initial point
    Tvals[-1] = T_init
    rhovals[-1] = rho0
    cvals[-1] = c0
    uvals[-1] = 0.0

    # Initialize saturation point index
    isat = 0

    # Go along isentrope, from the top
    T_guess = T_init
    beta_guess = beta_init
    for i in range(npts-2, -1, -1):
        p = pvals[i]
        psres = tp.psflash_twophase(p, s0, z, T_guess, beta_guess)
        T = psres[0]
        beta = psres[1]
        phase = psres[3]
        x = psres[4]
        y = psres[5]

        # Hit saturation point yet?
        if isat == 0 and phase == const.TWOPH:
            isat = i

        # Store temperature
        Tvals[i] = T

        # Get speed of sound
        cvals[i] = tp.speed_of_sound_twophase(T, p, z, x, y, beta, phase)

        # Get density
        v_molar = tp.specific_volume_twophase(T, p, z, x, y, beta, phase)
        rhovals[i] = mw/v_molar

        # Calculate outflow speed
        rho = 0.5*(rhovals[i] + rhovals[i+1])
        c = 0.5*(cvals[i] + cvals[i+1])
        dp = pvals[i+1] - pvals[i]
        uvals[i] = uvals[i+1] + dp/(c*rho)

        # Update guess values (linear extrapolation of T)
        dT = Tvals[i] - Tvals[i+1]
        T_guess = T + dT
        beta_guess = beta

    # Get decompression speed (speed of pressure level)
    vvals = cvals - uvals

    # Get index of choke-point
    if vvals[0] > 0.0:
        raise ValueError("p_min not small enough to reach choke")
    ichoke = np.searchsorted(vvals, 0.0)

    return pvals, Tvals, rhovals, cvals, uvals, ichoke, isat


def get_tp_routines():
    """Get the names of interface routines in pytp.tp"""
    names = []
    for name in dir(tp):
        if not name.startswith("_"):
            names.append(name)
    return names


def get_doc(name):
    """Get the documentation of routine "name" in pytp.tp"""
    return getattr(tp, name).__doc__


def showdoc():
    """Show the interface for every interface routine."""
    names = get_tp_routines()
    for name in names:
        doc = get_doc(name)
        print("\n-------------------------------------")
        print(doc)
        print("-------------------------------------")
    print("\nList of routines: ")
    for name in names:
        print("  ", name)


def vectorize_1D(func):
    """Make wrapper to apply 'func' vectorially.

    The function 'func' should have the interface:

        retval = func(x,*params)

    Returns a function-object, which is the vectorized wrapper.
    """
    # Construct function
    def vectorized_wrapper(x_array, *params):
        """Vectorized function."""
        ans_array = np.zeros_like(x_array)
        for i, x in enumerate(x_array):
            ans_array[i] = func(x, *params)
        return ans_array

    # Return function object
    return vectorized_wrapper


def vllebinarypxy(T, PMax=200e5, dzMax=0.005, dlnsMax=0.01, Pmin=1.0e5):
    """Return P,x1,y1 for an binary envelope"""
    #~ MH 2015-09-10: Initial version
    (xLLE, wLLE, pLLE, nLLE,
     xL1VE, yL1VE, pL1VE, nL1VE,
     xL2VE, yL2VE, pL2VE, nL2VE) = tp.vllebinarypxy(T, PMax, dzMax, dlnsMax, Pmin)
    xLLE = xLLE[:nLLE]
    wLLE = wLLE[:nLLE]
    pLLE = pLLE[:nLLE]
    xL1VE = xL1VE[:nL1VE]
    yL1VE = yL1VE[:nL1VE]
    pL1VE = pL1VE[:nL1VE]
    xL2VE = xL2VE[:nL2VE]
    yL2VE = yL2VE[:nL2VE]
    pL2VE = pL2VE[:nL2VE]
    return xLLE, wLLE, pLLE, xL1VE, yL1VE, pL1VE, xL2VE, yL2VE, pL2VE

def get_pure_fluid_saturation_line(p_init=1.0e5,maxDeltaP=1.0e5):
    """Get the saturation line

    pytp.tp.init() must have been called first.

    Arguments
        p_init      Initial pressure
        maxDeltaP   Pressure step between each point on saturation line

    Returns
        Temperature values (K)
        Pressure values (Pa)
        Specific gas volumes (m3/mol)
        Specific liquid volumes (m3/mol)
    """
    Tvals, pvals, nvals \
        = tp.pure_fluid_saturation_line(p_init,maxDeltaP)
    Tvals = Tvals[0:nvals]
    pvals = pvals[0:nvals]

    # Setting feed composition:
    z = np.array([1.0])

    vgvals = np.zeros_like(Tvals)
    vlvals = np.zeros_like(Tvals)
    # Mapping specific volume
    for i in range(nvals):
        vgvals[i] = tp.specific_volume_no_derivs(Tvals[i],pvals[i],z,const.VAPPH)
        vlvals[i] = tp.specific_volume_no_derivs(Tvals[i],pvals[i],z,const.LIQPH)

    return Tvals, pvals, vgvals, vlvals
