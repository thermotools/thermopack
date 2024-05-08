import numpy as np

# Avogadros number and gas constant
NA = 6.02214076e23
KB = 1.380650524e-23
RGAS = R = NA*KB

def calc_reduced_T(Ta, eps_div_kb):
    """ Calculate reduced temperature
    """
    Tstar = np.zeros_like(Ta)
    Tstar = Ta / eps_div_kb
    return Tstar

def calc_reduced_rho(rhoa, sigma):
    """ Calculate reduced density
    """
    rhoStar = np.zeros_like(rhoa)
    rhoStar = sigma**3 * NA * rhoa
    return rhoStar

def calc_real_T(Tstar, eps_div_kb):
    """ Calculate temperature from reduced temperature
    """
    Ta = np.zeros_like(Tstar)
    Ta = Tstar * eps_div_kb
    return Ta

def calc_real_rho(rhoStar, sigma):
    """ Calculate density from reduced density
    """
    rhoa = np.zeros_like(rhoStar)
    rhoa = rhoStar/(sigma**3 * NA)
    return rhoa

def calc_reduced_entropy(s):
    """ Calculate reduced entropy
    """
    sStar = np.zeros_like(s)
    sStar = s/RGAS
    return sStar

def calc_reduced_P(Pa, eps_div_kb, sigma):
    """ Calculate reduced pressure
    """
    Pstar = np.zeros_like(Pa)
    Pstar = Pa*sigma**3 / eps_div_kb / KB
    return Pstar

def calc_real_P(Pstar, eps_div_kb, sigma):
    """ Calculate reduced pressure
    """
    Pa = np.zeros_like(Pstar)
    Pa = Pstar * eps_div_kb * KB / sigma**3
    return Pa

def calc_reduced_heat_capacity(C):
    """ Calculate reduced heat capacity
    """
    CStar = np.zeros_like(C)
    CStar = C/RGAS
    return CStar

def calc_reduced_energy(e, eps_div_kb):
    """ Calculate reduced energy (J/mol)
    """
    eStar = np.zeros_like(e)
    eStar = e/(eps_div_kb * RGAS)
    return eStar

class EpsilonSigmaUnits:
    """Convenience class to ease conversion."""

    def __init__(self, eps_div_kb=None, sigma=None):
        """
         eps_div_kb: Energy scale (K)
         sigma: length scale (m)
        """
        self.eps = eps_div_kb
        self.sigma = sigma

    # Reducing functions
    def redT(self, Ta):
        """ Calculate reduced temperature"""
        return calc_reduced_T(Ta, self.eps)
        
    def redrho(self, rhoa):
        """ Calculate reduced density"""
        return calc_reduced_rho(rhoa, self.sigma)

    def redP(self, Pa):
        """ Calculate reduced pressure"""
        return calc_reduced_P(Pa, self.eps, self.sigma)

    def redPT(self, dPdT):
        """ Calculate reduced isochoric pressure derivative dPdT"""
        return self.redP(dPdT)/self.redT(1.0)


    def redentropy(self, s):
        """ Calculate reduced entropy"""
        return calc_reduced_entropy(s)
        
    def redheat_capacity(self, C):
        """ Calculate reduced heat capacity"""
        return calc_reduced_heat_capacity(C)

    def redenergy(self, e):
        """ Calculate reduced energy (J/mol)"""
        return calc_reduced_energy(e, self.eps)

    # Antireducing functions
    def realT(self, Tstar):
        """ Calculate temperature from reduced temperature"""
        return calc_real_T(Tstar, self.eps)

    def realP(self, Pstar):
        """ Calculate reduced pressure"""
        return calc_real_P(Pstar, self.eps, self.sigma)

    def realrho(self, rhoStar):
        """ Calculate density from reduced density"""
        return calc_real_rho(rhoStar, self.sigma)