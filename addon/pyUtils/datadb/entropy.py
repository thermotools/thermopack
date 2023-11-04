
RGAS = 8.31446261815324

class entropy_references(object):
    """Reference entropies for Gibbs free energy of formation
    """
    def __init__(self, fluidlist):
        """
        Arguments:
        fluidlist (list): List of fluids
        """
        self.Tref = 298.15
        # Entropy references at 1BAR from HSC Chemistry 9
        self.element_references = {}
        self.element_references["C"] = 5.740
        self.element_references["F"] = 202.789*0.5
        self.element_references["S"] = 32.070
        self.element_references["SI"] = 18.820
        # Map from fluid files - assuming 1BAR reference values
        self.element_references["H"] = 0.5*fluidlist["H2"].fluid["reference_state"]["entropy"]
        self.element_references["CL"] = 0.5*fluidlist["CL2"].fluid["reference_state"]["entropy"]
        self.element_references["D"] = 0.5*fluidlist["D2"].fluid["reference_state"]["entropy"]
        self.element_references["KR"] = fluidlist["KR"].fluid["reference_state"]["entropy"]
        self.element_references["HE"] = fluidlist["HE"].fluid["reference_state"]["entropy"]
        self.element_references["NE"] = fluidlist["NE"].fluid["reference_state"]["entropy"]
        self.element_references["O"] = 0.5*fluidlist["O2"].fluid["reference_state"]["entropy"]
        self.element_references["N"] = 0.5*fluidlist["N2"].fluid["reference_state"]["entropy"]
        self.element_references["AR"] = fluidlist["AR"].fluid["reference_state"]["entropy"]
        self.element_references["XE"] = fluidlist["XE"].fluid["reference_state"]["entropy"]

    def calc_gibbs_free_energy_of_formation(self, elements, dHf, S0):
        """
        Arguments:
            elements (dict): Dictionary of elements
            dHf (float): Formation enthalpy
            S0 (float): Fluid entropy reference at 1Bar
        """
        if dHf > 10000000000.0: # dHf not available
            return 100000000000.0
        dS0 = S0
        for key in elements:
            dS0 -= elements[key]*self.element_references[key]
        dGf = dHf - self.Tref*dS0
        if abs(dGf) < 1.0e-10:
            dGf = 0.0 # Flush to zero for reference states
        return dGf
