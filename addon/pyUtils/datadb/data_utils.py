import numpy as np

I = "  "
N_TAGS_PER_LINE = 5


def sci_print_float(x, p=None): return np.format_float_scientific(
    x, precision=p, trim=".")


def print_float(x, p=None): return np.format_float_positional(
    x, precision=p, trim=".")


def saft_eos_to_idx(eos):
    mod_eos = eos.replace(" ", "").replace("-", "").replace("_", "").upper()
    if (mod_eos == "SAFTVRMIE"):
        eosidx = "eosSAFT_VR_MIE"
    elif (mod_eos == "PCSAFT"):
        eosidx = "eosPC_SAFT"
    elif (mod_eos == "SPCSAFT"):
        eosidx = "eosSPC_SAFT"
    else:
        eosidx = "eosUNKNOWN"
    return eosidx


def get_assoc_scheme_parameter(assoc_scheme):
    """
        assoc_scheme - 3B, 3C, etc.
        Output:
        param - Thermopack parameter
    """
    if assoc_scheme.upper() == "NONE":
        param = "no_assoc"
    else:
        param = "assoc_scheme_{}".format(assoc_scheme)
    return param

def get_alpha_index_parameter(alpha_corr):
    """
        alpha_corr - Classic, TWU, MC, etc.
        Output:
        param - Thermopack parameter defined in cubic_eos.f90
    """
    return f"cbAlpha{alpha_corr}Idx"

def get_mix_model_parameter(model):
    """
        model - ARITHMETIC or GEOMETRIC
        Output:
        param - Thermopack parameter defined in assocschemeutils.f90
    """
    assert model == "GEOMETRIC" or model == "ARITHMETIC"
    return "ariComb" if model == "ARITHMETIC" else "geoComb"
