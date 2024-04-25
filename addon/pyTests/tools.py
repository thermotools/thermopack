from thermopack.cubic import SoaveRedlichKwong, SchmidtWensel, PengRobinson, PengRobinson78, PatelTeja, VanDerWaals
from thermopack.saftvrmie import saftvrmie
from thermopack.pcsaft import PCP_SAFT, SPC_SAFT
from thermopack.saftvrqmie import saftvrqmie
from thermopack.cpa import SRK_CPA, PR_CPA
from thermopack.quantum_cubic import qcubic
from thermopack.extended_csp import ext_csp
from thermopack.lee_kesler import lee_kesler
from thermopack.ljs_bh import ljs_bh
from thermopack.ljs_wca import ljs_uv, ljs_wca, ljs_wca_base
from thermopack.pets import pets
from thermopack.tcPR import tcPR

ALL_CUBIC = [SoaveRedlichKwong, SchmidtWensel, PengRobinson, PengRobinson78, PatelTeja, VanDerWaals, tcPR, lee_kesler,
             qcubic]
ALL_SAFT = [saftvrmie, saftvrqmie, PCP_SAFT, SPC_SAFT]

# List of all eos that can be initialized with eos(comps), for easy use with pytest.mark.parametrize
ALL_NO_CPA = [*ALL_CUBIC, *ALL_SAFT, qcubic]
ALL_COMP_EOS = [*ALL_NO_CPA, SRK_CPA, PR_CPA]

def check_eq(a, b, tol=1e-10):
    return abs(a - b) < tol