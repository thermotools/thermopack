"""
Test module to ensure that previously resolved issues remain resolved. Test functions are named test_issue_XXX, where XXX is
the number of the issue, or test_pr_YYY, where YYY is the number of the relevant PR if there is no issue to reference.

When an issue is resolved, a test with code that triggered it before resolution should be added here.
"""
from tools import ALL_CUBIC, ALL_SAFT, check_eq
from tools import saftvrmie, lee_kesler
from thermopack.platform_specifics import DIFFERENTIAL_RETURN_MODE
from pytest import mark
import warnings

@mark.parametrize('eos', ALL_CUBIC)
def test_pr_114(eos):
    T = 300
    eos = eos('ETOH,C2')
    if DIFFERENTIAL_RETURN_MODE == 'v3':
        h1 = eos.idealenthalpysingle(1, T)
    elif DIFFERENTIAL_RETURN_MODE == 'v2':
        h1, = eos.idealenthalpysingle(1, T)
    
    h2, dh = eos.idealenthalpysingle(1, T, dhdt=True)
    assert check_eq(h1, h2)

@mark.parametrize('EOS', [*ALL_CUBIC, saftvrmie, pc])
def test_pr_191(EOS):
    if DIFFERENTIAL_RETURN_MODE == 'v2':
        warnings.warn('Test only implemented for V3')
        return
    T = 300
    p = 1e5
    comps = ['PSEUDO', 'LJF']
    for comp in comps:
        if (EOS in ALL_SAFT) and (comp == 'PSEUDO'): continue
        eos = EOS(comp)
        if isinstance(eos, lee_kesler): continue
        
        v = eos.specific_volume(T, p, [1], phase=eos.VAPPH)
        _, dh_tp = eos.enthalpy(T, p, [1], phase=eos.VAPPH, dhdt=True)
        _, dh_tv = eos.enthalpy_tvp(T, v, [1], dhdt=True)
        assert check_eq(dh_tp.dT, dh_tv.dT)