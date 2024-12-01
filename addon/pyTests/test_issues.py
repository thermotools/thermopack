"""
Test module to ensure that previously resolved issues remain resolved. Test functions are named test_issue_XXX, where XXX is
the number of the issue, or test_pr_YYY, where YYY is the number of the relevant PR if there is no issue to reference.

When an issue is resolved, a test with code that triggered it before resolution should be added here.
"""
from tools import ALL_CUBIC, check_eq
from thermopack.platform_specifics import DIFFERENTIAL_RETURN_MODE
from pytest import mark

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