#include <cppThermopack/saftvrmie.h>
#include <iostream>
#include <vector>

int main(){
    std::string comps = "C3,C1";
    Saftvrmie svrm(comps);
    std::cout << "Initialized SAFT-VR Mie for " << comps << "\n";
    double T = 300;
    double p = 1e6;
    std::vector<double> z = {0.7, 0.3};

    double v = svrm.specific_volume(T, p, z, svrm.VAPPH);
    std::cout << "Vapour phase Specific volume is : " << v << " m3 / mol\n";

    Property rho = svrm.molar_density(T, p, z, svrm.LIQPH, true);
    std::cout << "Liquid phase density derivative wrt. temperature (drho / dT) : " << rho.dt() << " mol / m3 K\n";

    Property h = svrm.enthalpy(T, p, z, svrm.VAPPH, true);
    std::cout << "Vapour phase, specific enthalpy : " << static_cast<double>(h) << " J / mol, " << "Heat capacity : " << h.dt() << " J / mol K\n";
    std::cout << "Liquid phase, specific enthalpy : " << static_cast<double>(svrm.enthalpy(T, p, z, svrm.LIQPH)) << " J / mol\n";

    Property s_vap = svrm.entropy(T, p, z, svrm.VAPPH, false, false, true);
    std::cout << "Vapour phase partial molar entropies : " << s_vap.dn()[0] << ", " << s_vap.dn()[0] << " J / mol^2 K\n";
    Property s_liq = svrm.entropy(T, p, z, svrm.LIQPH, false, false, true);
    std::cout << "Liquid phase partial molar entropies : " << s_liq.dn()[0] << ", " << s_liq.dn()[1] << " J / mol^2 K\n";

    return 0;
}