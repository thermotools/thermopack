/*
    Demonstrate computation of properties in the TP-interface, i.e. functions of (T, p, z, phase).
    
    Note that the `Property` class has an implicit conversion to `double`, so if no derivatives are needed, 
    a `Property` can be returned to a `double` (as demonstrated for specific_volume). A property can also be
    `static_cast<double>`'d, as demonstrated for enthalpy. 

    Otherwise the `value` of a property can be extracted with the Property::value() method.

    Derivative flags (the booleans in the example) are given in the order
        dt, dp, dn
    with the default value `false`.
*/
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
    std::cout << "Vapour phase Specific volume : " << v << " m3 / mol\n\n";

    double density = svrm.molar_density(T, p, z, svrm.LIQPH); // No need for `Property` - we dont want derivatives
    std::cout << "Liquid phase density : " << density << " mol / m3\n";
    Property rho = svrm.molar_density(T, p, z, svrm.LIQPH, true); // Casting to `double` will discard the derivative, so here we need `Property`
    std::cout << "Liquid phase density derivative wrt. temperature (drho / dT) : " << rho.dt() << " mol / m3 K\n\n";

    Property h = svrm.enthalpy(T, p, z, svrm.VAPPH, true, false, true); // Compute enthaly and temperature and mole number derivatives
    std::cout << "Vapour phase, specific enthalpy : " << h.value() << " J / mol\n";
    std::cout << "Vapour phase heat capacity : "<< h.dt() << " J / mol K\n";
    std::cout << "Vapour phase partial molar enthalpies : " << h.dn()[0] << ", " << h.dn()[1] << " J / mol\n";
    std::cout << "Liquid phase, specific enthalpy : " << static_cast<double>(svrm.enthalpy(T, p, z, svrm.LIQPH)) << " J / mol\n\n"; // We can cast the output to a double

    Property s_vap = svrm.entropy(T, p, z, svrm.VAPPH, false, true, false); // Compute entropy and pressure derivative
    double dsdp = s_vap.dp(); // Extract pressure derivative
    std::cout << "Vapour phase entropy derivative wrt. pressure (ds/dp) : " << dsdp << " J / mol Pa K\n";
    Property s_liq = svrm.entropy(T, p, z, svrm.LIQPH, false, false, true); // Compute entropy and mole number derivatives
    std::vector<double> dsdn = s_liq.dn(); // Extract mole number derivatives
    std::cout << "Liquid phase partial molar entropies : " << dsdn[0] << ", " << dsdn[1] << " J / mol K\n";

    return 0;
}