#include "cppThermopack/thermo.h"
#include "cppThermopack/pcsaft.h"
#include <vector>
#include <iostream>

int main(){
    Pcsaft pcs("C3,NC6,ETOH"); // Initialize pcsaft for propane/hexane/ethanol mixture

    std::vector<double> z = {0.2, 0.5, 0.3}; // Total composition

    std::vector<double> crit_tvp = pcs.critical(z);

    double T{0.6 * crit_tvp[0]}; // Ensuring that we compute at some sub-critical temperature (0.6 * Tc)
    std::cout << "Computing dew- and bubble pressure at " << T << " K:\n";
    double p_dew = pcs.dew_pressure(T, z).p;
    double p_bub = pcs.bubble_pressure(T, z).p;

    std::cout << pcs.dew_pressure(T, z) << "\n";
    std::cout << pcs.bubble_pressure(T, z) << "\n";

    double p = 0.5 * (p_dew + p_bub);
    std::cout << "\nComputing dew- and bubble temperature at " << p / 1e5 << " bar\n";
    std::cout << pcs.dew_temperature(p, z) << "\n";
    std::cout << pcs.bubble_temperature(p, z) << "\n";

    return 0;
}