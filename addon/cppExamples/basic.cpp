#include <iostream>
#include <vector>
#include <cppThermopack/cubic.h>
#include <cppThermopack/saftvrmie.h>

// Note: The structs `Property` and `VectorProperty` are found in utils.h, which is implicitly included in all headers
// through thermo.h
// Also note: Order of bools for computing differentials follows the same convention as pycThermoPack (i.e. order of bools is equal to order of input-parameters)
// In general: The method signatures are kept consistent with pycThermopack, such that the pycThermopack docs can serve as a coarse lookup for the methods in the cpp wrapper
//             methods that return a single value (i.e. pressure_tv, specific_volume, etc.) will return a `Property`, which has an implicit conversion to `double`, while
//             methods that return several values (i.e. chemical_potential_tv) will return a `VectorProperty`, which has an implicit conversion to std::vector<double>

int main(){
    PengRobinson pr("N2,O2"); // Initialise Peng-Robinson EoS
    double T = 300.;
    double V = 1e3;
    vector1d n = {1., 2.};

    double pres = pr.pressure_tv(T, V, n); // Compute pressure (implicit conversion to double)
    Property p = pr.pressure_tv(T, V, n, false, true, true); // Computing property and differentials
    std::cout << "P : \n" << p << std::endl;
    std::cout << "Pressures are equal : " << ((pres == p.value()) ? "true" : "false") << std::endl;
    double dpdv = p.dv(); // Accessing differentials
    std::cout << "Accessed dpdv : " << dpdv << std::endl;

    std::vector<double> chem_pot = pr.chemical_potential_tv(T, V, n); // Compute chemical potential (implicit conversion to std::vector)
    VectorProperty mu = pr.chemical_potential_tv(T, V, n, true, false, true); // Computing vector-valued property and differentials
    std::cout << "mu :\n" << mu;
    std::vector<double> dmudt = mu.dt(); // Accessing the differentials
    std::vector<std::vector<double>> dmudn = mu.dn();

    Saftvrmie svrm("AR,KR"); // Initialise SAFT-VR Mie EoS
    V = 1e-3;
    std::cout << "SVRM chemical potential : \n" << svrm.chemical_potential_tv(T, V, n, false, true) << std::endl;


    Saftvrmie svrm_c1("C1");
    p = 1e6;
    T = 120.;
    double vl = svrm_c1.specific_volume(T, p, {1}, Phase::liq);
    double vg = svrm_c1.specific_volume(T, p, {1}, Phase::vap);
    std::cout << "At T = " << T << " K, p = " << p / 1e5 << " bar, specific volume of methane is : " << vl << " / " << vg << std::endl;
    return 0;
}