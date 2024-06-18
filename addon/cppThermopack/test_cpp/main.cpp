#include <iostream>
#include <vector>
#include "cubic.h"
#include "saftvrmie.h"

// Note: The structs `Property` and `VectorProperty` are found in utils.h, which is implicitly included in all headers
// through thermo.h
// Also note: Order of bools for computing differentials follows the same convention as pycThermoPack (i.e. order of bools is equal to order of input-parameters)

int main(){
    PengRobinson pr("N2,O2");
    double T = 300.;
    double V = 1e3;
    vector1d n = {1., 2.};
    Property p = pr.pressure_tv(T, V, n, false, true, true); // Computing property and differentials
    VectorProperty mu = pr.chemical_potential_tv(T, V, n, true, false, true); // Computing vector-valued property and differentials
    std::cout << "P : \n" << p << std::endl;
    std::cout << "mu :\n" << mu;

    double pres = pr.pressure_tv(T, V, n); // Implicit conversion to double
    std::cout << "Pressures are equal : " << ((pres == p.value()) ? "true" : "false") << std::endl;
    double dpdv = p.dv(); // Accessing differentials

    std::vector<double> chem_pot = pr.chemical_potential_tv(T, V, n); // Implicit conversion to std::vector
    std::vector<double> dmudt = mu.dt(); // Accessing the differentials
    std::vector<std::vector<double>> dmudn = mu.dn();

    Saftvrmie svrm("AR,KR");
    V = 1e-3;
    std::cout << "SVRM chemical potential : \n" << svrm.chemical_potential_tv(T, V, n, false, true) << std::endl;

    return 0;
}