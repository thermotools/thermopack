#include <iostream>
#include <vector>
#include <cppThermopack/cubic.h>
#include <cppThermopack/saftvrmie.h>
#include <cppThermopack/ideal.h>
#include <cppThermopack/pcsaft.h>
#include <cppThermopack/ljs.h>

// Note: The structs `Property` and `VectorProperty` are found in utils.h, which is included in all the cppThermopack headers through thermo.h
// Also note: Order of bools for computing differentials follows the same convention as pycThermoPack (i.e. order of bools is equal to order of input-parameters)
// In general: The method signatures are kept consistent with pycThermopack, such that the pycThermopack docs can serve as a coarse lookup for the methods in the cpp wrapper.
//             - Methods that return a single value (i.e. pressure_tv, specific_volume, etc.) will return a `Property`, which has an implicit conversion to `double`
//             - Methods that return several values (i.e. chemical_potential_tv) will return a `VectorProperty`, which has an implicit conversion to std::vector<double>
//             - The `Property` and `VectorProperty` objects are used to hold the value of a property, and also it's differentials, which are accessed through the methods
//               [Vector]Property::dv(), ::dn(), ::dt(), ::dp()
//               Attempting to access a differential that has not been computed causes an std::runtime_error to be thrown.

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
    double vl = svrm_c1.specific_volume(T, p, {1}, Phase::liq); // Implicit converstion from `Property` to `double`
    double vg = svrm_c1.specific_volume(T, p, {1}, Phase::vap);
    std::cout << "At T = " << T << " K, p = " << p / 1e5 << " bar, specific volume of methane is : " << vl << " / " << vg << std::endl;
    
    Pcsaft pcs1("NC6,NC12"); // Initialise PC-SAFT eos (simplified, non-polar)
    std::cout << "PC-SAFT specific_volume : " << pcs1.specific_volume(T, p, {0.3, 0.7}, Phase::liq) << std::endl;
    Spcsaft spcs("CO2,NH3"); // Init Simplified PC-SAFT eos
    std::cout << "SPC-SAFT specific_volume : " << spcs.specific_volume(T, p, {0.3, 0.7}, Phase::liq) << std::endl;
    Pcpsaft pcps("H2O,NH3"); // Init Polar PC-SAFT eos
    std::cout << "PCP-SAFT specific_volume : " << pcps.specific_volume(T, p, {0.3, 0.7}, Phase::liq) << std::endl;
    
    Ideal id("AR,KR"); // Ideal gas law for
    std::cout << "Ideal volume : " << id.specific_volume(T, p, {0.3, 0.7}, Phase::vap) << std::endl;
    
    LJs_bh ljsbh; // LJ Splined Barker-Henderson model with default parameters
    std::cout << "LJs-BH default parameters : " << ljsbh.get_sigma(0) << " / " << ljsbh.get_eps_div_k(0) << std::endl;
    LJs_wca ljswca; // LJ Splined WCA model with default parameters
    std::cout << "LJs-WCA default parameters : " << ljswca.get_sigma(0) << " / " << ljswca.get_eps_div_k(0) << std::endl;
    LJs_uv ljsuv; // LJ Splined UV-Theory with default parameters
    std::cout << "LJs-UV default parameters : " << ljsuv.get_sigma(0) << " / " << ljsuv.get_eps_div_k(0) << std::endl;

    double sigma = 3e-10;
    double eps_div_k = 150.;
    LJs_bh ljsbh2(sigma, eps_div_k); // LJ Splined Barker-Henderson model with specified parameters
    std::cout << "LJs-BH with parameters : " << ljsbh2.get_sigma(0) << " / " << ljsbh2.get_eps_div_k(0) << std::endl;
    LJs_wca ljswca2(sigma, eps_div_k); // LJ Splined WCA model with specified parameters
    std::cout << "LJs-WCA with parameters : " << ljswca2.get_sigma(0) << " / " << ljswca2.get_eps_div_k(0) << std::endl;
    LJs_uv ljsuv2(sigma, eps_div_k); // LJ Splined UV-Theory with specified parameters
    std::cout << "LJs-UV with parameters : " << ljsuv2.get_sigma(0) << " / " << ljsuv2.get_eps_div_k(0) << std::endl;
    return 0;
}