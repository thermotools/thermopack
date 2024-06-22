#include <cppThermopack/pcsaft.h>
#include <iostream>
#include <vector>

// A couple convenience methods
inline std::vector<double> operator*(double d, const std::vector<double>& v){
    std::vector<double> v_out{v};
    for (double& vi : v_out) vi *= d;
    return v_out;
}

inline std::vector<double> operator*(const std::vector<double>& v, double d){
    return d * v;
}

inline std::ostream& operator<<(std::ostream& os, const std::vector<double> v){
    for (auto d : v) os << d << ", ";
    return os;
}

// This is the example
int main(){
    std::string comps = "C1,ETOH,PROP1OL";
    Pcsaft pcs(comps);
    std::cout << "Initialized PC-SAFT for components " << comps << "\n";

    double T{400.}, V{0.3};
    double n_tot = 15;
    std::vector<double> z = {0.2, 0.5, 0.5};
    std::vector<double> n = n_tot * z;

    std::cout << "Computing TV properties at T = " << T << " K, V = " << V << " m^3, n_tot = " << n_tot << " mol ... \n\n";

    std::cout << "Pressure : " << pcs.pressure_tv(T, V, n) / 1e5 << " bar\n";
    std::cout << "Internal energy : " << pcs.internal_energy_tv(T, V, n) / 1e3 << " kJ\n";
    std::cout << "Entropy : " << pcs.entropy_tv(T, V, n) << " J / K\n";
    std::cout << "Enthalpy : " << pcs.enthalpy_tv(T, V, n) / 1e3 << " kJ\n";
    std::cout << "Helmholtz energy : " << pcs.helmholtz_tv(T, V, n) / 1e3 << " kJ\n";
    std::cout << "Chemical potential : " << pcs.chemical_potential_tv(T, V, n) << " J / mol\n";

    std::cout << "\nComputing derivatives ...\n";
    // Bools mark what derivatives to compute. 
    // Order is dt, dv, dn
    Property H = pcs.enthalpy_tv(T, V, n, true, true, true);
    std::cout << "dHdT : " << H.dt() << " J / K, dHdV : " << H.dv() << " J / m^3, dHdn_i : " << H.dn() << "\n";
    
    Property A = pcs.helmholtz_tv(T, V, n, false, false, true);
    std::cout << "dAdn : " << A.dn() << "\n";
    VectorProperty mu = pcs.chemical_potential_tv(T, V, n, true, false, true);
    std::cout << "mu   : " << mu.value() << "\n";
    std::cout << "Chemical potential with derivatives : \n" << mu;


    std::vector<std::vector<double>> dmudn = mu.dn();
    return 0;
}
