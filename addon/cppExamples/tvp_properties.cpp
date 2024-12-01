/*
TVP-interface demo

The TVP-interface methods are used when you want to compute a property at specified (T, V, n), but want derivatives at specified (T, p, n).
These are convenient if you want e.g. the isobaric heat capacity at some temperature and density, as it saves you from evaluating the pressure,
and will save some runtime as opposed to solving for the density if you specify the pressure.
*/
#include <cppThermopack/ljs.h>

int main(){
    LJs_bh eos; // Lennard-Jones Splined, Barker Henderson model

    double T = 300; // Kelvin
    double V = 1e-3; // m3 / mol
    std::vector<double> n = {2.}; // mol
    Property h = eos.enthalpy_tvp(T, V, n, true, true, true);
    std::cout << "Computed enthalpy : " << h << std::endl;;
    std::cout << "Isobaric heat capacity is : " << h.dt() << std::endl;

    Property s = eos.entropy_tvp(T, V, n, true, true, true);
    std::cout << "\nEntropy : " << s << std::endl;

    VectorProperty fug = eos.fugacity_tvp(T, V, n, true, true, true);
    std::cout << "\nFugacity : " << fug << std::endl;
}