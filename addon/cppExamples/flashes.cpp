#include <cppThermopack/cubic.h>
#include <vector>
#include <iostream>

int main(){
    SoaveRedlichKwong srk("PROP1OL,NC6");
    std::cout << "Demonstrating flashes with SRK ...\n";
    
    double T1{348.}, p1{1e5};
    std::vector<double> z = {0.4, 0.6};
    FlashResult flsh = srk.two_phase_tpflash(T1, p1, z);
    std::cout << "Initial state is \n" << flsh;

    double h = srk.enthalpy(T1, p1, flsh.x, srk.LIQPH) * flsh.betaL 
                + srk.enthalpy(T1, p1, flsh.y, srk.VAPPH) * flsh.betaV;
    std::cout << "Molar enthalpy at (T, p) = (" << T1 << ", " << p1 << ") : " << h << " J / mol\n";
    
    double p2 = 5e3;
    std::cout << "\nIsenthalpic decompression to " << p2 / 1e5 << " bar ...\n";
    flsh = srk.two_phase_phflash(p2, h, z);
    std::cout << "After isenthalpic decompression: \n" << flsh;

    double s = srk.entropy(flsh.T, flsh.p, flsh.x, srk.LIQPH) * flsh.betaL
                + srk.entropy(flsh.T, flsh.p, flsh.y, srk.VAPPH) * flsh.betaV;
    std::cout << "Molar entropy after decompression : " << s << " J / mol K\n";
    std::cout << "\nIsentropic compression to 2 bar ...\n";
    flsh = srk.two_phase_psflash(2e5, s, z);
    std::cout << "After isentropic compression : \n" << flsh;
    
    double v = srk.specific_volume(flsh.T, flsh.p, flsh.x, srk.LIQPH) * flsh.betaL
                + srk.specific_volume(flsh.T, flsh.p, flsh.y, srk.VAPPH) * flsh.betaV;
    double u = srk.internal_energy_tv(flsh.T, v, flsh.z);
    std::cout << "Internal energy after isentropic compression : " << u << " J / mol\n";
    
    double q = 30.; // J / mol, Heat to add
    std::cout << "\nIsochoric heating with " << q << " J / mol heat ...\n";
    flsh = srk.two_phase_uvflash(u + q, v, z);
    std::cout << "After isochoric heating : \n" << flsh;
    return 0;
}