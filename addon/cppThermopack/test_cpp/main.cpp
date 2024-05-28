#include <iostream>
#include "cubic.h"

int main(){
    PengRobinson pr("N2");
    double T = 300.;
    double V = 1e-3;
    vector1d n(1, 2.);
    std::cout << "P : " << pr.pressure_tv(T, V, n) << std::endl;
}