#include "../cpp_wrapper/NotIdGas.h"
#include "../cpp_wrapper/Variant1.h"
#include "../cpp_wrapper/Variant2.h"
#include <iostream>
#include <vector>

int main(){
    float T = 300.;
    float V = 1.;
    float n[2] = {1., 2.};
    float Fres, Ft, Fv, p_ideal, p_real;
    float Fn[2];
    float* Ft_p, *Fv_p, *Fn_p, *p_ideal_p, *p_real_p;
    Ft_p = &Ft; Fv_p = &Fv;


    NotIdGas nid = NotIdGas(0, 2, 100., 50.);
    nid.Fres(T, V, &n[0], &Fres, &Ft_p, &Fv_p, &Fn_p);
    std::cout << "Not Ideal Fres is : " << Fres << std::endl;
    nid.Fideal(T, V, &n[0], &Fres, &Ft_p, &p_ideal_p, &Fn_p);
    std::cout << "Ideal pressure at T, V = " << T << " " << V << " is : " << -p_ideal << std::endl;
    nid.pressure(T, V, &n[0], &p_real);
    std::cout << "Not Ideal pressure is : " << p_real << std::endl;

    Variant1 v1 = Variant1(1, 2, 100., 50., 2, 5.);
    v1.Fres(T, V, &n[0], &Fres, &Ft_p, &Fv_p, &Fn_p);
    std::cout << "\nVariant1 Fres is : " << Fres << std::endl;
    v1.pressure(T, V, &n[0], &p_real);
    std::cout << "Variant1 pressure is : " << p_real << std::endl;

    Variant2 v2 = Variant2(3, 2, 100., 50., 2, 5.);
    v2.Fres(T, V, &n[0], &Fres, &Ft_p, &Fv_p, &Fn_p);
    std::cout << "\nVariant2 Fres is : " << Fres << std::endl;
    v2.pressure(T, V, &n[0], &p_real);
    std::cout << "Variant2 pressure is : " << p_real << std::endl;

    return 0;
}