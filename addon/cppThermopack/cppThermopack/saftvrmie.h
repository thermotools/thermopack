#pragma once
#include <string>
#include "dllmacros.h"
#include "saft.h"

extern "C" {
    void get_export_name(eoslibinit, init_saftvrmie)(char* comps, char* parameter_ref, size_t comps_strlen, size_t ref_strlen);

    void get_export_name(saftvrmie_containers, get_saftvrmie_pure_fluid_param)(size_t* ci, double* m, double* sigma, double* eps_div_k, double* la, double* lr);
    void get_export_name(saftvrmie_containers, set_saftvrmie_pure_fluid_param)(size_t* ci, double* m, double* sigma, double* eps_div_k, double* la, double* lr);
}

class Saftvrmie : public Saft {
public:
    Saftvrmie(std::string comps, std::string parameter_reference="Default")
	: Saft(comps)
    {
	activate();
	get_export_name(eoslibinit, init_saftvrmie)(comps.data(), parameter_reference.data(), comps.size(), parameter_reference.size());
    lambda_a = std::vector<double>(nc);
    lambda_r = std::vector<double>(nc);
    init_params();
    }

protected:
    void init_params(){
        activate();
        size_t ci; // Fortran component index
        for (size_t i = 0; i < nc; i++){
            ci = i + 1;
            get_export_name(saftvrmie_containers, get_saftvrmie_pure_fluid_param)(&ci, &(ms[i]), &(sigma[i]), &(eps_div_k[i]), &(lambda_a[i]), &(lambda_r[i]));
        }
    }

private:
    std::vector<double> lambda_a, lambda_r;

};
