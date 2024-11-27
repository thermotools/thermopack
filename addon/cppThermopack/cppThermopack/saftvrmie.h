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

    void set_pure_fluid_param(size_t ci, double ms_, double sigma_, double eps_div_k_, double la, double lr){
        size_t i = ci - 1;
        ms[i] = ms_; sigma[i] = sigma_, eps_div_k[i] = eps_div_k_; lambda_a[i] = la; lambda_r[i] = lr;
        reset_pure_fluid_param(ci);
    }

    void set_sigma(size_t ci, double sigma_){
        sigma[ci - 1] = sigma_;
        reset_pure_fluid_param(ci);
    }

    void set_eps_div_k(size_t ci, double eps_div_k_){
        eps_div_k[ci - 1] = eps_div_k_;
        reset_pure_fluid_param(ci);
    }

    void set_la(size_t ci, double la){
        lambda_a[ci - 1] = la;
        reset_pure_fluid_param(ci);
    }

    void set_lr(size_t ci, double lr){
        lambda_r[ci - 1] = lr;
        reset_pure_fluid_param(ci);
    }

protected:
    std::vector<double> lambda_a, lambda_r;

    explicit Saftvrmie(std::string comps, bool is_inheriting) // Used by inheriting classes to skip saftvrmie eoslibinit call
    : Saft(comps)
    {
        lambda_a = std::vector<double>(nc);
        lambda_r = std::vector<double>(nc);
    }

    void init_params(){
        activate();
        size_t ci; // Fortran component index
        for (size_t i = 0; i < nc; i++){
            ci = i + 1;
            get_export_name(saftvrmie_containers, get_saftvrmie_pure_fluid_param)(&ci, &(ms[i]), &(sigma[i]), &(eps_div_k[i]), &(lambda_a[i]), &(lambda_r[i]));
        }
    }

private:
    void reset_pure_fluid_param(size_t ci){
        activate();
        size_t i = ci - 1;
        get_export_name(saftvrmie_containers, set_saftvrmie_pure_fluid_param)(&ci, &(ms[i]), &(sigma[i]), &(eps_div_k[i]), &(lambda_a[i]), &(lambda_r[i]));
    }

};
