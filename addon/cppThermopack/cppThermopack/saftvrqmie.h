#pragma once
#include <string>
#include "dllmacros.h"
#include "saftvrmie.h"
#include "saft.h"

extern "C" {
    void get_export_name(eoslibinit, init_quantum_saftvrmie)(char* comps, int* fh_order, int* additive_hard_sphere, char* parameter_ref, size_t comps_strlen, size_t ref_strlen);
    void get_export_name(saftvrmie_containers, get_feynman_hibbs_order)(size_t* ci, int* fh_order, int* fh_hs);
}

class Saftvrqmie : public Saftvrmie {
public:
    Saftvrqmie(std::string comps, int feynman_hibbs_order=1, bool additive_hard_sphere=false, std::string parameter_reference="Default", double minimum_temperature=20.)
	: Saftvrmie(comps, true)
    {
        activate();
        int additive_hs = (additive_hard_sphere) ? true_int : 0;
        get_export_name(eoslibinit, init_quantum_saftvrmie)(comps.data(), &feynman_hibbs_order, &additive_hs,
                                                            parameter_reference.data(), comps.size(), parameter_reference.size());
        lambda_a = std::vector<double>(nc);
        lambda_r = std::vector<double>(nc);
        init_params();
        set_tmin(minimum_temperature);
    }

    double get_sigma_eff(size_t i, double T){return get_sigma_eff(i, i, T);}
    double get_sigma_eff(size_t i, size_t j, double T){
        activate();
        double sigma_eff;
        get_export_name(saft_interface, sigma_eff_ij)(&i, &j, &T, &sigma_eff);
        return sigma_eff;
    }

    double get_epsilon_eff(size_t i, double T){return get_epsilon_eff(i, i, T);}
    double get_epsilon_eff(size_t i, size_t j, double T){
        activate();
        double epsilon_eff;
        get_export_name(saft_interface, epsilon_eff_ij)(&i, &j, &T, &epsilon_eff);
        return epsilon_eff;
    }

    int get_feynman_hibbs_order(size_t ci){
        activate();
        int fh, fh_hs;
        get_export_name(saftvrmie_containers, get_feynman_hibbs_order)(&ci, &fh, &fh_hs);
        return fh;
    }
};