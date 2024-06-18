#pragma once
#include "dllmacros.h"
#include "utils.h"
#include <vector>
#include <algorithm>

using vector1d = std::vector<double>;
using vector2d = std::vector<vector1d>;

extern "C" {
    int get_export_name(thermopack_var, add_eos)();
    void get_export_name(thermopack_var, activate_model)(const int* model_idx);
    void get_export_name(thermopack_var, delete_eos)(const int* model_idx);

    double get_export_name(eostv, pressure)(double* T, double* V, double* n, double* dpdv, double* dpdt, double* d2pdv2, double* dpdn, int* property_flag);
    void get_export_name(eostv, chemical_potential_tv)(double* T, double* V, double* n, double* mu, double* dmudt, double* dmudv, double* dmudn, int* property_flag);
}


class Thermo{
    public:

    Thermo(std::string& comps) : true_int{1}, nc{static_cast<size_t>(std::count_if(comps.begin(), comps.end(), [](char c) {return c == ',';})) + 1},
            model_index_c{get_export_name(thermopack_var, add_eos)()}
        {}

    ~Thermo(){
        get_export_name(thermopack_var, delete_eos)(&model_index_c);
    }

    Property pressure_tv(double T, double V, vector1d n, bool dpdt=false, bool dpdv=false, bool dpdn=false, 
                        int property_flag=PropertyFlag::total){
        activate();
        Property p(nc, dpdt, dpdv, false, dpdn);
        p.value_ = get_export_name(eostv, pressure)(&T, &V, n.data(), p.dv_ptr, p.dt_ptr, nullptr, p.dn_ptr, &property_flag);
        return p;
    }

    VectorProperty chemical_potential_tv(double T, double V, vector1d n, bool dmudt=false, bool dmudv=false, bool dmudn=false, 
                                    int property_flag=PropertyFlag::total){
            activate();
            VectorProperty mu(nc, dmudt, dmudv, false, dmudn);
            get_export_name(eostv, chemical_potential_tv)(&T, &V, n.data(), mu.value_.data(), mu.dt_ptr, mu.dv_ptr, mu.dn_ptr, &property_flag);
            return mu;                            
    }

    protected:
    const int true_int;
    size_t nc;
    void activate(){
        get_export_name(thermopack_var, activate_model)(&model_index_c);
    }
    
    private:
    const int model_index_c;
};