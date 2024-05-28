#pragma once
#include "dllmacros.h"
#include <vector>

using vector1d = std::vector<double>;

extern "C" {
    int get_export_name(thermopack_var, add_eos)();
    void get_export_name(thermopack_var, activate_model)(const int* model_idx);
    void get_export_name(thermopack_var, delete_eos)(const int* model_idx);

    double get_export_name(eostv, pressure)(double* T, double* V, double* n, double** dpdt, double** dpdv, 
                                            double** dpdn, int* recalculate, int* property_flag);
}

enum Property{
    total = 0,
    residual,
    ideal
};

enum Phase{
    two = 0,
    liq,
    vap,
    fake,
    mingibbs,
    solid
};

class Thermo{
    public:

    Thermo() : model_index_c{get_export_name(thermopack_var, add_eos)()}, true_int{1} {}
    ~Thermo(){
        get_export_name(thermopack_var, delete_eos)(&model_index_c);
    }

    double pressure_tv(double T, double V, vector1d n, double* dpdt=nullptr, double* dpdv=nullptr, vector1d* dpdn=nullptr, 
                        int property_flag=Property::total){
        activate();
        int recalculate = true_int;
        double* dpdn_p = dpdn ? dpdn->data() : nullptr;
        double p = get_export_name(eostv, pressure)(&T, &V, n.data(), &dpdt, &dpdv, &dpdn_p,
                                                &recalculate, &property_flag);
        return p;
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