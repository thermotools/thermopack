#include "saft.h"
#include <array>

extern "C" {
    void get_export_name(eoslibinit, init_pcsaft)(char* comps, char* param_ref, int* simplified, int* polar, size_t comp_strlen, size_t ref_strlen);

    void get_export_name(saft_interface, pc_saft_get_pure_params)(int* ci, double* param);
        
}

class Pcsaft : public Saft{
    public:
    Pcsaft(std::string comps, std::string parameter_reference="Default", bool simplified=false, bool polar=false)
        : Saft(comps)
    {
        activate();
        int i_simplified = (simplified) ? true_int : 0;
        int i_polar = (polar) ? true_int : 0;
        get_export_name(eoslibinit, init_pcsaft)(comps.data(), parameter_reference.data(), &i_simplified, &i_polar, comps.size(), parameter_reference.size());
        sigma = std::vector<double>(nc);
        eps_div_k = std::vector<double>(nc);
        init_params();
    }

protected:
    void init_params(){
        std::array<double, 5> param;
        int ci; // Fortran component index
        for (size_t i = 0; i < nc; i++){
            ci = i + 1;
            get_export_name(saft_interface, pc_saft_get_pure_params)(&ci, param.data());
            ms[i] = param[0]; sigma[i] = param[1]; eps_div_k[i] = param[2];
        }
    }
};

class Spcsaft : public Pcsaft{
    public:
    Spcsaft(std::string comps, std::string parameter_reference="Default")
        : Pcsaft(comps, parameter_reference, true)
    {}
};

class Pcpsaft : public Pcsaft{
    public:
    Pcpsaft(std::string comps, std::string parameter_reference="Default")
        : Pcsaft(comps, parameter_reference, false, true)
    {}
};