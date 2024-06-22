#include "saft.h"

extern "C" {
    void get_export_name(eoslibinit, init_pcsaft)(char* comps, char* param_ref, int* simplified, int* polar, size_t comp_strlen, size_t ref_strlen);
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