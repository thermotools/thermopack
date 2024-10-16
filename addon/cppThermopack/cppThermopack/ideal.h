#pragma once
#include "thermo.h"
#include "dllmacros.h"
#include <string>

extern "C" {
    void get_export_name(eoslibinit, init_ideal_eos)(char* comps, int* ierr, char* parameter_ref, size_t comps_strlen, size_t ref_strlen);
}

class Ideal : public Thermo {
    public:
    Ideal(std::string comps, std::string parameter_ref="Default") : Thermo(comps) {
        activate();
        int ierr{0};
        get_export_name(eoslibinit, init_ideal_eos)(comps.data(), &ierr, parameter_ref.data(), comps.size(), parameter_ref.size());
    }
};