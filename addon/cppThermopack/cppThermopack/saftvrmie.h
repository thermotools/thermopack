#pragma once
#include <string>
#include "dllmacros.h"
#include "saft.h"

extern "C" {
    void get_export_name(eoslibinit, init_saftvrmie)(char* comps, char* parameter_ref, size_t comps_strlen, size_t ref_strlen);
}

class Saftvrmie : public Saft {
    public:
    Saftvrmie(std::string comps, std::string parameter_reference="Default")
	: Saft(comps)
    {
	activate();
	get_export_name(eoslibinit, init_saftvrmie)(comps.data(), parameter_reference.data(), comps.size(), parameter_reference.size());
    }
};
