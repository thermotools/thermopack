#pragma once
#include <string>
#include "dllmacros.h"
#include "thermo.h"

extern "C" {
    void get_export_name(eoslibinit, init_cubic)(char* comps, char* eos, char* mixing, char* alpha, char* ref,
						int* volume_shift, size_t comp_strlen, size_t eos_strlen, size_t mixing_strlen,
						size_t alpha_strlen, size_t ref_strlen);

}

class Cubic : public Thermo{
    public:
    Cubic(std::string comps, std::string eos,
	    std::string mixing="vdW", std::string alpha="Classic",
	    std::string ref="Default", bool volume_shift=false)
	: Thermo(comps)
    {
	activate();
	int vol_shift_int = volume_shift ? true_int : 0;
	get_export_name(eoslibinit, init_cubic)(comps.data(), eos.data(), mixing.data(), alpha.data(), ref.data(),
	    &vol_shift_int, comps.size(), eos.size(), mixing.size(), alpha.size(), ref.size());

    }

};

class PengRobinson : public Cubic{
    public:
    PengRobinson(std::string comps, std::string mixing="vdW", std::string alpha="Classic",
	    std::string ref="Default", bool volume_shift=false)
	: Cubic(comps, "PR", mixing, alpha, ref, volume_shift)
	{}
};

class SoaveRedlichKwong : public Cubic{
    public:
    SoaveRedlichKwong(std::string comps, std::string mixing="vdW", std::string alpha="Classic",
	    std::string ref="Default", bool volume_shift=false)
	: Cubic(comps, "SRK", mixing, alpha, ref, volume_shift)
	{}
};
