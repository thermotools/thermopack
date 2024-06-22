#pragma once
#include "thermo.h"

class Saft : public Thermo {
    public:
    Saft(std::string& comps) : Thermo(comps){}

};