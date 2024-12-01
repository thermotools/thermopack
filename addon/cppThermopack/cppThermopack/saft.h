/*
    Parent class for SAFT-type equations of state
    Constructor forwards the "std::string comps" argument to Thermo, which sets the "const size_t nc" member

    Implements generic set- and get methods that are common to all saft-type EoS.

    Inheriting classes must implement "init_params", and ensure that the member variabels "sigma", "eps_div_k", and "ms" are set upon construction.
*/
#pragma once
#include "thermo.h"

extern "C" {
    void get_export_name(saft_interface, sigma_ij)(size_t* i, size_t* j, double* sigma);
    void get_export_name(saft_interface, epsilon_ij)(size_t* i, size_t* j, double* eps);
    void get_export_name(saft_interface, sigma_eff_ij)(size_t* i, size_t* j, double* T, double* sigma_eff);
    void get_export_name(saft_interface, epsilon_eff_ij)(size_t* i, size_t* j, double* T, double* epsilon_eff);
}

class Saft : public Thermo {
public:
    Saft(std::string comps) : Thermo(comps){
        sigma = std::vector<double>(nc);
        eps_div_k = std::vector<double>(nc);
        ms = std::vector<double>(nc);
    }

    // virtual void set_sigma(size_t ci, double sigma_);
    double get_sigma(size_t ci){return sigma[ci];}
    virtual std::vector<double> get_sigma(){
        return sigma;
    }

    // virtual void set_eps_div_k(size_t ci, double eps_div_k_);
    double get_eps_div_k(size_t ci){return eps_div_k[ci];}
    virtual std::vector<double> get_eps_div_k(){
        return eps_div_k;
    }

protected:
    virtual void init_params() = 0;

    std::vector<double> sigma;
    std::vector<double> eps_div_k;
    std::vector<double> ms;
};