#pragma once
#include "thermo.h"
#include "saft.h"
#include "dllmacros.h"
#include <string>

extern "C" {
    void get_export_name(eoslibinit, init_ljs)(char* model, char* parameter_ref, size_t model_strlen, size_t ref_strlen);
    void get_export_name(lj_splined, ljs_bh_model_control)(int* use_lafitte_a3, int* enable_chi_correction, int* enable_hs, int* enable_a1, int* enable_a2, int* enable_a3);

    void get_export_name(lj_splined, ljs_bh_get_pure_params)(double* sigma, double* eps_div_k);
    void get_export_name(lj_splined, ljs_bh_set_pure_params)(double* sigma, double* eps_div_k);

    void get_export_name(lj_splined, ljs_wca_get_pure_params)(double* sigma, double* eps_div_k);
    void get_export_name(lj_splined, ljs_wca_set_pure_params)(double* sigma, double* eps_div_k);

    void get_export_name(lj_splined, calc_ai_reduced_ljs_ex)(double* T, double* rho_red, double* a1, double* a2, double* a3);
    void get_export_name(lj_splined, ljs_bh_get_bh_diameter_div_sigma)(double* T_red, double* d_BH);
}

class LJs_base : public Saft {
public:
    LJs_base(std::string model, std::string parameter_ref="Default", double minimum_temperature=2.0) : Saft("LJs"){
        activate();
        get_export_name(eoslibinit, init_ljs)(model.data(), parameter_ref.data(), model.size(), parameter_ref.size());
        set_tmin(minimum_temperature);
        ms[0] = 1.;
    }

    virtual void set_sigma_eps(double sigma_, double eps_div_k_) = 0;

    void set_sigma(double sigma_){
        activate();
        sigma[0] = sigma_;
        set_sigma_eps(sigma_, eps_div_k[0]);
    }

    void set_eps_div_k(double eps_div_k_){
        activate();
        set_sigma_eps(sigma[0], eps_div_k_);
    }

protected:
    virtual void init_params() = 0;
};

class LJs_bh : public LJs_base {
public:
    LJs_bh(std::string parameter_ref="Default", double minimum_temperature=2.0) : LJs_base("BH", parameter_ref, minimum_temperature) {init_params();}
    LJs_bh(double sigma_, double eps_div_k_) : LJs_base("BH") {
        init_params();
        set_sigma_eps(sigma_, eps_div_k_);
    }

    void set_sigma_eps(double sigma_, double eps_div_k_) override {
        activate();
        sigma[0] = sigma_;
        eps_div_k[0] = eps_div_k_;
        get_export_name(lj_splined, ljs_bh_set_pure_params)(&sigma_, &eps_div_k_);
    }

protected:
    void init_params() override {
        activate();
        get_export_name(lj_splined, ljs_bh_get_pure_params)(sigma.data(), eps_div_k.data());
    }
};

class LJs_wca_base : public LJs_base {
public:
    LJs_wca_base(std::string model, std::string parameter_ref="Default", double minimum_temperature=2.0) : LJs_base(model) {init_params();}
    LJs_wca_base(std::string model, double sigma_, double eps_div_k_) : LJs_base(model) {
        init_params();
        set_sigma_eps(sigma_, eps_div_k_);
    }

    void set_sigma_eps(double sigma_, double eps_div_k_) override {
        activate();
        sigma[0] = sigma_;
        eps_div_k[0] = eps_div_k_;
        get_export_name(lj_splined, ljs_wca_set_pure_params)(&sigma_, &eps_div_k_);
    }

protected:
    void init_params() override {
        activate();
        get_export_name(lj_splined, ljs_wca_get_pure_params)(sigma.data(), eps_div_k.data());
    }
};

class LJs_wca : public LJs_wca_base{
public:
    LJs_wca(std::string parameter_ref="Default", double minimum_temperature=2.0) 
        : LJs_wca_base("WCA", parameter_ref, minimum_temperature)
    {}
    LJs_wca(double sigma_, double eps_div_k_) : LJs_wca_base("WCA", sigma_, eps_div_k_) {}
};

class LJs_uv : public LJs_wca_base{
public:
    LJs_uv(std::string parameter_ref="Default", double minimum_temperature=2.0) 
        : LJs_wca_base("UV", parameter_ref, minimum_temperature)
    {}
    LJs_uv(double sigma_, double eps_div_k_) : LJs_wca_base("UV", sigma_, eps_div_k_) {}
};

