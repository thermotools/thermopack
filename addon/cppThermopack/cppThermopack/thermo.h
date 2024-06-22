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

    void get_export_name(eos, specificvolume)(double* T, double* p, double* z, int* phase, double* v, double* dvdt, double* dvdp, double* dvdn);
    void get_export_name(eos, zfac)(double* T, double* p, double* z, int* phase, double* zfac, double* dzdt, double* dzdp, double* dzdn);
    void get_export_name(eos, thermo)(double* T, double* p, double* z, int* phase, double* lnfug, double* dlnfugdt, double* dlnfugdp, double* dlnfugdn, int* ophase, int* metaextremum, double* v);
    void get_export_name(eos, enthalpy)(double* T, double* p, double* z, int* phase, double* h, double* dhdt, double* dhdp, double* dhdn, int* property_flag);
    void get_export_name(eos, entropy)(double* T, double* p, double* z, int* phase, double* s, double* dsdt, double* dsdp, double* dsdn, int* property_flag);
    void get_export_name(eos, ideal_enthalpy_single)(double* T, int* comp_idx, double* h_id, double* dhdt);
    void get_export_name(eos, ideal_entropy_single)(double* T, double* p, int* comp_idx, double* s_id, double* dsdt, double* dsdp);

    double get_export_name(eostv, pressure)(double* T, double* V, double* n, double* dpdv, double* dpdt, double* d2pdv2, double* dpdn, int* property_flag);
    void get_export_name(eostv, internal_energy_tv)(double* T, double* V, double* n, double* U, double* dudt, double* dudv, double* dudn, int* property_flag);
    void get_export_name(eostv, chemical_potential_tv)(double* T, double* V, double* n, double* mu, double* dmudt, double* dmudv, double* dmudn, int* property_flag);

    void get_export_name(tp_solver, twophasetpflash)(double* T, double* p, double* z, double* betaV, double* betaL, int* phase, double* x, double* y);
    void get_export_name(ps_solver, twophasepsflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* s, int* phase, int* ierr);
    void get_export_name(uv_solver, twophaseuvflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* u, double* v, int* phase);
    void get_export_name(ph_solver, twophasephflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* h, int* phase, int* ierr);
}


class Thermo{
    public:

    Thermo(std::string& comps) : true_int{1}, nc{static_cast<size_t>(std::count_if(comps.begin(), comps.end(), [](char c) {return c == ',';})) + 1},
            model_index_c{get_export_name(thermopack_var, add_eos)()}
        {}

    ~Thermo(){
        get_export_name(thermopack_var, delete_eos)(&model_index_c);
    }

    int TWOPH = Phase::two;
    int LIQPH = Phase::liq;
    int VAPPH = Phase::vap;

    Property specific_volume(double T, double p, vector1d z, int phase, bool dvdt=false, bool dvdp=false, bool dvdn=false){
        activate();
        Property v(nc, dvdt, false, dvdp, dvdn);
        get_export_name(eos, specificvolume)(&T, &p, z.data(), &phase, &v.value_, v.dt_ptr, v.dp_ptr, v.dn_ptr);
        return v;
    }

    Property zfac(double T, double p, vector1d z, int phase, bool dzdt=false, bool dzdp=false, bool dzdn=false){
        activate();
        Property Z(nc, dzdt, false, dzdp, dzdn);
        get_export_name(eos, zfac)(&T, &p, z.data(), &phase, &Z.value_, Z.dt_ptr, Z.dp_ptr, Z.dn_ptr);
        return Z;
    }

    VectorProperty thermo(double T, double p, vector1d z, int phase, bool dlnfugdt=false, bool dlnfugdp=false, bool dlnfugdn=false, int* ophase=nullptr, double* v=nullptr){
        int metaextremum;
        activate();
        VectorProperty lnfug(nc, dlnfugdt, false, dlnfugdp, dlnfugdn);
        get_export_name(eos, thermo)(&T, &p, z.data(), &phase, lnfug.value_.data(), lnfug.dt_ptr, lnfug.dp_ptr, lnfug.dn_ptr, ophase, &metaextremum, v);
        return lnfug;
    }

    Property enthalpy(double T, double p, vector1d z, int phase, bool dhdt=false, bool dhdp=false, bool dhdn=false, int property_flag=PropertyFlag::total){
        activate();
        Property h(nc, dhdt, false, dhdp, dhdn);
        get_export_name(eos, enthalpy)(&T, &p, z.data(), &phase, &h.value_, h.dt_ptr, h.dp_ptr, h.dn_ptr, &property_flag);
        return h;
    }

    Property entropy(double T, double p, vector1d z, int phase, bool dsdt=false, bool dsdp=false, bool dsdn=false, int property_flag=PropertyFlag::total){
        activate();
        Property s(nc, dsdt, false, dsdp, dsdn);
        get_export_name(eos, entropy)(&T, &p, z.data(), &phase, &s.value_, s.dt_ptr, s.dp_ptr, s.dn_ptr, &property_flag);
        return s;
    }

    Property idealenthalpysingle(double T, int comp_idx, bool dhdt=false){
        activate();
        Property h_id(1, dhdt, false, false, false);
        get_export_name(eos, ideal_enthalpy_single)(&T, &comp_idx, &h_id.value_, h_id.dt_ptr);
        return h_id;
    }

    Property idealentropysingle(double T, double p, int comp_idx, bool dsdt=false, bool dsdp=false){
        activate();
        Property s_id(1, dsdt, false, dsdp, false);
        get_export_name(eos, ideal_entropy_single)(&T, &p, &comp_idx, &s_id.value_, s_id.dt_ptr, s_id.dp_ptr);
        return s_id;
    }

    Property pressure_tv(double T, double V, vector1d n, bool dpdt=false, bool dpdv=false, bool dpdn=false, 
                        int property_flag=PropertyFlag::total){
        activate();
        Property p(nc, dpdt, dpdv, false, dpdn);
        p.value_ = get_export_name(eostv, pressure)(&T, &V, n.data(), p.dv_ptr, p.dt_ptr, nullptr, p.dn_ptr, &property_flag);
        return p;
    }

    Property internal_energy_tv(double T, double V, vector1d n, bool dudt=false, bool dudv=false, bool dudn=false, int property_flag=PropertyFlag::total){
        activate();
        Property U(nc, dudt, dudv, false, dudn);
        get_export_name(eostv, internal_energy_tv)(&T, &V, n.data(), &U.value_, U.dt_ptr, U.dv_ptr, U.dn_ptr, &property_flag);
        return U;
    }

    VectorProperty chemical_potential_tv(double T, double V, vector1d n, bool dmudt=false, bool dmudv=false, bool dmudn=false, 
                                    int property_flag=PropertyFlag::total){
            activate();
            VectorProperty mu(nc, dmudt, dmudv, false, dmudn);
            get_export_name(eostv, chemical_potential_tv)(&T, &V, n.data(), mu.value_.data(), mu.dt_ptr, mu.dv_ptr, mu.dn_ptr, &property_flag);
            return mu;                            
    }

    FlashResult two_phase_tpflash(double T, double p, vector1d z){
        activate();
        FlashResult fr(T, p, z, "TP");
        get_export_name(tp_solver, twophasetpflash)(&T, &p, z.data(), &fr.betaV, &fr.betaL, &fr.phase, fr.x.data(), fr.y.data());
        return fr;
    }

    FlashResult two_phase_psflash(double p, double s, vector1d z, double T=0.){
        activate();
        FlashResult fr(T, p, z, "PS");
        int ierr = 0;
        get_export_name(ps_solver, twophasepsflash)(&fr.T, &p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &s, &fr.phase, &ierr);
        if (ierr) throw std::runtime_error("PS flash failed");
        return fr;
    }

    FlashResult two_phase_phflash(double p, double h, vector1d z, double T=0.){
        activate();
        FlashResult fr(T, p, z, "PH");
        int ierr = 0;
        get_export_name(ph_solver, twophasephflash)(&fr.T, &p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &h, &fr.phase, &ierr);
        if (ierr) throw std::runtime_error("PH flash failed");
        return fr;
    }

    FlashResult two_phase_uvflash(double u, double v, vector1d z, double T=0., double p=0.){
        activate();
        FlashResult fr(T, p, z, "UV");
        get_export_name(uv_solver, twophaseuvflash)(&fr.T, &fr.p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &u, &v, &fr.phase);
        return fr;
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