#pragma once
#include "dllmacros.h"
#include "extern.h"
#include "utils.h"
#include <utility>
#include <vector>
#include <algorithm>
#include <iostream>

using vector1d = std::vector<double>;
using vector2d = std::vector<vector1d>;

class Thermo{
public:

    Thermo(std::string& comps) : true_int{1}, nc{static_cast<size_t>(std::count_if(comps.begin(), comps.end(), [](char c) {return c == ',';})) + 1},
	    model_idx{get_export_name(thermopack_var, add_eos)()}
	{}

    Thermo(Thermo&& other) : true_int{other.true_int}, nc{other.nc}, model_idx{std::exchange(other.model_idx, 0)} {}
    Thermo(const Thermo& other) = delete; // Suggested implementation if needed : true_int{other.true_int}, nc{other.nc}, model_idx{get_export_name(thermopack_var, add_eos)()}{}

    ~Thermo(){
	// To handle move semantics (which we need), the move constructor sets the moved-from objects model_idx to
	// zero, so that we can check in the destructor whether the object has been moved from before deleting
	// the underlying memory with delete_eos.
	if (model_idx > 0) get_export_name(thermopack_var, delete_eos)(&model_idx);
    }

    int TWOPH = Phase::two;
    int LIQPH = Phase::liq;
    int VAPPH = Phase::vap;

/************************************************************************/
// ------------------------ TP-Property Interfaces -------------------- //
/************************************************************************/

    Property specific_volume(double T, double p, vector1d z, int phase, bool dvdt=false, bool dvdp=false, bool dvdn=false) const {
		activate();
		Property v(nc, dvdt, false, dvdp, dvdn);
		get_export_name(eos, specificvolume)(&T, &p, z.data(), &phase, &v.value_, v.dt_ptr, v.dp_ptr, v.dn_ptr);
		return v;
    }

    Property molar_density(double T, double p, vector1d z, int phase, bool drdt=false, bool drdp=false, bool drdn=false) const {
		activate();
		Property rho(nc, drdt, false, drdp, drdn);
		get_export_name(eos, molardensity)(&T, &p, z.data(), &phase, &rho.value_, rho.dt_ptr, rho.dp_ptr, rho.dn_ptr);
		return rho;
    }

    Property zfac(double T, double p, vector1d z, int phase, bool dzdt=false, bool dzdp=false, bool dzdn=false) const {
		activate();
		Property Z(nc, dzdt, false, dzdp, dzdn);
		get_export_name(eos, zfac)(&T, &p, z.data(), &phase, &Z.value_, Z.dt_ptr, Z.dp_ptr, Z.dn_ptr);
		return Z;
    }

    VectorProperty thermo(double T, double p, vector1d z, int phase, bool dlnfugdt=false, bool dlnfugdp=false, bool dlnfugdn=false, int* ophase=nullptr, double* v=nullptr) const {
		int metaextremum;
		activate();
		VectorProperty lnfug(nc, dlnfugdt, false, dlnfugdp, dlnfugdn);
		get_export_name(eos, thermo)(&T, &p, z.data(), &phase, lnfug.value_.data(), lnfug.dt_ptr, lnfug.dp_ptr, lnfug.dn_ptr, ophase, &metaextremum, v);
		return lnfug;
    }

    Property enthalpy(double T, double p, vector1d z, int phase, bool dhdt=false, bool dhdp=false, bool dhdn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property h(nc, dhdt, false, dhdp, dhdn);
		get_export_name(eos, enthalpy)(&T, &p, z.data(), &phase, &h.value_, h.dt_ptr, h.dp_ptr, h.dn_ptr, &property_flag);
		return h;
    }

    Property entropy(double T, double p, vector1d z, int phase, bool dsdt=false, bool dsdp=false, bool dsdn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property s(nc, dsdt, false, dsdp, dsdn);
		get_export_name(eos, entropy)(&T, &p, z.data(), &phase, &s.value_, s.dt_ptr, s.dp_ptr, s.dn_ptr, &property_flag);
		return s;
    }

    Property idealenthalpysingle(double T, int comp_idx, bool dhdt=false) const {
		activate();
		Property h_id(1, dhdt, false, false, false);
		get_export_name(eos, ideal_enthalpy_single)(&T, &comp_idx, &h_id.value_, h_id.dt_ptr);
		return h_id;
    }

    Property idealentropysingle(double T, double p, int comp_idx, bool dsdt=false, bool dsdp=false) const {
		activate();
		Property s_id(1, dsdt, false, dsdp, false);
		get_export_name(eos, ideal_entropy_single)(&T, &p, &comp_idx, &s_id.value_, s_id.dt_ptr, s_id.dp_ptr);
		return s_id;
    }

/************************************************************************/
// ------------------------ TV-Property Interfaces -------------------- //
/************************************************************************/

    Property pressure_tv(double T, double V, vector1d n, bool dpdt=false, bool dpdv=false, bool dpdn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property p(nc, dpdt, dpdv, false, dpdn);
		p.value_ = get_export_name(eostv, pressure)(&T, &V, n.data(), p.dv_ptr, p.dt_ptr, nullptr, p.dn_ptr, &property_flag);
		return p;
    }

    Property internal_energy_tv(double T, double V, vector1d n, bool dudt=false, bool dudv=false, bool dudn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property U(nc, dudt, dudv, false, dudn);
		get_export_name(eostv, internal_energy_tv)(&T, &V, n.data(), &U.value_, U.dt_ptr, U.dv_ptr, U.dn_ptr, &property_flag);
		return U;
    }

    Property entropy_tv(double T, double V, vector1d n, bool dsdt=false, bool dsdv=false, bool dsdn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property S(nc, dsdt, dsdv, false, dsdn);
		get_export_name(eostv, entropy_tv)(&T, &V, n.data(), &S.value_, S.dt_ptr, S.dv_ptr, S.dn_ptr, &property_flag);
		return S;
    }

    Property enthalpy_tv(double T, double V, vector1d n, bool dhdt=false, bool dhdv=false, bool dhdn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property H(nc, dhdt, dhdv, false, dhdn);
		get_export_name(eostv, enthalpy_tv)(&T, &V, n.data(), &H.value_, H.dt_ptr, H.dv_ptr, H.dn_ptr, &property_flag);
		return H;
    }

    Property helmholtz_tv(double T, double V, vector1d n, bool dadt=false, bool dadv=false, bool dadn=false, int property_flag=PropertyFlag::total) const {
		activate();
		Property A(nc, dadt, dadv, false, dadn);
		get_export_name(eostv, free_energy_tv)(&T, &V, n.data(), &A.value_, A.dt_ptr, A.dv_ptr, A.dn_ptr, &property_flag);
		return A;
    }

    VectorProperty chemical_potential_tv(double T, double V, vector1d n, bool dmudt=false, bool dmudv=false, bool dmudn=false, int property_flag=PropertyFlag::total) const {
	    activate();
	    VectorProperty mu(nc, dmudt, dmudv, false, dmudn);
	    get_export_name(eostv, chemical_potential_tv)(&T, &V, n.data(), mu.value_.data(), mu.dt_ptr, mu.dv_ptr, mu.dn_ptr, &property_flag);
	    return mu;
    }

    VectorProperty fugacity_tv(double T, double V, vector1d n, bool dlnphidt=false, bool dlnphidv=false, bool dlnphidn=false) const {
		activate();
		VectorProperty lnphi(nc, dlnphidt, dlnphidv, false, dlnphidn);
		get_export_name(eostv, thermo_tv)(&T, &V, n.data(), lnphi.value_.data(), lnphi.dt_ptr, lnphi.dv_ptr, lnphi.dn_ptr);
		return lnphi;
    }

/************************************************************************/
// --------------------------- Flash interfaces ----------------------- //
/************************************************************************/
    FlashResult two_phase_tpflash(double T, double p, vector1d z) const {
		activate();
		FlashResult fr(T, p, z, "TP");
		get_export_name(tp_solver, twophasetpflash)(&T, &p, z.data(), &fr.betaV, &fr.betaL, &fr.phase, fr.x.data(), fr.y.data());
		return fr;
    }

    FlashResult two_phase_psflash(double p, double s, vector1d z, double T=0.) const {
		activate();
		FlashResult fr(T, p, z, "PS");
		int ierr = 0;
		get_export_name(ps_solver, twophasepsflash)(&fr.T, &p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &s, &fr.phase, &ierr);
		if (ierr) throw std::runtime_error("PS flash failed");
		return fr;
    }

    FlashResult two_phase_phflash(double p, double h, vector1d z, double T=0.) const {
		activate();
		FlashResult fr(T, p, z, "PH");
		int ierr = 0;
		get_export_name(ph_solver, twophasephflash)(&fr.T, &p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &h, &fr.phase, &ierr);
		if (ierr) throw std::runtime_error("PH flash failed");
		return fr;
    }

    FlashResult two_phase_uvflash(double u, double v, vector1d z, double T=0., double p=0.) const {
		activate();
		FlashResult fr(T, p, z, "UV");
		get_export_name(uv_solver, twophaseuvflash)(&fr.T, &fr.p, z.data(), &fr.betaV, &fr.betaL, fr.x.data(), fr.y.data(), &u, &v, &fr.phase);
		return fr;
    }

    FlashResult bubble_temperature(double p, vector1d z) const {
		activate();
		FlashResult bub = FlashResult::bub(300., p, z, "Bub_T");
		int ierr;
		bub.T = get_export_name(saturation, safe_bubt)(&p, z.data(), bub.y.data(), &ierr);
		if (ierr == true_int) throw std::runtime_error("Bubble temperature calculation failed!");

		return bub;
    }

    FlashResult bubble_pressure(double T, vector1d z) const {
		activate();
		FlashResult bub = FlashResult::bub(T, 1e5, z, "Bub_P");
		int ierr;
		bub.p = get_export_name(saturation, safe_bubp)(&T, z.data(), bub.y.data(), &ierr);
		if (ierr == true_int)throw std::runtime_error("Bubble pressure calculation failed!");

		return bub;
    }

    FlashResult dew_temperature(double p, vector1d z) const {
		activate();
		FlashResult dew = FlashResult::dew(300., p, z, "Dew_T");
		int ierr;
		dew.T = get_export_name(saturation, safe_dewt)(&p, dew.x.data(), z.data(), &ierr);
		if (ierr == true_int)throw std::runtime_error("Dew temperature calculation failed!");

		return dew;
    }

    FlashResult dew_pressure(double T, vector1d z) const {
		activate();
		FlashResult dew = FlashResult::dew(T, 1e5, z, "Dew_P");
		int ierr;
		dew.p = get_export_name(saturation, safe_dewp)(&T, dew.x.data(), z.data(), &ierr);
		if (ierr == true_int)throw std::runtime_error("Dew pressure calculation failed!");

		return dew;
    }

    vector1d critical(vector1d n, double T=0., double V=0., double tol=1e-7, double v_min=-1.) const {
		activate();
		double p = 0.;
		vector1d tvp = {T, V, p};
		int ierr;
		get_export_name(critical, calccriticaltv)(&(tvp[0]), &(tvp[1]), n.data(), &ierr, &tol, (v_min > 0) ? &v_min : nullptr, &(tvp[2]));
		if (ierr == true_int) throw std::runtime_error("Critical calculation failed!");

		return tvp;
    }

/************************************************************************/
// ------------------------------- Utility ---------------------------- //
/************************************************************************/

	double get_tmin() const {
		activate();
		return get_export_name(thermopack_var, get_tmin)();
	}

    void set_tmin(double tmin) {
		activate();
		get_export_name(thermopack_var, set_tmin)(&tmin);
	}

    double get_tmax() const {
		activate();
		return get_export_name(thermopack_var, get_tmax)();
	}

	void set_tmax(double tmax) {
		activate();
		get_export_name(thermopack_var, set_tmax)(&tmax);
	}

    double get_pmin() const {
		activate();
		return get_export_name(thermopack_var, get_pmin)();
	}
    void set_pmin(double pmin) {
		activate();
		get_export_name(thermopack_var, set_pmin)(&pmin);
	}

    double get_pmax() const {
		activate();
		return get_export_name(thermopack_var, get_pmax)();
	}

    void set_pmax(double pmax) {
		activate();
		get_export_name(thermopack_var, set_pmax)(&pmax);
	}

protected:
    const int true_int;
    size_t nc;
    void activate() const {
		get_export_name(thermopack_var, activate_model)(&model_idx);
    }

private:
    int model_idx;
};
