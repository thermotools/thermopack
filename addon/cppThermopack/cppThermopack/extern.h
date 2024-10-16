extern "C" {
    int get_export_name(thermopack_var, add_eos)();
    void get_export_name(thermopack_var, activate_model)(const int* model_idx);
    void get_export_name(thermopack_var, delete_eos)(const int* model_idx);

    double get_export_name(thermopack_var, get_tmin)();
    void get_export_name(thermopack_var, set_tmin)(double* tmin);
    double get_export_name(thermopack_var, get_tmax)();
    void get_export_name(thermopack_var, set_tmax)(double* tmax);
    double get_export_name(thermopack_var, get_pmin)();
    void get_export_name(thermopack_var, set_pmin)(double* pmin);
    double get_export_name(thermopack_var, get_pmax)();
    void get_export_name(thermopack_var, set_pmax)(double* pmax);

    void get_export_name(eos, specificvolume)(double* T, double* p, double* z, int* phase, double* v, double* dvdt, double* dvdp, double* dvdn);
    void get_export_name(eos, molardensity)(double* T, double* p, double* z, int* phase, double* v, double* dvdt, double* dvdp, double* dvdn);
    void get_export_name(eos, zfac)(double* T, double* p, double* z, int* phase, double* zfac, double* dzdt, double* dzdp, double* dzdn);
    void get_export_name(eos, thermo)(double* T, double* p, double* z, int* phase, double* lnfug, double* dlnfugdt, double* dlnfugdp, double* dlnfugdn, int* ophase, int* metaextremum, double* v);
    void get_export_name(eos, enthalpy)(double* T, double* p, double* z, int* phase, double* h, double* dhdt, double* dhdp, double* dhdn, int* property_flag);
    void get_export_name(eos, entropy)(double* T, double* p, double* z, int* phase, double* s, double* dsdt, double* dsdp, double* dsdn, int* property_flag);
    void get_export_name(eos, ideal_enthalpy_single)(double* T, int* comp_idx, double* h_id, double* dhdt);
    void get_export_name(eos, ideal_entropy_single)(double* T, double* p, int* comp_idx, double* s_id, double* dsdt, double* dsdp);
    // get/set standard enthalpy/entropy/ideal cp
    // speed_of_sound

    double get_export_name(eostv, pressure)(double* T, double* V, double* n, double* dpdv, double* dpdt, double* d2pdv2, double* dpdn, int* property_flag);
    void get_export_name(eostv, internal_energy_tv)(double* T, double* V, double* n, double* U, double* dudt, double* dudv, double* dudn, int* property_flag);
    void get_export_name(eostv, entropy_tv)(double* T, double* V, double* n, double* S, double* dsdt, double* dsdv, double* dsdn, int* property_flag);
    void get_export_name(eostv, enthalpy_tv)(double* T, double* V, double* n, double* S, double* dsdt, double* dsdv, double* dsdn, int* property_flag);
    void get_export_name(eostv, free_energy_tv)(double* T, double* V, double* n, double* S, double* dsdt, double* dsdv, double* dsdn, int* property_flag);
    void get_export_name(eostv, chemical_potential_tv)(double* T, double* V, double* n, double* mu, double* dmudt, double* dmudv, double* dmudn, int* property_flag);
    void get_export_name(eostv, thermo_tv)(double* T, double* V, double* n, double* lnphi, double* dt, double* dv, double* dn);

    void get_export_name(tp_solver, twophasetpflash)(double* T, double* p, double* z, double* betaV, double* betaL, int* phase, double* x, double* y);
    void get_export_name(ps_solver, twophasepsflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* s, int* phase, int* ierr);
    void get_export_name(uv_solver, twophaseuvflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* u, double* v, int* phase);
    void get_export_name(ph_solver, twophasephflash)(double* T, double* p, double* z, double* betaV, double* betaL, double* x, double* y, double* h, int* phase, int* ierr);
    // guess_phase

    double get_export_name(saturation, safe_bubt)(double* p, double* z, double* y, int* ierr);
    double get_export_name(saturation, safe_bubp)(double* T, double* z, double* y, int* ierr);
    double get_export_name(saturation, safe_dewt)(double* p, double* z, double* x, int* ierr);
    double get_export_name(saturation, safe_dewp)(double* T, double* z, double* x, int* ierr);

    void get_export_name(critical, calccriticaltv)(double* T, double* V, double* n, int* ierr, double* tol, double* v_min, double* p);
}