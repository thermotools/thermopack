#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#ifndef thermopack_h_included
#define thermopack_h_included
  // Note that "thermopack_h_included" is just a dummy name; any other name
  // would yield the same result.

  //Author: HHU 06-2014, Ailo 09-2016, Morten Hammer 10-2020

  //---------------------------------------------------------------------//
  void thermopack_init_c(const char* eosLib, const char* eos, const char* mixing,
			 const char* alpha, int* ncomp,
			 const char* comp_string, int* nphases,
			 /*Optionals, set to NULL if not available: */
			 int* liq_vap_discr_method_in,
			 const char* csp_eos, const char* csp_ref_comp,
			 int* kij_setno, int alpha_setno[],
			 int* saft_setno, double* b_exponent);
  //---------------------------------------------------------------------//
  void thermopack_tpflash_c(const double* t, const double* p,
			   const double x_overall[],
			   double* beta, int* phase, double comp_liquid[],
			   double comp_vapor[]);
  //---------------------------------------------------------------------//
  void thermopack_uvflash_c(double* t, double* p, const double z[],
			    double* beta, double x[], double y[],
			    double* uspec, double* vspec, int* iphase, int* ierr);
  //---------------------------------------------------------------------//
  void thermopack_hpflash_c(double* t, const double* p, const double z[],
			    double* beta, double* betal, double x[], double y[],
			    const double* hspec, int* phase, int* ierr);
  //---------------------------------------------------------------------//
  void thermopack_spflash_c(double* t, const double* p, const double z[],
			    double* beta, double* betal, double x[], double y[],
			    const double* sspec, int* phase, int* ierr);
  //---------------------------------------------------------------------//
  void thermopack_bubt_c(const double* p, const double x[], double y[],
			 double* tbub, int *ierr);
  //---------------------------------------------------------------------//
  void thermopack_bubp_c(const double* t, const double x[], double y[],
			 double* pbub, int *ierr);
  //---------------------------------------------------------------------//
  void thermopack_dewt_c(const double* p, const double x[], double y[],
			 double* tdew, int *ierr);
  //---------------------------------------------------------------------//
  void thermopack_dewp_c(const double* t, const double x[], double y[],
			 double* pdew, int *ierr);
  //---------------------------------------------------------------------//
  void thermopack_zfac_c(const double* t, const double* p,
			 const double x[], const int* phase,
			 double* z);
  //---------------------------------------------------------------------//
  void thermopack_entropy_c(const double* t, const double* p, const double x[],
			    const int* phase, double* s);
  //---------------------------------------------------------------------//
  void thermopack_wilsonk_c(const double* t, const double* p, double k[]);
  //---------------------------------------------------------------------//
  void thermopack_pressure_c(const double* t, const double* rho, const double z[],
			     double* p);
  //---------------------------------------------------------------------//
  void thermopack_getcriticalparam_c(const int* i, double* tci, double* pci,
				     double* oi);
  //---------------------------------------------------------------------//
  void get_phase_flags_c(int* iTWOPH, int* iLIQPH, int* iVAPPH, int* iMINGIBBSPH,
			 int* iSINGLEPH, int* iSOLIDPH, int* iFAKEPH);
  //---------------------------------------------------------------------//
  void thermopack_compmoleweight_c(const int* j, double* mw);
  //---------------------------------------------------------------------//
  void thermopack_entropy_tv_c(const double* t, const double* v, const double n[], double* s);
  //---------------------------------------------------------------------//
  void thermopack_wilsonki_c(const int* i, const double* t, const double* p,
     const double* lnPhi_offset, double* k);
  //---------------------------------------------------------------------//
  void thermopack_enthalpy_c(const double* t, const double* p, const double x[],
     const int* phase, double* h, double* dhdt, double* dhdp, double dhdx[]);
  //---------------------------------------------------------------------//
  void thermopack_puresat_t_c(const double* p, const double z[], double* t, int* ierr);
  //---------------------------------------------------------------------//
  void thermopack_guess_phase_c(const double* t, const double* p, const double z[], int* phase);
  //---------------------------------------------------------------------//
  void thermopack_thermo_c(const double*T, const double*P, const double x[], const int* phase,
     const double lnfug[], const double lnfugt[], const double lnfugp[], const double* lnfugx,
     int* ophase);
  //---------------------------------------------------------------------//
  void thermopack_moleweight_c(const double z[], double* mw);
  //---------------------------------------------------------------------//
  void thermopack_specific_volume_c(const double* t, const double* p,
     const double x[], const int* iphase,
     double* v);
  //---------------------------------------------------------------------//
  void thermopack_twophase_dhdt_c(const double* t, const double *p,
     const double z[], const double x[],
     const double y[], const double *betav,
     const double *betaL, double *dhdt);

#endif ///*thermopack_h_included */

#if defined (__cplusplus)|| defined (c_plusplus)
} //extern "C"
#endif
