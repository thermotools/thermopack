#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#ifndef thermopack_h_included
#define thermopack_h_included
  // Note that "thermopack_h_included" is just a dummy name; any other name
  // would yield the same result.

  //Author: HHU 06-2014, Ailo 09-2016, Morten Hammer 10-2020

#ifdef __INTEL_THERMOPACK__
// INTEL FORTRAN (x64)
#define MOD_PREFIX
#define MOD_INTERTXT _mp_
#define MOD_POSTFIX _
#define FORTRAN_POSTFIX _
#else
// GNU FORTRAN mapping
#define MOD_PREFIX __
#define MOD_INTERTXT _MOD_
#define MOD_POSTFIX
#define FORTRAN_POSTFIX _
#endif

#define _CONCAT(x,y) x##y
#define CONCAT(x,y) _CONCAT(x,y)
#define MODULE_METHOD(MOD, METHOD) CONCAT(CONCAT(MOD_PREFIX,MOD),CONCAT(CONCAT(MOD_INTERTXT,METHOD),MOD_POSTFIX))
#define FORTRAN_METHOD(METHOD) CONCAT(METHOD,FORTRAN_POSTFIX)
#define ISO_C_METHOD(METHOD) METHOD

  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_init_c)(const char* eos, const char* mixing,
				       const char* alpha, const char* comp_string,
				       int* nphases,
				       /*Optionals, set to NULL (int/double) and "" (const char*) if not available: */
				       int* liq_vap_discr_method_in,
				       const char* csp_eos, const char* csp_ref_comp,
				       const char* kij_ref, const char* alpha_ref,
				       const char* saft_ref, double* b_exponent);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_tpflash_c)(const double* t, const double* p,
					  const double x_overall[],
					  double* beta, int* phase, double comp_liquid[],
					  double comp_vapor[]);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_uvflash_c)(double* t, double* p, const double z[],
					  double* beta, double x[], double y[],
					  double* uspec, double* vspec, int* iphase, int* ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_hpflash_c)(double* t, const double* p, const double z[],
					  double* beta, double* betal, double x[], double y[],
					  const double* hspec, int* phase, int* ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_spflash_c)(double* t, const double* p, const double z[],
					  double* beta, double* betal, double x[], double y[],
					  const double* sspec, int* phase, int* ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_bubt_c)(const double* p, const double x[], double y[],
				       double* tbub, int *ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_bubp_c)(const double* t, const double x[], double y[],
				       double* pbub, int *ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_dewt_c)(const double* p, const double x[], double y[],
				       double* tdew, int *ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_dewp_c)(const double* t, const double x[], double y[],
				       double* pdew, int *ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_zfac_c)(const double* t, const double* p,
				       const double x[], const int* phase,
				       double* z);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_entropy_c)(const double* t, const double* p, const double x[],
					  const int* phase, double* s);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_wilsonk_c)(const double* t, const double* p, double k[]);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_pressure_c)(const double* t, const double* v, const double n[],
					   double* p);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_getcriticalparam_c)(const int* i, double* tci, double* pci,
						   double* oi);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(get_phase_flags_c)(int* iTWOPH, int* iLIQPH, int* iVAPPH, int* iMINGIBBSPH,
				       int* iSINGLEPH, int* iSOLIDPH, int* iFAKEPH);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_compmoleweight_c)(const int* j, double* mw);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_entropy_tv_c)(const double* t, const double* v, const double n[], double* s);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_wilsonki_c)(const int* i, const double* t, const double* p,
					   const double* lnPhi_offset, double* k);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_enthalpy_c)(const double* t, const double* p, const double x[],
					   const int* phase, double* h, double* dhdt, double* dhdp, double dhdx[]);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_puresat_t_c)(const double* p, const double z[], double* t, int* ierr);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_guess_phase_c)(const double* t, const double* p, const double z[], int* phase);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_thermo_c)(const double*T, const double*P, const double x[], const int* phase,
					 const double lnfug[],
					/*Optionals, set to NULL if undesired: */
					 const double lnfugt[], const double lnfugp[], const double* lnfugx,
					 int* ophase);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_moleweight_c)(const double z[], double* mw);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_specific_volume_c)(const double* t, const double* p,
						  const double x[], const int* iphase,
						  double* v);
  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_twophase_dhdt_c)(const double* t, const double *p,
						const double z[], const double x[],
						const double y[], const double *betav,
						const double *betaL, double *dhdt);

  //---------------------------------------------------------------------//
  void ISO_C_METHOD(thermopack_comp_name_c)(const int* i, char** compname);

  //---------------------------------------------------------------------//
  // Module interface
  //---------------------------------------------------------------------//
  double MODULE_METHOD(eostv, pressure)(const double* t, const double* v,
					const double n[],
					/*Optionals, set to NULL if undesired: */
					const double* dpdv,
					const double* dpdt, const double* d2pdv2,
					const double dpdn[], const int* contribution);

  //---------------------------------------------------------------------//
  // Module parameters/variables
  //---------------------------------------------------------------------//
  #define RGAS MODULE_METHOD(thermopack_var, rgas)
    extern double RGAS;
  #define TMAX MODULE_METHOD(thermopack_var, tptmax)
    extern double TMAX;
  #define TMIN MODULE_METHOD(thermopack_var, tptmin)
    extern double TMIN;

#endif ///*thermopack_h_included */

#if defined (__cplusplus)|| defined (c_plusplus)
} //extern "C"
#endif
