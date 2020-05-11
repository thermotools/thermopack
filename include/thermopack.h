#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#ifndef thermopack_h_included
#define thermopack_h_included
  // Note that "thermopack_h_included" is just a dummy name; any other name
  // would yield the same result.

  //Author: HHU 06-2014, Ailo 09-2016

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
  void thermopack_init_int_flags_(char* components_in, int* n_comp,
				  int* eos_lib_flag, int* eos_flag,
				  int* mixrule_flag, int* alpha_flag,
				  int* ierr, int* liq_vap_discr_flag,
				  int comps_strlen);
  // Note that the last argument comps_strlen is needed when passing arrays
  // between C and Fortran. Note that it is not passed as a pointer, and that it
  // is not included in the implementation of this routine in external.f90.

  //---------------------------------------------------------------------//
  void thermopack_init_(char* components, int* n_comp,
		       int* strlen, char eos_lib_str[], char eos_str[],
		       char mixrule_str[], char alpha_str[], int* ierr,
		       int eos_lib_str_len);
  //---------------------------------------------------------------------//
  void thermopack_cleanup_();
  //---------------------------------------------------------------------//
  void thermopack_tpflash_(const double* t, const double* p,
			   const double x_overall[],
			   double* beta, int* phase, double comp_liquid[],
			   double comp_vapor[], int nc);
  //---------------------------------------------------------------------//
  void thermopack_mf_tpflash_(const double* T, const double* P,
			      const double x_overall[],
			      int* n_phases, double* beta, int phaseflags[],
			      double* compositions[], int nph, int nc);
  //---------------------------------------------------------------------//
  void thermopack_fugacity_(const double* T, const double* P,
			    const double x_phase[],
			    const int* phaseflag, double fug[], int nc);
  //---------------------------------------------------------------------//
  double thermopack_bubble_pressure_(double* T, double* P, const double x[],
				     double Y[], int nc);
  //---------------------------------------------------------------------//
  void thermopack_phase_envelope_(const double* T, const double* P,
				  const double* beta, const char* cspec,
				  const double z[], const double* Pmax,
				  double Ta[], double Pa[], double* xa[],
				  double* ya[], int* nmax, int* n, int nc);
  //---------------------------------------------------------------------//
  void thermopack_density_(const double* t, const double* p, const double z[],
			   const int* iphase, double* rho, int ncomp);
  //---------------------------------------------------------------------//
  void thermopack_pressure_(const double* t, const double* rho, const double z[],
			    double* p, int nc);
  //---------------------------------------------------------------------//
  void thermopack_specific_volume_(const double* t, const double* p,
				   const double x[], const int* iphase,
				   double* v, int nc);
  //---------------------------------------------------------------------//
  void thermopack_internal_energy_(const double* t, const double* v,
				   const double x[], double* u, int nc);
  //---------------------------------------------------------------------//
  void thermopack_uvflash_(double* t, double* p, const double z[],
			   double* beta, double x[], double y[],
			   double* uspec, double* vspec, int* iphase, int nc);
  //---------------------------------------------------------------------//
  void thermopack_objective_(const double* t, const double* p, const double z[],
			     const double v[], int ncomp, const int* iobjective, int nc);
  //---------------------------------------------------------------------//
  void thermopack_hpflash_(double* t, const double* p, const double z[],
			   double* beta, double* betal, double x[], double y[],
			   const double* hspec, int* phase, int ncomp);
  //---------------------------------------------------------------------//
  void thermopack_spflash_(double* t, const double* p, const double z[],
			   double* beta, double* betal, double x[], double y[],
			   const double* sspec, int* phase, int ncomp);
  //---------------------------------------------------------------------//
  void thermopack_mp_spflash_(int* nd, double* t, const double* p,
			      const double Z[], int nc, double beta[], int nph,
			      double* xx[], int phasevec[], const double* sspec);
  //---------------------------------------------------------------------//
  void thermopack_bubt_(const double* p, const double x[], double y[],
			double* tbub, int *ierr, int nc);
  //---------------------------------------------------------------------//
  void thermopack_bubp_(const double* t, const double x[], double y[],
			double* pbub, int *ierr, int nc);
  //---------------------------------------------------------------------//
  void thermopack_dewt_(const double* p, const double x[], double y[],
			double* tdew, int *ierr, int nc);
  //---------------------------------------------------------------------//
  void thermopack_dewp_(const double* t, const double x[], double y[],
			double* pdew, int *ierr, int nc);
  //---------------------------------------------------------------------//
  void thermopack_zfac_(const double* t, const double* p,
			const double x[], const int* phase,
			double* z, int nc);
  //---------------------------------------------------------------------//
  void thermopack_twophase_specific_volume_(const double* t, const double* p,
					    const double z[], const double x[],
					    const double y[], const double* beta,
					    const int* phase, double* v,
					    int nc);
  //---------------------------------------------------------------------//
  void thermopack_enthalpy_(const double* t, const double* p, const double x[],
			    const int* phase, double* h, double* spec, double dhdx[],
			    const int* specflag, const int* residual, int nc);
  //---------------------------------------------------------------------//
  void thermopack_twophase_enthalpy_(const double* t, const double* p,
				     const double z[], const double x[],
				     const double y[], const double* beta,
				     const int* phase, double* h, int nc);
  //---------------------------------------------------------------------//
  void thermopack_entropy_(const double* t, const double* p, const double x[],
			   const int* phase, double* s, int nc);
  //---------------------------------------------------------------------//
  void thermopack_twophase_entropy_(const double* t, const double* p,
				    const double z[], const double x[],
				    const double y[], const double* beta,
				    const int* phase, double* s, int nc);
  //---------------------------------------------------------------------//
  void thermopack_pseudo_(const double x[], double* tpc, double* ppc,
			  double* acfpc, double* zpc, double* vpc,
			  int nc);
  //---------------------------------------------------------------------//
  void thermopack_wilsonk_(const double* t, const double* p, double k[], int nc);
  //---------------------------------------------------------------------//
  void thermopack_wilsonkdiff_(const double* t, const double* p, double k[],
			       double* dkdp, double* dkdt, int nc);
  //---------------------------------------------------------------------//
  void thermopack_getcriticalparam_(const int* i, double* tci, double* pci,
				    double* oi);
  //---------------------------------------------------------------------//
  void thermopack_single_idealgibbs_(const double* t, const double* p, const int* j,
				     double* g);
  //---------------------------------------------------------------------//
  void thermopack_single_idealentropy_(const double* t, const double* p, const int* j,
				       double* s);
  //---------------------------------------------------------------------//
  void thermopack_single_idealenthalpy_(const double* t, const double* p, const int* j,
					double* h);
  //---------------------------------------------------------------------//
  void thermopack_residual_gibbs_(const double* t, const double* p, const double z[],
				  const int* phase, double* gr, int nc);
  //---------------------------------------------------------------------//
  void thermopack_moleweight_(const double z[], double* mw, int nc);
  //---------------------------------------------------------------------//
  void thermopack_compmoleweight_(const int* j, double* mw, int nc);
  //---------------------------------------------------------------------//
  void thermopack_twophase_sound_velocity_(const double* t, const double*p,
					   const double x[], const double y[],
					   const double z[], const double* betav,
					   const double* betal, const int* phase,
					   double* sos, int nc);
  //---------------------------------------------------------------------//
  void thermopack_cp_(const double* t, const double* p, const double x[],
		      const int* phase, double* cp, int nc);
  //---------------------------------------------------------------------//
  void thermopack_cv_(const double* t, const double* p, const double x[],
		      const int* phase, double* cv, int nc);
  //---------------------------------------------------------------------//
  void thermopack_cp_ideal_(const int* i, const double* t, double* cp_id);
  //---------------------------------------------------------------------//
  void thermopack_twophase_dhdt_(const double* t, const double *p,
				 const double z[], const double x[],
				 const double y[], const double *betav,
				 const double *betaL, double *dhdt, int ncomp);
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
