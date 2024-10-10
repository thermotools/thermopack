#ifndef CUBIC_H
#define CUBIC_H

#include <string>
#include <vector>
#include "thermo.h"

class Cubic : public Thermo
{
public:
	// Constructors
	Cubic(const std::string &comps, const std::string &eos, const std::string &mixing);

	// Initialization methods
	void init(const std::string &comps, const std::string &eos, const std::string &mixing = "vdW", 
			  const std::string &alpha = "Classic", const std::string &parameter_reference = "Default", 
			  bool volume_shift = false);
	void init_pseudo(const std::string &comps, const std::vector<double> &Tclist, const std::vector<double> &Pclist, 
					 const std::vector<double> &acflist, const std::vector<double> &Mwlist, 
					 const std::string &mixing = "vdW", const std::string &alpha = "Classic");

	// Interaction parameters (kij, lij, hv, ws)
	double get_kij(int c1, int c2);
	void set_kij(int c1, int c2, double kij);

	double get_lij(int c1, int c2);

	void get_hv_param(int c1, int c2, double &alpha_ij, double &alpha_ji, double &a_ij, double &a_ji, 
					  double &b_ij, double &b_ji, double &c_ij, double &c_ji);
	void thermopack_setHVparam(int i, int j, double alpha_ij, double alpha_ji, 
                                double aGE_ij, double aGE_ji, double bGE_ij, 
                                double bGE_ji, double cGE_ij, double cGE_ji);

	void get_ws_param(int c1, int c2, double &alpha_ij, double &alpha_ji, double &k_ij, double &k_ji, 
					  double &tau_ij, double &tau_ji);
	void set_ws_param(int c1, int c2, double alpha_ij, double alpha_ji, double k_ij, double k_ji, 
					  double tau_ij, double tau_ji);

	// Component-specific parameters (ci)
	void get_ci(int cidx, double &ciA, double &ciB, double &ciC, int &ci_type);
	void set_ci(int cidx, double ciA, double ciB = 0.0, double ciC = 0.0, int ci_type = 1);

	// Cubic equation parameters
	std::vector<double> get_covolumes();
	std::vector<double> get_energy_constants();

	// Correction terms
	void set_alpha_corr(int ic, const std::string &corrname, const std::vector<double> &coeffs);
	void set_beta_corr(int ic, const std::string &corrname, const std::vector<double> &coeffs);

	// Export method
	std::string get_export_name(const std::string &module, const std::string &method);

	int get_nc() const;
	void thermopack_setlijandji(int i, int j, double lij);
	

private:
	void *tp_handle;  // Handle for the thermodynamic package
	int nc;           // Number of components (tracked during initialization)

	std::string prefix = "__";
	std::string module = "eoslibinit";
	std::string postfix = "_MOD_";
	std::string postfix_nm = "";

	// New method to set the number of components based on the composition string
	void set_nc_from_comps(const std::string &comps);

	void load_library();
	void unload_library();
};

// Function declarations for interaction with the Fortran backend
extern "C"
{
	void __eoslibinit_MOD_init_cubic(char *comps, char *eos, char *mixing, char *alpha, char *ref,
									 int *volume_shift, size_t comp_strlen, size_t eos_strlen, 
									 size_t mixing_strlen, size_t alpha_strlen, size_t ref_strlen);
	void __eoslibinit_MOD_init_cubic_pseudo(const char *comps, const double *Tclist, const double *Pclist, 
											const double *acflist, const double *Mwlist, const char *mixing, 
											const char *alpha, size_t comps_len, size_t mixing_len, 
											size_t alpha_len);
											
	void thermopack_setlijandji_(int* i, int* j, double* lij);
	
	void thermopack_sethvparam_(int* i, int* j, double* alpha_ij, double* alpha_ji, 
                                double* aGE_ij, double* aGE_ji, double* bGE_ij, 
                                double* bGE_ji, double* cGE_ij, double* cGE_ji);
}
#endif // CUBIC_H
