#ifndef THERMO_H
#define THERMO_H

#include <string>
#include <vector>
#include <dlfcn.h>
#include <stdexcept>
#include <cstdint>
#include <cstring>

typedef int (*p_get_comp_index)(const char *, size_t);

class Thermo
{
public:
	Thermo();
	virtual ~Thermo();
	
	int MINGIBBSPH = 3;
	int TWOPH = 2;
	int LIQPH = 1;
	int VAPPH = 0;

	void activate();
	void init_thermo(const std::string &eos, const std::string &mixing, const std::string &alpha,
					 const std::string &comps, int nphases, int *liq_vap_discr_method = nullptr,
					 const std::string &csp_eos = "", const std::string &csp_ref_comp = "",
					 const std::string &kij_ref = "Default", const std::string &alpha_ref = "Default",
					 const std::string &saft_ref = "Default", double *b_exponent = nullptr,
					 const std::string &TrendEosForCp = "", int *cptype = nullptr,
					 bool *silent = nullptr);

	void init_peneloux_volume_translation(const std::string &parameter_reference = "Default");

	// Get critical temperature of a component
	double critical_temperature(int i);

	// Calculate bubble pressure given temperature and composition
	std::pair<double, std::vector<double>> bubble_pressure(double temp, const std::vector<double> &z);

	// Perform thermodynamic property calculations
	// Added optional pointers for differentials (dlnfugdt, dlnfugdp, dlnfugdn) and phase/v outputs (ophase, v)
	std::vector<double> thermo(double T, double P, const std::vector<double> &composition, int phase_flag, 
                                  std::vector<double>* dlnfugdt, std::vector<double>* dlnfugdp, 
                                  std::vector<double>* dlnfugdn, int* ophase, double* v);

	std::string get_model_id();
	int get_comp_index(const std::string &comp);
	std::vector<double> specific_volume(double T, double P, const std::vector<double> &composition, int phase_flag);
	double compmoleweight(int32_t comp, bool si_units = false);

protected:
	void add_eos();
	void delete_eos();
	int get_true_int_value();
	p_get_comp_index s_get_comp_index = nullptr;

private:
	void *tp_handle;
	int model_index;
	int _true_int_value;

	// Function pointers
	typedef void (*activate_model_func)(int *);
	typedef int (*add_eos_func)();
	typedef void (*delete_eos_func)(int *);
	typedef void (*get_model_id_func)(char *, size_t);
	typedef void (*init_thermo_func)(const char *, const char *, const char *, const char *,
									 int *, int *, const char *, const char *, const char *, const char *,
									 const char *, double *, const char *, int *, bool *, size_t, size_t,
									 size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t);
	typedef void (*init_volume_translation_func)(const char *, const char *, size_t, size_t);
	typedef void (*get_true_func)(int *);
	typedef void (*getCriticalParam_func)(int *, double *, double *, double *, double *, double *);
	typedef double (*bubble_p_func)(double *, double *, double *, int *);

	activate_model_func s_activate_model;
	add_eos_func s_add_eos;
	delete_eos_func s_delete_eos;
	get_model_id_func s_get_model_id;
	init_thermo_func eoslibinit_init_thermo;
	init_volume_translation_func eoslibinit_init_volume_translation;
	get_true_func s_get_true;
	getCriticalParam_func s_eos_getCriticalParam;
	bubble_p_func s_bubble_p;
	std::string get_export_name(const std::string &module, const std::string &method);
};

extern "C" {
	// Declare the function type expected from the Fortran library
	void __thermopack_var_MOD_activate_model(int *index);
	int __thermopack_var_MOD_add_eos();
	void __thermopack_var_MOD_delete_eos(int *index);
	void __eoslibinit_MOD_init_thermo(const char* eos, const char* mixing, const char* alpha, const char* comps,
									  int* nphases, int* liq_vap_discr_method, const char* csp_eos, const char* csp_ref_comp,
									  const char* kij_ref, const char* alpha_ref, const char* saft_ref, double* b_exponent,
									  const char* TrendEosForCp, int* cptype, bool* silent, size_t eos_len, size_t mixing_len,
									  size_t alpha_len, size_t comps_len, size_t csp_eos_len, size_t csp_ref_comp_len,
									  size_t kij_ref_len, size_t alpha_ref_len, size_t saft_ref_len, size_t TrendEosForCp_len);
	void __eoslibinit_MOD_init_volume_translation(const char* volume_trans_model, const char* param_ref, int volume_trans_model_len, int param_ref_len);
	int __compdata_MOD_comp_index_active(const char* compName, size_t compName_len);
	void __thermopack_constants_MOD_get_true(int *true_value);
	void __eos_MOD_getcriticalparam(int* i, double* tci, double* pci, double* w, double* vci, double* tnbi);
	double __saturation_MOD_safe_bubp(double* temp, double* z, double* y, int* ierr);
	void __eos_MOD_thermo(double* t, double* p, double* z, int* phase,
						  double* lnfug, double* lnfugt, double* lnfugp,
						  double* lnfugx, int* ophase, int* metaExtremum, double* v);
						  
	double __eos_MOD_compmoleweight(int32_t* comp);
						  
	void thermopack_specific_volume_c(double* temp, double* press, double* x, int* iphase, double* v);
}

#endif // THERMO_H
