#ifndef THERMO_H
#define THERMO_H

#include <string>
#include <vector>
#include <dlfcn.h>
#include <stdexcept>
#include <cstring>


typedef int (*p_get_comp_index)(const char*, size_t);

class Thermo {
public:
	Thermo();
	virtual ~Thermo();

	int TWOPH = 2;
	int LIQPH = 1;
	int VAPPH = 0;
	
	void activate();
	void init_thermo(const std::string& eos, const std::string& mixing, const std::string& alpha,
					 const std::string& comps, int nphases, int* liq_vap_discr_method = nullptr,
					 const std::string& csp_eos = "", const std::string& csp_ref_comp = "",
					 const std::string& kij_ref = "Default", const std::string& alpha_ref = "Default",
					 const std::string& saft_ref = "Default", double* b_exponent = nullptr,
					 const std::string& TrendEosForCp = "", int* cptype = nullptr,
					 bool* silent = nullptr);

	void init_peneloux_volume_translation(const std::string& parameter_reference = "Default");
	double critical_temperature(int i);
	std::pair<double, std::vector<double>> bubble_pressure(double temp, const std::vector<double>& z);
	std::vector<double> thermo(double T, double P, const std::vector<double>& composition, int phase_flag);

	std::string get_model_id();
	int get_comp_index(const std::string &comp);
	
protected:
	void add_eos();
	void delete_eos();
	int get_true_int_value();
	p_get_comp_index s_get_comp_index = nullptr;

private:
	void* tp_handle;
	int model_index;
	int _true_int_value;

	typedef void (*activate_model_func)(int*);
	typedef int (*add_eos_func)();
	typedef void (*delete_eos_func)(int*);
	typedef void (*get_model_id_func)(char*, size_t);
	typedef void (*init_thermo_func)(const char*, const char*, const char*, const char*,
									 int*, int*, const char*, const char*, const char*, const char*,
									 const char*, double*, const char*, int*, bool*, size_t, size_t,
									 size_t, size_t, size_t, size_t, size_t, size_t, size_t, size_t);
	typedef void (*init_volume_translation_func)(const char*, const char*, size_t, size_t);
	typedef void (*get_true_func)(int*);
	typedef void (*getCriticalParam_func)(int*, double*, double*, double*, double*, double*);
	typedef double (*bubble_p_func)(double*, double*, double*, int*);

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

#endif // THERMO_H
