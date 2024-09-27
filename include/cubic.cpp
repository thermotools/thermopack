#include "cubic.h"
#include <iostream>
#include <stdexcept>
#include <dlfcn.h>
#include <cstring>
#ifdef _WIN32
#include <windows.h>
#endif

// Constructor: Initialize the Cubic EoS
Cubic::Cubic(const std::string &comps, const std::string &eos, const std::string &mixing)
{
	load_library();
	init(comps, eos, mixing);
}

// Load thermopack library dynamically
void Cubic::load_library()
{
	tp_handle = dlopen("libthermopack.dll", RTLD_LAZY);
	if (!tp_handle)
	{
		throw std::runtime_error("Failed to load thermopack library.");
	}
}

// Unload thermopack library
void Cubic::unload_library()
{
	if (tp_handle)
	{
		dlclose(tp_handle);
	}
}

std::string Cubic::get_export_name(const std::string &module, const std::string &method) {
	if (module.empty()) {
		// If no module, just return the method with postfix for method name
		return method;
	} else {
		// Return in format: __module_MOD_method
		return prefix + module + postfix + method;
	}
}

// Init function
void Cubic::init(const std::string &comps, const std::string &eos, const std::string &mixing,
				 const std::string &alpha, const std::string &ref, bool volume_shift) {
	std::string export_name = get_export_name("eoslibinit", "init_cubic");

	typedef void (*init_cubic_func)(char*, char*, char*, char*, char*, int*, size_t, size_t, size_t, size_t, size_t);
	init_cubic_func init_cubic = (init_cubic_func)dlsym(tp_handle, export_name.c_str());

	if (!init_cubic) {
		const char* error_msg = dlerror();
		std::cerr << "Error: Could not load " << export_name << " from thermopack. "
				  << (error_msg ? error_msg : "Unknown error") << std::endl;
		throw std::runtime_error("Could not load init_cubic from thermopack.");
	}

	int vol_shift_int = volume_shift ? 1 : 0;

	init_cubic(const_cast<char*>(comps.c_str()), const_cast<char*>(eos.c_str()), const_cast<char*>(mixing.c_str()),
			   const_cast<char*>(alpha.c_str()), const_cast<char*>(ref.c_str()), &vol_shift_int,
			   comps.size(), eos.size(), mixing.size(), alpha.size(), ref.size());
}


void Cubic::init_pseudo(const std::string& comps, const std::vector<double>& Tclist, const std::vector<double>& Pclist, const std::vector<double>& acflist, const std::vector<double>& Mwlist, const std::string& mixing, const std::string& alpha) {
	using init_cubic_pseudo_func = void(*)(const char*, const double*, const double*, const double*, const double*, const char*, const char*, size_t, size_t, size_t);
	init_cubic_pseudo_func init_cubic_pseudo = (init_cubic_pseudo_func)dlsym(tp_handle, "__eoslibinit_MOD_init_cubic_pseudo");

	if (!init_cubic_pseudo) {
		const char* error_msg = dlerror();
		std::cerr << "Error: Could not load eoslibinit_MOD_init_cubic_pseudo from thermopack. " << (error_msg ? error_msg : "Unknown error") << std::endl;
		throw std::runtime_error("Could not load init_cubic_pseudo from thermopack.");
	}

	init_cubic_pseudo(comps.c_str(), Tclist.data(), Pclist.data(), acflist.data(), Mwlist.data(), 
					  mixing.c_str(), alpha.c_str(), 
					  comps.size(), mixing.size(), alpha.size());
}

double Cubic::get_kij(int c1, int c2)
{
	typedef void (*get_kij_func)(int *, int *, double *);
	get_kij_func get_kij = (get_kij_func)dlsym(tp_handle, "__saft_interface_MOD_pc_saft_get_kij");

	if (!get_kij)
	{
		throw std::runtime_error("Could not load thermopack_getkij from thermopack.");
	}

	double kij;
	get_kij(&c1, &c2, &kij);
	return kij;
}

void Cubic::set_kij(int c1, int c2, double kij)
{
	typedef void (*set_kij_func)(int *, int *, double *);
	set_kij_func set_kij = (set_kij_func)dlsym(tp_handle, "__saft_interface_MOD_cpa_set_kij");

	if (!set_kij)
	{
		throw std::runtime_error("Could not load thermopack_setkijandji from thermopack.");
	}

	set_kij(&c1, &c2, &kij);
}

double Cubic::get_lij(int c1, int c2)
{
	typedef void (*get_lij_func)(int *, int *, double *);
	get_lij_func get_lij = (get_lij_func)dlsym(tp_handle, "thermopack_getlij_");

	if (!get_lij)
	{
		throw std::runtime_error("Could not load thermopack_getlij from thermopack.");
	}

	double lij;
	get_lij(&c1, &c2, &lij);
	return lij;
}

void Cubic::set_lij(int c1, int c2, double lij)
{
	typedef void (*set_lij_func)(int *, int *, double *);
	set_lij_func set_lij = (set_lij_func)dlsym(tp_handle, "thermopack_setlijandji_");

	if (!set_lij)
	{
		throw std::runtime_error("Could not load thermopack_setlijandji from thermopack.");
	}

	set_lij(&c1, &c2, &lij);
}

void Cubic::get_hv_param(int c1, int c2, double &alpha_ij, double &alpha_ji, double &a_ij, double &a_ji, double &b_ij, double &b_ji, double &c_ij, double &c_ji)
{
	typedef void (*get_hv_param_func)(int *, int *, double *, double *, double *, double *, double *, double *, double *, double *);
	get_hv_param_func get_hv_param = (get_hv_param_func)dlsym(tp_handle, "thermopack_gethvparam_");

	if (!get_hv_param)
	{
		throw std::runtime_error("Could not load thermopack_gethvparam from thermopack.");
	}

	get_hv_param(&c1, &c2, &alpha_ij, &alpha_ji, &a_ij, &a_ji, &b_ij, &b_ji, &c_ij, &c_ji);
}

void Cubic::set_hv_param(int c1, int c2, double alpha_ij, double alpha_ji, double a_ij, double a_ji, double b_ij, double b_ji, double c_ij, double c_ji)
{
	typedef void (*set_hv_param_func)(int *, int *, double *, double *, double *, double *, double *, double *, double *, double *);
	set_hv_param_func set_hv_param = (set_hv_param_func)dlsym(tp_handle, "thermopack_sethvparam_");

	if (!set_hv_param)
	{
		throw std::runtime_error("Could not load thermopack_sethvparam from thermopack.");
	}

	set_hv_param(&c1, &c2, &alpha_ij, &alpha_ji, &a_ij, &a_ji, &b_ij, &b_ji, &c_ij, &c_ji);
}

void Cubic::get_ws_param(int c1, int c2, double &alpha_ij, double &alpha_ji, double &k_ij, double &k_ji, double &tau_ij, double &tau_ji)
{
	typedef void (*get_ws_param_func)(int *, int *, double *, double *, double *, double *, double *, double *);
	get_ws_param_func get_ws_param = (get_ws_param_func)dlsym(tp_handle, "thermopack_getwsparam_");

	if (!get_ws_param)
	{
		throw std::runtime_error("Could not load thermopack_getwsparam from thermopack.");
	}

	get_ws_param(&c1, &c2, &alpha_ij, &alpha_ji, &k_ij, &k_ji, &tau_ij, &tau_ji);
}

void Cubic::set_ws_param(int c1, int c2, double alpha_ij, double alpha_ji, double k_ij, double k_ji, double tau_ij, double tau_ji)
{
	typedef void (*set_ws_param_func)(int *, int *, double *, double *, double *, double *, double *, double *);
	set_ws_param_func set_ws_param = (set_ws_param_func)dlsym(tp_handle, "thermopack_setwsparam_");

	if (!set_ws_param)
	{
		throw std::runtime_error("Could not load thermopack_setwsparam from thermopack.");
	}

	set_ws_param(&c1, &c2, &alpha_ij, &alpha_ji, &k_ij, &k_ji, &tau_ij, &tau_ji);
}

void Cubic::get_ci(int cidx, double &ciA, double &ciB, double &ciC, int &ci_type)
{
	typedef void (*get_ci_func)(int *, double *, double *, double *, int *);
	get_ci_func get_ci = (get_ci_func)dlsym(tp_handle, "thermopack_get_volume_shift_parameters_");

	if (!get_ci)
	{
		throw std::runtime_error("Could not load thermopack_get_volume_shift_parameters from thermopack.");
	}

	get_ci(&cidx, &ciA, &ciB, &ciC, &ci_type);
}

void Cubic::set_ci(int cidx, double ciA, double ciB, double ciC, int ci_type)
{
	typedef void (*set_ci_func)(int *, double *, double *, double *, int *);
	set_ci_func set_ci = (set_ci_func)dlsym(tp_handle, "thermopack_set_volume_shift_parameters_");

	if (!set_ci)
	{
		throw std::runtime_error("Could not load thermopack_set_volume_shift_parameters from thermopack.");
	}

	set_ci(&cidx, &ciA, &ciB, &ciC, &ci_type);
}

std::vector<double> Cubic::get_covolumes()
{
	typedef void (*get_covolumes_func)(double *);
	get_covolumes_func get_covolumes = (get_covolumes_func)dlsym(tp_handle, "__cubic_eos_MOD_get_covolumes");

	if (!get_covolumes)
	{
		throw std::runtime_error("Could not load cubic_eos_get_covolumes from thermopack.");
	}

	std::vector<double> covolumes(nc, 0.0);
	get_covolumes(covolumes.data());

	return covolumes;
}

std::vector<double> Cubic::get_energy_constants()
{
	typedef void (*get_energy_constants_func)(double *);
	get_energy_constants_func get_energy_constants = (get_energy_constants_func)dlsym(tp_handle, "__cubic_eos_MOD_get_energy_constants");

	if (!get_energy_constants)
	{
		throw std::runtime_error("Could not load cubic_eos_get_energy_constants from thermopack.");
	}

	std::vector<double> energy_constants(nc, 0.0);
	get_energy_constants(energy_constants.data());

	return energy_constants;
}

void Cubic::set_alpha_corr(int ic, const std::string &corrname, const std::vector<double> &coeffs)
{
	typedef void (*set_alpha_corr_func)(int *, const char *, double *, int *);
	set_alpha_corr_func set_alpha_corr = (set_alpha_corr_func)dlsym(tp_handle, "thermopack_set_alpha_corr_");

	if (!set_alpha_corr)
	{
		throw std::runtime_error("Could not load thermopack_set_alpha_corr from thermopack.");
	}

	int num_coeffs = coeffs.size();
	set_alpha_corr(&ic, corrname.c_str(), const_cast<double *>(coeffs.data()), &num_coeffs);
}

void Cubic::set_beta_corr(int ic, const std::string &corrname, const std::vector<double> &coeffs)
{
	typedef void (*set_beta_corr_func)(int *, const char *, double *, int *);
	set_beta_corr_func set_beta_corr = (set_beta_corr_func)dlsym(tp_handle, "thermopack_set_beta_corr_");

	if (!set_beta_corr)
	{
		throw std::runtime_error("Could not load thermopack_set_beta_corr from thermopack.");
	}

	int num_coeffs = coeffs.size();
	set_beta_corr(&ic, corrname.c_str(), const_cast<double *>(coeffs.data()), &num_coeffs);
}
