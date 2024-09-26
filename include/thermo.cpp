#include "thermo.h"
#include <iostream>
#include <stdexcept>
#include <cstring>
#include <cstdlib>

Thermo::Thermo()
{
	std::cout << "Initializing Thermo object" << std::endl;
	tp_handle = dlopen("libthermopack.dll", RTLD_LAZY);
	std::cout << "tp_handle: " << tp_handle << std::endl;
	if (!tp_handle)
	{
		throw std::runtime_error("Could not load thermopack library");
	}

	s_activate_model = (activate_model_func)dlsym(tp_handle, "thermopack_var_activate_model");
	s_add_eos = (add_eos_func)dlsym(tp_handle, "__thermopack_var_MOD_add_eos");
	s_delete_eos = (delete_eos_func)dlsym(tp_handle, "thermopack_var_delete_eos");
	s_get_model_id = (get_model_id_func)dlsym(tp_handle, "thermopack_var_get_eos_identification");
	eoslibinit_init_thermo = (init_thermo_func)dlsym(tp_handle, "eoslibinit_init_thermo");
	eoslibinit_init_volume_translation = (init_volume_translation_func)dlsym(tp_handle, "eoslibinit_init_volume_translation");
	s_get_true = (get_true_func)dlsym(tp_handle, "__thermopack_constants_MOD_get_true");
	if (!s_get_true)
	{
		std::cerr << "Failed to load __thermopack_constants_MOD_get_true" << std::endl;
		throw std::runtime_error("Could not load __thermopack_constants_MOD_get_true.");
	}
	s_eos_getCriticalParam = (getCriticalParam_func)dlsym(tp_handle, "__eos_MOD_getcriticalparam");
	s_bubble_p = (bubble_p_func)dlsym(tp_handle, "__saturation_MOD_safe_bubp");

	std::cout << "Getting true int values" << std::endl;
	_true_int_value = get_true_int_value();
	std::cout << "Values got: " << _true_int_value << std::endl;
	std::cout << "Initializing Thermo object" << std::endl;
	s_get_comp_index = (p_get_comp_index)dlsym(tp_handle, "__compdata_MOD_comp_index_active");
	if (!s_get_comp_index)
	{
		std::cerr << "Failed to load __compdata_MOD_comp_index_active" << std::endl;
		throw std::runtime_error("Could not load comp_index_active function");
	}

	add_eos();
}

Thermo::~Thermo()
{
	delete_eos();
	dlclose(tp_handle);
}

std::string Thermo::get_export_name(const std::string &module, const std::string &method)
{
	return "thermopack_" + module + "_" + method;
}

int Thermo::get_comp_index(const std::string &comp)
{
	if (s_get_comp_index)
	{
		const char *comp_c = comp.c_str();
		size_t comp_len = comp.length();
		return s_get_comp_index(comp_c, comp_len);
	}
	throw std::runtime_error("get_comp_index function not loaded");
}

int Thermo::get_true_int_value()
{
	if (!s_get_true)
	{
		std::cerr << "Failed to load thermopack_constants_get_true" << std::endl;
		throw std::runtime_error("Could not load thermopack_constants_get_true.");
	}

	int int_true_c = 0;
	s_get_true(&int_true_c);
	return int_true_c;
}

void Thermo::add_eos()
{
	model_index = s_add_eos();
}

void Thermo::delete_eos()
{
	s_delete_eos(&model_index);
}

void Thermo::activate()
{
	s_activate_model(&model_index);
}

std::string Thermo::get_model_id()
{
	char eosid[40] = {0};
	s_get_model_id(eosid, sizeof(eosid));
	return std::string(eosid);
}

void Thermo::init_thermo(const std::string &eos, const std::string &mixing, const std::string &alpha,
						 const std::string &comps, int nphases, int *liq_vap_discr_method,
						 const std::string &csp_eos, const std::string &csp_ref_comp,
						 const std::string &kij_ref, const std::string &alpha_ref,
						 const std::string &saft_ref, double *b_exponent,
						 const std::string &TrendEosForCp, int *cptype, bool *silent)
{
	activate();

	char eos_c[80], mixing_c[80], alpha_c[80], comp_string_c[80];
	strncpy(eos_c, eos.c_str(), 80);
	strncpy(mixing_c, mixing.c_str(), 80);
	strncpy(alpha_c, alpha.c_str(), 80);
	strncpy(comp_string_c, comps.c_str(), 80);

	eoslibinit_init_thermo(eos_c, mixing_c, alpha_c, comp_string_c, &nphases, liq_vap_discr_method,
						   csp_eos.c_str(), csp_ref_comp.c_str(), kij_ref.c_str(), alpha_ref.c_str(),
						   saft_ref.c_str(), b_exponent, TrendEosForCp.c_str(), cptype, silent,
						   eos.length(), mixing.length(), alpha.length(), comps.length(),
						   csp_eos.length(), csp_ref_comp.length(), kij_ref.length(), alpha_ref.length(),
						   saft_ref.length(), TrendEosForCp.length());
}

void Thermo::init_peneloux_volume_translation(const std::string &parameter_reference)
{
	activate();

	const char *volume_trans_model = "PENELOUX";
	eoslibinit_init_volume_translation(volume_trans_model, parameter_reference.c_str(),
									   strlen(volume_trans_model), parameter_reference.length());
}

double Thermo::critical_temperature(int i)
{
	activate();

	int comp_c = i;
	double tci = 0.0, pci = 0.0, w = 0.0, vci = 0.0, tnbi = 0.0;

	s_eos_getCriticalParam(&comp_c, &tci, &pci, &w, &vci, &tnbi);

	return tci;
}

std::pair<double, std::vector<double>> Thermo::bubble_pressure(double temp, const std::vector<double> &z)
{
	activate();

	double temp_c = temp;
	std::vector<double> y(z.size(), 0.0);
	std::vector<double> z_c = z;
	int ierr = 0;

	double press = s_bubble_p(&temp_c, z_c.data(), y.data(), &ierr);

	if (ierr != 0)
	{
		throw std::runtime_error("bubble_pressure calculation failed");
	}

	return {press, y};
}

std::vector<double> Thermo::thermo(double T, double P, const std::vector<double> &composition, int phase_flag)
{
	activate();
	double T_c = T;
	double P_c = P;
	int phase_c = phase_flag;
	std::vector<double> composition_c = composition;
	std::vector<double> result(composition.size(), 0.0);

	typedef void (*thermo_func)(double *, double *, double *, int *, double *);
	thermo_func s_thermo = (thermo_func)dlsym(tp_handle, "eos_thermo");

	if (!s_thermo)
	{
		throw std::runtime_error("Could not load eos_thermo from thermopack.");
	}

	s_thermo(&T_c, &P_c, composition_c.data(), &phase_c, result.data());

	return result;
}
