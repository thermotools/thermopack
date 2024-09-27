#include "thermo.h"
#include <iostream>
#include <stdexcept>
#include <cstring>
#include <cstdlib>

Thermo::Thermo()
{
	std::cout << "Initializing Thermo object" << std::endl;

	add_eos();
	_true_int_value = get_true_int_value();
}

Thermo::~Thermo()
{
	delete_eos();
}

int Thermo::get_comp_index(const std::string &comp)
{
	const char *comp_c = comp.c_str();
	size_t comp_len = comp.length();
	
	int index = __compdata_MOD_comp_index_active(comp_c, comp_len);

	if (index < 0)
	{
		throw std::runtime_error("Component not found: " + comp);
	}

	return index;
}

int Thermo::get_true_int_value()
{
	int int_true_c = 0;

	__thermopack_constants_MOD_get_true(&int_true_c);

	return int_true_c;
}

void Thermo::add_eos()
{
	model_index = __thermopack_var_MOD_add_eos();
}

void Thermo::delete_eos()
{
	__thermopack_var_MOD_delete_eos(&model_index);
}

void Thermo::activate()
{
	__thermopack_var_MOD_activate_model(&model_index);
}

std::string Thermo::get_model_id()
{
	char eosid[40] = {0}; // Allocate a char buffer for the EOS ID

	s_get_model_id(eosid, sizeof(eosid));

	return std::string(eosid); // Convert the char buffer to std::string
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

	__eoslibinit_MOD_init_thermo(eos_c, mixing_c, alpha_c, comp_string_c, &nphases, liq_vap_discr_method,
								 csp_eos.c_str(), csp_ref_comp.c_str(), kij_ref.c_str(), alpha_ref.c_str(),
								 saft_ref.c_str(), b_exponent, TrendEosForCp.c_str(), cptype, silent,
								 eos.length(), mixing.length(), alpha.length(), comps.length(),
								 csp_eos.length(), csp_ref_comp.length(), kij_ref.length(), alpha_ref.length(),
								 saft_ref.length(), TrendEosForCp.length());
}

void Thermo::init_peneloux_volume_translation(const std::string &parameter_reference) {
	
	activate();

	std::string volume_trans_model = "PENELOUX";
	const char *volume_trans_model_c = volume_trans_model.c_str();
	const char *ref_string_c = parameter_reference.c_str();

	__eoslibinit_MOD_init_volume_translation(volume_trans_model_c, ref_string_c, 
											 volume_trans_model.size(), parameter_reference.size());
}

double Thermo::critical_temperature(int i)
{
	activate();

	int comp_c = i;
	double tci = 0.0, pci = 0.0, w = 0.0, vci = 0.0, tnbi = 0.0;

	__eos_MOD_getcriticalparam(&comp_c, &tci, &pci, &w, &vci, &tnbi);

	return tci;
}

std::pair<double, std::vector<double>> Thermo::bubble_pressure(double temp, const std::vector<double> &z)
{
	
	activate();

	double temp_c = temp;
	std::vector<double> y(z.size(), 0.0);
	std::vector<double> z_c = z;
	int ierr = 0;

	double press = __saturation_MOD_safe_bubp(&temp_c, z_c.data(), y.data(), &ierr);

	if (ierr != 0)
	{
		throw std::runtime_error("Bubble pressure calculation failed");
	}

	return {press, y};
}

std::vector<double> Thermo::thermo(double T, double P, const std::vector<double>& composition, int phase_flag) {
	activate();

	int nc = composition.size();  // Number of components

	std::vector<double> lnfug(nc, 0.0);  // Fugacity coefficients

	std::vector<double> z = composition;  // Copy composition array
	
	double temp = T;
	double press = P;
	int phase = phase_flag;

	double* lnfugt = nullptr;     // Temperature differentials (optional)
	double* lnfugp = nullptr;     // Pressure differentials (optional)
	double* lnfugx = nullptr;     // Composition differentials (optional)
	int* ophase = nullptr;        // Phase identifier for MINGIBBSPH (optional)
	int* metaExtremum = nullptr;  // MetaExtremum flag (optional)
	double* v = nullptr;          // Specific volume (optional)

	__eos_MOD_thermo(&temp, &press, z.data(), &phase, lnfug.data(), lnfugt, lnfugp, lnfugx, ophase, metaExtremum, v);

	return lnfug;
}

