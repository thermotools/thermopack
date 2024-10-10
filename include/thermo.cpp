#include "thermo.h"
#include <iostream>
#include <stdexcept>
#include <cstring>
#include <cstdlib>

Thermo::Thermo()
{
	add_eos();
	_true_int_value = get_true_int_value();
}

Thermo::~Thermo()
{
	delete_eos();
}

int Thermo::get_comp_index(const std::string &comp)
{
	// Ensure string is properly null-terminated
	const char *comp_c = comp.c_str();
	size_t comp_len = comp.length();
	
	// Call the Fortran function with the component name and its length
	int index = __compdata_MOD_comp_index_active(comp_c, comp_len);

	// Check if the index is valid
	if (index < 0)
	{
		// If the component is not found, throw a more detailed error
		throw std::runtime_error("Component not found: '" + comp + "' (Length: " + std::to_string(comp_len) + ")");
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

void Thermo::activate() {
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

std::vector<double> Thermo::thermo(double T, double P, const std::vector<double> &composition, int phase_flag, 
								  std::vector<double>* dlnfugdt, std::vector<double>* dlnfugdp, 
								  std::vector<double>* dlnfugdn, int* ophase, double* v) 
{
	activate();  // Activate the EOS model

	int nc = composition.size();  // Number of components
	std::vector<double> lnfug(nc, 0.0);  // Fugacity coefficients (output)

	// Prepare inputs for the Fortran function
	std::vector<double> z = composition;  // Molar composition
	double temp = T;
	double press = P;
	int phase = phase_flag;

	// Pointers to pass optional outputs
	double* lnfugt_c = nullptr;
	double* lnfugp_c = nullptr;
	double* lnfugx_c = nullptr;
	int* ophase_c = ophase;
	int* metaExtremum_c = nullptr;  // Set to nullptr as it's unused

	// If dlnfugdt is provided, allocate memory for Fortran to store the result
	if (dlnfugdt && dlnfugdt->size() == nc) {
		lnfugt_c = dlnfugdt->data();
	}

	// If dlnfugdp is provided, allocate memory for Fortran to store the result
	if (dlnfugdp && dlnfugdp->size() == nc) {
		lnfugp_c = dlnfugdp->data();
	}

	// If dlnfugdn is provided, allocate memory for Fortran to store the result as a 1D array
	if (dlnfugdn && dlnfugdn->size() == nc * nc) {
		lnfugx_c = dlnfugdn->data();
	}

	// Call the Fortran subroutine
	__eos_MOD_thermo(&temp, &press, z.data(), &phase, lnfug.data(), lnfugt_c, lnfugp_c, lnfugx_c, ophase_c, metaExtremum_c, v);

	// Return the calculated fugacity coefficients
	return lnfug;
}


std::vector<double> Thermo::specific_volume(double T, double P, const std::vector<double> &composition, int phase_flag)
{	
	activate();
	
	std::vector<double> result(1);  // Only one specific volume is returned
	double v = 0.0;
	
	
	double* x = const_cast<double*>(composition.data());  
	int iphase = phase_flag;

	
	thermopack_specific_volume_c(&T, &P, x, &iphase, &v);

	result[0] = v;
	return result;
}

double Thermo::compmoleweight(int32_t comp, bool si_units) {
	// Ensure the Fortran function is available
	if (comp <= 0) {
		throw std::invalid_argument("Component index must be positive.");
	}

	// Call the Fortran function using the wrapper
	double mw = __eos_MOD_compmoleweight(&comp);

	// Convert to SI units if needed
	if (si_units) {
		mw *= 1e-3;  // Convert g/mol to kg/mol
	}

	return mw;
}

