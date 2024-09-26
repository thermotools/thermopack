#ifndef CUBIC_H
#define CUBIC_H

#include <string>
#include <vector>
#include "thermo.h"

class Cubic : public Thermo {
public:
	Cubic(const std::string& comps, const std::string& eos, const std::string& mixing);
	void init(const std::string& comps, const std::string& eos, const std::string& mixing = "vdW", const std::string& alpha = "Classic", const std::string& parameter_reference = "Default", bool volume_shift = false);
	void init_pseudo(const std::string& comps, const std::vector<double>& Tclist, const std::vector<double>& Pclist, const std::vector<double>& acflist, const std::vector<double>& Mwlist, const std::string& mixing = "vdW", const std::string& alpha = "Classic");

	double get_kij(int c1, int c2);
	void set_kij(int c1, int c2, double kij);

	double get_lij(int c1, int c2);
	void set_lij(int c1, int c2, double lij);

	void get_hv_param(int c1, int c2, double& alpha_ij, double& alpha_ji, double& a_ij, double& a_ji, double& b_ij, double& b_ji, double& c_ij, double& c_ji);
	void set_hv_param(int c1, int c2, double alpha_ij, double alpha_ji, double a_ij, double a_ji, double b_ij, double b_ji, double c_ij, double c_ji);

	void get_ws_param(int c1, int c2, double& alpha_ij, double& alpha_ji, double& k_ij, double& k_ji, double& tau_ij, double& tau_ji);
	void set_ws_param(int c1, int c2, double alpha_ij, double alpha_ji, double k_ij, double k_ji, double tau_ij, double tau_ji);

	void get_ci(int cidx, double& ciA, double& ciB, double& ciC, int& ci_type);
	void set_ci(int cidx, double ciA, double ciB = 0.0, double ciC = 0.0, int ci_type = 1);

	std::vector<double> get_covolumes();
	std::vector<double> get_energy_constants();

	void set_alpha_corr(int ic, const std::string& corrname, const std::vector<double>& coeffs);
	void set_beta_corr(int ic, const std::string& corrname, const std::vector<double>& coeffs);
	
    using Thermo::Thermo;
    
    void init_peneloux_volume_translation(const std::string& parameter_reference = "Default") {
        Thermo::init_peneloux_volume_translation(parameter_reference);
    }

private:
	void* tp_handle; 
	int nc;           

	void load_library();
	void unload_library();
};

#endif // CUBIC_H
