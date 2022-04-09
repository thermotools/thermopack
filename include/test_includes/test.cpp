#include <iostream>
#include <vector>
#include <cmath>
#include "../thermopack.h"

// Simple code testing thermopack.h
int main() {
    std::cout << "Testing interface to cpp\n";
    int nph(2);
    const int ncomp(3);

    std::cout << "\nMemory sizes\n";
    std::cout << "sizeof(double): " << sizeof(double) << std::endl;
    std::cout << "sizeof(int): " << sizeof(int) << std::endl;

    std::cout << "\nPhase flags\n";
    int iTWOPH(0), iLIQPH(0), iVAPPH(0), iMINGIBBSPH(0),
      iSINGLEPH(0), iSOLIDPH(0), iFAKEPH(0);
    ISO_C_METHOD(get_phase_flags_c)(&iTWOPH, &iLIQPH, &iVAPPH, &iMINGIBBSPH,
				    &iSINGLEPH, &iSOLIDPH, &iFAKEPH);

    std::cout << "iTWOPH=" << iTWOPH << std::endl;
    std::cout << "iLIQPH=" << iLIQPH << std::endl;
    std::cout << "iVAPPH=" << iVAPPH << std::endl;
    std::cout << "iMINGIBBSPH=" << iMINGIBBSPH << std::endl;
    std::cout << "iSINGLEPH=" << iSINGLEPH << std::endl;
    std::cout << "iSOLIDPH=" << iSOLIDPH << std::endl;
    std::cout << "iFAKEPH=" << iFAKEPH << std::endl;

    std::cout << "\nInit of SRK\n";
    ISO_C_METHOD(thermopack_init_c)("SRK", "Classic",
				    "Classic", "CO2,N2,C1",
				    &nph, NULL, NULL, NULL,
				    NULL, NULL, NULL, NULL);


    std::cout << "\nConstants\n";
    TMIN = 100.0;
    std::cout << "Rgas=" << RGAS << std::endl;
    std::cout << "Tmin=" << TMIN << std::endl;
    std::cout << "Tmax=" << TMAX << std::endl;

    std::cout << "\nTP flash\n";
    std::vector<double> z(ncomp);
    z[0] = 0.9;
    z[1] = 0.05;
    z[2] = 0.05;
    std::vector<double> x(ncomp), y(ncomp);
    double beta(0.0);
    int phase(0);
    double T(280.0);
    double P(6.0e6);
    ISO_C_METHOD(thermopack_tpflash_c)(&T, &P, &z[0], &beta, &phase, &x[0], &y[0]);
    std::cout << "beta=" << beta << std::endl;
    std::cout << "phase=" << phase << std::endl;
    std::cout << "x=" << x[0] << ", " << x[1] << ", " << x[2] << std::endl;
    std::cout << "y=" << y[0] << ", " << y[1] << ", " << y[2] << std::endl;

    std::cout << "\nSpecific volume\n";
    int iphgas(iVAPPH);
    double vg(0.0);
    ISO_C_METHOD(thermopack_specific_volume_c)(&T, &P, &y[0], &iphgas, &vg);
    int iphliq(iLIQPH);
    double vl(0.0);
    ISO_C_METHOD(thermopack_specific_volume_c)(&T, &P, &x[0], &iphliq, &vl);
    std::cout << "vg=" << vg << std::endl;
    std::cout << "vl=" << vl << std::endl;
    // Define mixture specific volume for later
    double v(vg*beta+vl*(1.0-beta));

    std::cout << "\nGuess phase\n";
    int iph(0);
    ISO_C_METHOD(thermopack_guess_phase_c)(&T, &P, &x[0], &iph);
    std::cout << "iph=" << iph << std::endl;

    std::cout << "\nPressure\n";
    double Pg(0.0);
    ISO_C_METHOD(thermopack_pressure_c)(&T, &vg, &y[0], &Pg);
    std::cout << "Pg=" << Pg << std::endl;
    double Pl = MODULE_METHOD(eostv, pressure)(&T, &vl,&x[0], NULL, NULL, NULL, NULL, NULL);
    std::cout << "Pl=" << Pl << std::endl;

    std::cout << "\nCompressibillit factor\n";
    double zg(0.0), zl(0.0);
    ISO_C_METHOD(thermopack_zfac_c)(&T, &P, &x[0], &iphliq, &zl);
    ISO_C_METHOD(thermopack_zfac_c)(&T, &P, &y[0], &iphgas, &zg);
    std::cout << "zg, Pv/nRT: " << zg << ", " << P*vg/(RGAS*T) << std::endl;
    std::cout << "zl, Pv/nRT: " << zl << ", " << P*vl/(RGAS*T) << std::endl;

    std::cout << "\nSpecific enthalpy\n";
    double hg(0.0);
    ISO_C_METHOD(thermopack_enthalpy_c)(&T, &P, &y[0], &iphgas, &hg, NULL, NULL, NULL);
    double hl(0.0);
    ISO_C_METHOD(thermopack_enthalpy_c)(&T, &P, &x[0], &iphliq, &hl, NULL, NULL, NULL);
    std::cout << "hg=" << hg << std::endl;
    std::cout << "hl=" << hl << std::endl;
    // Define enthalpy for later
    double h(hg*beta+hl*(1.0-beta));
    std::cout << "h=" << h << std::endl;
    std::cout << "beta=" << beta << std::endl;
    // Define internal energy for later
    double u(h-P*v);

    std::cout << "\nSpecific entropy\n";
    double sg(0.0);
    ISO_C_METHOD(thermopack_entropy_c)(&T, &P, &y[0], &iphgas, &sg);
    double sl(0.0);
    ISO_C_METHOD(thermopack_entropy_c)(&T, &P, &x[0], &iphliq, &sl);
    std::cout << "sg=" << sg << std::endl;
    std::cout << "sl=" << sl << std::endl;
    // Define overall entropy for later
    double s(sg*beta+sl*(1.0-beta));

    std::cout << "\nEntropy tvn\n";
    double sg_tv(0.0);
    ISO_C_METHOD(thermopack_entropy_tv_c)(&T, &vg, &y[0], &sg_tv);
    std::cout << "sg_tv=" << sg_tv << std::endl;

    std::cout << "\nBubble temperature\n";
    int ierr(0);
    std::vector<double> x_bub, y_bub(ncomp);
    double T_bub(0.0);
    double P_bub(6.0e6);
    x_bub = z;
    ISO_C_METHOD(thermopack_bubt_c)(&P_bub, &x_bub[0], &y_bub[0], &T_bub, &ierr);
    std::cout << "T_bub=" << T_bub << std::endl;

    std::cout << "Bubble pressure\n";
    P_bub = 0.0;
    ISO_C_METHOD(thermopack_bubp_c)(&T_bub, &x_bub[0], &y_bub[0], &P_bub, &ierr);
    std::cout << "P_bub=" << P_bub << std::endl;

    std::cout << "\nDew temperature\n";
    std::vector<double> x_dew(ncomp), y_dew(ncomp);
    double T_dew(0.0);
    double P_dew(6.0e6);
    y_dew = z;
    ISO_C_METHOD(thermopack_dewt_c)(&P_dew, &x_dew[0], &y_dew[0], &T_dew, &ierr);
    std::cout << "T_dew=" << T_dew << std::endl;

    std::cout << "\nDew pressure\n";
    P_dew = 0.0;
    ISO_C_METHOD(thermopack_dewp_c)(&T_dew, &x_dew[0], &y_dew[0], &P_dew, &ierr);
    std::cout << "P_dew=" << P_dew << std::endl;

    std::cout << "\nPure saturation\n";
    double T_pure_sat(0.0);
    ISO_C_METHOD(thermopack_puresat_t_c)(&P, &z[0], &T_pure_sat, &ierr);
    std::cout << "T_pure_sat=" << T_pure_sat << std::endl;

    std::cout << "\nWilson-K\n";
    std::vector<double> k(ncomp);
    ISO_C_METHOD(thermopack_wilsonk_c)(&T, &T, &k[0]);
    std::cout << "k=" << k[0] << ", " << k[1] << ", " << k[2] << std::endl;
    double lnPhi_offset(0.0);
    for (int i = 1 ; i <= ncomp; ++i) {
      double ki(0.0);
      ISO_C_METHOD(thermopack_wilsonki_c)(&i, &T, &T, &lnPhi_offset, &ki);
      std::cout << "ki=" << ki << std::endl;
    }

    std::cout << "\nComponent mol weights (kg/mol)\n";
    double mwz(0.0), mwz_sum(0.0);
    ISO_C_METHOD(thermopack_moleweight_c)(&z[0], &mwz);
    std::vector<double> mw(ncomp);
    for (std::vector<double>::iterator it = mw.begin() ; it != mw.end(); ++it) {
      int fortran_index = it - mw.begin() + 1;
      ISO_C_METHOD(thermopack_compmoleweight_c)(&fortran_index, &it[0]);
      mwz_sum += it[0]*z[fortran_index-1];
    }
    std::cout << "mw=" << mwz << ", " << mwz_sum << std::endl;

    std::cout << "\nPure component parameters\n";
    for (int i = 1 ; i <= ncomp; ++i) {
      double Tc(0.0), Pc(0.0), w(0.0);
      ISO_C_METHOD(thermopack_getcriticalparam_c)(&i, &Tc, &Pc, &w);
      char* comp_name = NULL;
      ISO_C_METHOD(thermopack_comp_name_c)(&i, &comp_name);
      std::cout << "Comp. " << comp_name << std::endl;
      std::cout << "Tc=" << Tc << " (K)" << std::endl;
      std::cout << "Pc=" << Pc*1e-6 << " (MPa)" << std::endl;
      std::cout << "w=" << w << std::endl;
    }

    std::cout << "\nHP-flash\n";
    double betal(0.0);
    T = 300.0;
    ISO_C_METHOD(thermopack_hpflash_c)(&T, &P, &z[0],
				       &beta, &betal, &x[0], &y[0],
				       &h, &phase, &ierr);
    std::cout << "beta=" << beta << std::endl;
    std::cout << "T_hp=" << T << std::endl;

    std::cout << "\nSP-flash\n";
    T = 300.0;
    ISO_C_METHOD(thermopack_spflash_c)(&T, &P, &z[0],
				       &beta, &betal, &x[0], &y[0],
				       &s, &phase, &ierr);
    std::cout << "beta=" << beta << std::endl;
    std::cout << "T_sp=" << T << std::endl;

    std::cout << "\nUV-flash\n";
    T = 300.0;
    P = 50.0e5;
    ISO_C_METHOD(thermopack_uvflash_c)(&T, &P, &z[0],
				       &beta, &x[0], &y[0],
				       &u, &v, &phase, &ierr);
    std::cout << "beta=" << beta << std::endl;
    std::cout << "T_uv=" << T << std::endl;
    std::cout << "P_uv=" << P << std::endl;
    std::cout << "ierr=" << ierr << std::endl;

    std::cout << "\ndhdt\n";
    double dhdt(0.0);
    ISO_C_METHOD(thermopack_twophase_dhdt_c)(&T, &P, &z[0], &x[0], &y[0], &beta, &betal, &dhdt);
    double dT(T*1.0e-5);
    T += dT;
    ISO_C_METHOD(thermopack_tpflash_c)(&T, &P, &z[0], &beta, &phase, &x[0], &y[0]);
    ISO_C_METHOD(thermopack_enthalpy_c)(&T, &P, &y[0], &iphgas, &hg, NULL, NULL, NULL);
    ISO_C_METHOD(thermopack_enthalpy_c)(&T, &P, &x[0], &iphliq, &hl, NULL, NULL, NULL);
    double hp(hg*beta+hl*(1.0-beta));
    std::cout << "dhdt=" << dhdt << ", " << (hp-h)/dT <<  std::endl;

    std::cout << "\nThermo\n";
    std::vector<double> lnfug_l(ncomp), lnfug_g(ncomp);
    ISO_C_METHOD(thermopack_thermo_c)(&T, &P, &x[0], &iphliq, &lnfug_l[0], NULL, NULL, NULL, NULL);
    ISO_C_METHOD(thermopack_thermo_c)(&T, &P, &y[0], &iphgas, &lnfug_g[0], NULL, NULL, NULL, NULL);
    std::cout << "lnfug_l + log(x)"
	      << lnfug_l[0]+log(x[0]) << ", "
	      << lnfug_l[1]+log(x[1]) << ", "
	      << lnfug_l[2]+log(x[2]) << std::endl;
    std::cout << "lnfug_g + log(y)="
	      << lnfug_g[0]+log(y[0]) << ", "
	      << lnfug_g[1]+log(y[1]) << ", "
	      << lnfug_g [2]+log(y[2]) << std::endl;

    return 0;
}
