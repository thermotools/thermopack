#include <stdio.h>
#include <dlfcn.h>
#include <iostream>
#include <stdlib.h>
#include <cstdlib>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>
#ifdef USEOMP
#include <omp.h>
#include <sys/sendfile.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif

#ifdef __linux__
// Allow for compatibillity with older versions of glibc
// To enable, link with option: -Wl,--wrap=memcpy
extern "C" void *__memcpy_glibc_2_2_5(void *, const void *, size_t);

asm(".symver __memcpy_glibc_2_2_5, memcpy@GLIBC_2.2.5");
extern "C" void *__wrap_memcpy(void *dest, const void *src, size_t n)
{
  return __memcpy_glibc_2_2_5(dest, src, n);
}
#endif

extern "C" {
  // TREND functions
  typedef void (*f_trend_init_no_char)(int* nc,int path[],int* npath,
                                       int comps[],int* ncomps,int *mix,
                                       double* Rgas);
  typedef void (*f_trend_enthalpy)(double* t,double* rho,
                                   double x[],double* h,double* dhdt,
                                   double* dhdp,double dhdx[]);
  typedef void (*f_trend_entropy)(double* t,double* rho,
                                  double x[],double* s,double* dsdt,
                                  double* dsdp,double dsdx[]);
  typedef void (*f_trend_specificVolume)(double* t,
                                         double x[],double* v,double* dvdt,
                                         double* dvdp,double dvdx[]);
  typedef void (*f_trend_thermo_dens)(double* t,double* rho,
                                      double x[],double lnfug[],
                                      double* lnfugt,
                                      double* lnfugp,double* lnfugx);
  typedef double (*f_trend_dpdrho)(double* x,double* t,double* rho);
  typedef double (*f_trend_d2pdrho2)(double x[],double* t,double* rho);
  typedef double (*f_trend_compMoleWeight)(int* j);
  typedef double (*f_trend_moleWeight)(double z[]);
  typedef void (*f_trend_getcrit)(int* i,double* tci,double* pci,
                                  double* oi,double* vci,double* tnbi);
  typedef void (*f_trend_rhomax_srk)(double x[], double* rhomax);
  typedef double (*f_trend_pressure)(double n[], double* t, double* v,
                                     double* dpdv, double* dpdt,
                                     double* dpdn);
  typedef double (*f_trend_psrk)(double* t, double* p, double x[],
                                 int* phase);
  typedef void (*f_trend_zfac)(double* t,double* rho,double x[],double* zfac,
                               double* dzdt, double* dzdp, double dzdx[]);
  typedef void (*f_trend_residualgibbs_TP)(double* t,double* p,double* rho,
                                           double x[],double* gr,
                                           double* dgrdt,double* dgrdp);
  typedef void (*f_trend_residualenthalpy_TP)(double* t,double* p,double* rho,
                                              double x[], double* hr,double* dhrdt,
                                              double* dhrdp, double dhrdx[]);
  typedef void (*f_trend_residualentropy_TP)(double* t,double* p,double* rho,
                                             double x[],double* sr,double* dsrdt,
                                             double* dsrdp, double dsrdx[]);
  typedef double (*f_trend_speedofsound)(double* t,double* rho,double x[]);
  typedef double (*f_trend_ideal_Cp)(double* t,int* j);
  typedef double (*f_trend_ideal_enthalpy)(double* t,int* j);
  typedef double (*f_trend_ideal_entropy)(double* t,double* p, int* j);
  typedef void (*f_trend_get_binary_parameters)(int* i1, int* i2,
                                                double ParRedFun[],
                                                int ndf[],
                                                double ParDepFun[]);
  typedef void (*f_trend_set_binary_parameters)(int* i1, int* i2,
                                                double ParRedFun[],
                                                int ndf[],
                                                double ParDepFun[]);
  typedef void (*f_trend_getreducedrhot)(double* t, double x[],
                                         double* rhored, double *tred);
  typedef double (*f_trend_free_energy)(double x[], double* t, double* v,
                                        double* dydv, double* dydt,
                                        double* d2ydt2, double* d2ydv2,
                                        double* d2ydvdt);
  typedef double (*f_trend_internal_energy)(double x[], double* t, double* v,
                                            double* dudv, double* dudt);

  typedef void (*f_trend_thermotv)(double* t, double* v, double n[],
                                   double lnfug[], double lnfugT[],
                                   double lnfugV[], double* lnfugn);

  typedef void (*f_trend_calcfres)(double* t, double* v, double n[],
                                   double* F, double* F_T, double* F_V,
                                   double F_n[], double* F_TT, double* F_TV,
                                   double* F_VV, double F_Tn[],
                                   double F_Vn[], double* F_nn);

  typedef void (*f_trend_calcfid)(double* t, double* v, double n[],
                                  double* F, double* F_T, double* F_V,
                                  double* F_TT, double* F_TV,
                                  double* F_VV, double* F_n,
                                  double* F_Tn, double* F_Vn,
                                  double* F_nn);
  typedef double (*f_trend_specvol)(double* T, double* P, double n[],
                                    int* phase);
  typedef int (*f_trend_voltest)(double* T, double* P, double* V,
                                 double n[], int* phase);
  typedef double (*f_trend_rmix)(double z[]);
  typedef double (*f_trend_aux_sat_prop)(double* T, int* i, double* P,
                                         double* vv, double* vl);
  typedef double (*f_trend_ideal)(double* T, int* i, double* Cp,
                                  double* h, double* s);
}

struct trend_pointers {
  std::string filename;
  void* handle;
  f_trend_init_no_char fptr_trend_init_no_char;
  f_trend_enthalpy fptr_trend_enthalpy;
  f_trend_entropy fptr_trend_entropy;
  f_trend_specificVolume fptr_trend_specificVolume;
  f_trend_thermo_dens fptr_trend_thermo_dens;
  f_trend_dpdrho fptr_trend_dpdrho;
  f_trend_d2pdrho2 fptr_trend_d2pdrho2;
  f_trend_compMoleWeight fptr_trend_compMoleWeight;
  f_trend_moleWeight fptr_trend_moleWeight;
  f_trend_getcrit fptr_trend_getcrit;
  f_trend_rhomax_srk fptr_trend_rhomax_srk;
  f_trend_pressure fptr_trend_pressure;
  f_trend_psrk fptr_trend_psrk;
  f_trend_zfac fptr_trend_zfac;
  f_trend_residualgibbs_TP fptr_trend_residualgibbs_TP;
  f_trend_residualenthalpy_TP fptr_trend_residualenthalpy_TP;
  f_trend_residualentropy_TP fptr_trend_residualentropy_TP;
  f_trend_speedofsound fptr_trend_speedofsound;
  f_trend_ideal_Cp fptr_trend_ideal_Cp;
  f_trend_ideal_enthalpy fptr_trend_ideal_enthalpy;
  f_trend_ideal_entropy fptr_trend_ideal_entropy;
  f_trend_get_binary_parameters fptr_trend_get_binary_parameters;
  f_trend_set_binary_parameters fptr_trend_set_binary_parameters;
  f_trend_getreducedrhot fptr_trend_getreducedrhot;
  f_trend_free_energy fptr_trend_free_energy;
  f_trend_internal_energy fptr_trend_internal_energy;
  f_trend_thermotv fptr_trend_thermotv;
  f_trend_calcfres fptr_trend_calcfres;
  f_trend_calcfid fptr_trend_calcfid;
  f_trend_specvol fptr_trend_specvol;
  f_trend_voltest fptr_trend_voltest;
  f_trend_rmix fptr_trend_rmix;
  f_trend_aux_sat_prop fptr_trend_aux_sat_prop;
  f_trend_ideal fptr_trend_ideal;
};

static std::vector<trend_pointers> trend_if;

void load_trend_so_and_pointers(const std::string& path,
                                const std::string& so_name,
                                trend_pointers& tptrs)
{
  //std::cout << "Loading " << so_name << "\n";
  std::string fullPath(path + so_name);
  tptrs.filename = so_name;
  /* open the needed object */
  tptrs.handle = dlopen(fullPath.c_str(), RTLD_LOCAL | RTLD_LAZY);
  if (!tptrs.handle) {
    fputs(dlerror(), stderr);
    exit(1);
  }

  {
    *(void **)(&tptrs.fptr_trend_init_no_char) = dlsym(tptrs.handle,
                                                       "trend_init_no_char_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_enthalpy) = dlsym(tptrs.handle,
                                                   "trend_enthalpy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_entropy) = dlsym(tptrs.handle,
                                                  "trend_entropy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_specificVolume) = dlsym(tptrs.handle,
                                                         "trend_specificvolume_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_thermo_dens) = dlsym(tptrs.handle,
                                                      "trend_thermo_dens_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_dpdrho) = dlsym(tptrs.handle, "trend_dpdrho_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_d2pdrho2) = dlsym(tptrs.handle,
                                                   "trend_d2pdrho2_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_compMoleWeight) = dlsym(tptrs.handle,
                                                         "trend_compmoleweight_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_moleWeight) = dlsym(tptrs.handle,
                                                     "trend_moleweight_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_getcrit) = dlsym(tptrs.handle,
                                                  "trend_getcrit_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_rhomax_srk) = dlsym(tptrs.handle,
                                                     "trend_rhomax_srk_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_pressure) = dlsym(tptrs.handle,
                                                   "trend_pressure_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_psrk) = dlsym(tptrs.handle, "trend_psrk_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_zfac) = dlsym(tptrs.handle,
                                               "trend_zfac_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_residualgibbs_TP) = dlsym(tptrs.handle,
                                                           "trend_residualgibbs_tp_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_residualenthalpy_TP) = dlsym(tptrs.handle,
                                                              "trend_residualenthalpy_tp_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_residualentropy_TP) = dlsym(tptrs.handle,
                                                             "trend_residualentropy_tp_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_speedofsound) = dlsym(tptrs.handle,
                                                       "trend_speedofsound_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_ideal_Cp) = dlsym(tptrs.handle,
                                                   "trend_ideal_cp_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_ideal_enthalpy) = dlsym(tptrs.handle,
                                                         "trend_ideal_enthalpy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_ideal_entropy) = dlsym(tptrs.handle,
                                                        "trend_ideal_entropy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_get_binary_parameters) = dlsym(tptrs.handle,
                                                                "trend_get_binary_parameters_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_set_binary_parameters) = dlsym(tptrs.handle,
                                                                "trend_set_binary_parameters_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_getreducedrhot) = dlsym(tptrs.handle,
                                                         "trend_getreducedrhot_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_free_energy) = dlsym(tptrs.handle,
                                                      "trend_free_energy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_internal_energy) = dlsym(tptrs.handle,
                                                          "trend_internal_energy_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_thermotv) = dlsym(tptrs.handle,
                                                   "trend_thermotv_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_calcfres) = dlsym(tptrs.handle,
                                                   "trend_calcfres_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_calcfid) = dlsym(tptrs.handle,
                                                  "trend_calcfid_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_specvol) = dlsym(tptrs.handle,
                                                  "trend_specvol_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_voltest) = dlsym(tptrs.handle,
                                                  "trend_voltest_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_rmix) =
      dlsym(tptrs.handle, "trend_rmix_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_aux_sat_prop) =
      dlsym(tptrs.handle, "trend_aux_sat_prop_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

  {
    *(void **)(&tptrs.fptr_trend_ideal) =
      dlsym(tptrs.handle, "trend_ideal_");
    char *error = dlerror();
    if (error != NULL)  {
      fputs(error, stderr);
      exit(1);
    }
  }

}

extern "C" void trend_init_no_char_(int* nc,int path[],int* npath,
                                    int comps[],int* ncomps,int *mix,
                                    double* Rgas)
{
  int omp_num_threads = 1;
#ifdef USEOMP
  omp_num_threads = omp_get_max_threads();
#endif
  const char* trendroot = std::getenv("TRENDROOT");
  std::string pathstring;
  if (trendroot == NULL) {
    pathstring = "./trend/bin/";
  }
  else {
    pathstring = trendroot;
    pathstring = pathstring + "/bin/";
  }
  std::string so_name_base("libtrend_");
#ifdef TARGET
#define xstr1(s) str1(s)
#define str1(s) #s
  so_name_base += xstr1(TARGET);
#else
  so_name_base += "optim_gfortran_Linux";
#endif
  so_name_base += ".so";
  std::vector<std::string> so_name(omp_num_threads,so_name_base);

#ifdef USEOMP
  // Copy files?
  if (omp_num_threads > 1) {

    /* Open the input file. */
    std::string fullpath = pathstring + so_name_base;
    int read_fd = open (fullpath.c_str(), O_RDONLY);
    if (read_fd <= 0) {
      std::cout << "File not found: " << fullpath.c_str() << "\n";
      exit(1);
    }
    /* Stat the input file to obtain its size. */
    struct stat stat_buf;
    fstat (read_fd, &stat_buf);
    /* Open the output file for writing, with the same permissions as the
       source file. */
    so_name_base = so_name_base.substr(0,so_name_base.length()-3);
    for (int i=1; i<=omp_num_threads; ++i) {
      std::stringstream ss;
      ss.width(2);
      ss << std::setfill('0') << i;
      so_name[i-1] = so_name_base + ss.str() + ".so";
      std::string fullpath = pathstring + so_name[i-1];
      int write_fd = open (fullpath.c_str(), O_WRONLY | O_CREAT, stat_buf.st_mode);
      /* Blast the bytes from one file to the other. */
      off_t offset = 0;
      sendfile (write_fd, read_fd, &offset, stat_buf.st_size);
      /* Close up. */
      close (write_fd);
    }
    close (read_fd);
  }
#endif

  trend_if.clear();
  trend_if.resize(omp_num_threads);

  for(int i=0; i<omp_num_threads; ++i) {
    load_trend_so_and_pointers(pathstring,so_name[i],trend_if[i]);
    (*trend_if[i].fptr_trend_init_no_char)(nc,path,npath,comps,ncomps,mix,Rgas);
  }
}

extern "C" void trend_enthalpy_(double* t,double* rho,
                                double x[],double* h,double* dhdt,
                                double* dhdp,double dhdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_enthalpy)(t,rho,x,h,dhdt,dhdp,dhdx);
}

extern "C" void trend_entropy_(double* t,double* rho,
                               double x[],double* s,double* dsdt,
                               double* dsdp,double dsdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_entropy)(t,rho,x,s,dsdt,dsdp,dsdx);
}

extern "C" void trend_specificvolume_(double* t,
                                      double x[],double* v,double* dvdt,
                                      double* dvdp,double dvdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_specificVolume)(t,x,v,dvdt,dvdp,dvdx);
}

extern "C" void trend_thermo_dens_(double* t,double* rho,
                                   double x[],double lnfug[],double* lnfugt,
                                   double* lnfugp,double* lnfugx)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_thermo_dens)(t,rho,x,lnfug,lnfugt,lnfugp,lnfugx);
}

extern "C" double trend_dpdrho_(double x[],double* t,double* rho)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_dpdrho)(x,t,rho);
}

extern "C" double trend_d2pdrho2_(double x[],double* t,double* rho)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_d2pdrho2)(x,t,rho);
}

extern "C" double trend_compmoleweight_(int* j)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_compMoleWeight)(j);
}

extern "C" double trend_moleweight_(double z[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_moleWeight)(z);
}

extern "C" void trend_getcrit_(int* i,double* tci,double* pci,double* oi,
                               double* vci,double* tnbi)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_getcrit)(i,tci,pci,oi,vci,tnbi);
}

extern "C" void trend_rhomax_srk_(double x[], double* rhomax)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_rhomax_srk)(x, rhomax);
}

extern "C" double trend_pressure_(double n[], double* t, double* v,
                                  double* dpdv, double* dpdt, double* dpdn)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_pressure)(n, t, v, dpdv, dpdt, dpdn);
}

extern "C" double trend_psrk_(double* t, double* p, double x[], int* phase)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_psrk)(t, p, x, phase);
}

extern "C" void trend_zfac_(double* t,double* rho,
                            double x[],double* zfac,double* dzdt,
                            double* dzdp,double dzdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_zfac)(t,rho,x,zfac,dzdt,dzdp,dzdx);
}

extern "C" void trend_residualgibbs_tp_(double* t,double* p,
                                        double* rho,double x[],
                                        double* gr,double* dgrdt,
                                        double* dgrdp)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_residualgibbs_TP)(t,p,rho,x,gr,dgrdt,dgrdp);
}

extern "C" void trend_residualenthalpy_tp_(double* t,double* p,
                                           double* rho,double x[],
                                           double* hr,double* dhrdt,
                                           double* dhrdp,double dhrdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_residualenthalpy_TP)(t,p,rho,x,hr,dhrdt,dhrdp,dhrdx);
}

extern "C" void trend_residualentropy_tp_(double* t,double* p,double* rho,
                                          double x[],double* sr,double* dsrdt,
                                          double* dsrdp,double dsrdx[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  (*trend_if[thread_index].fptr_trend_residualentropy_TP)(t,p,rho,x,sr,dsrdt,dsrdp,dsrdx);
}

extern "C" double trend_speedofsound_(double* t, double* rho, double x[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_speedofsound)(t, rho, x);
}

extern "C" double trend_ideal_cp_(double* t, int* j)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_ideal_Cp)(t, j);
}

extern "C" double trend_ideal_enthalpy_(double* t, int* j)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_ideal_enthalpy)(t, j);
}

extern "C" double trend_ideal_entropy_(double* t, double* p, int* j)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_ideal_entropy)(t, p, j);
}

extern "C" void trend_get_binary_parameters_(int* i1, int* i2, double ParRedFun[],
                                             int ndf[], double ParDepFun[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_get_binary_parameters)(i1, i2,
                                                                    ParRedFun, ndf, ParDepFun);
}

extern "C" void trend_set_binary_parameters_(int* i1, int* i2, double ParRedFun[],
                                             int ndf[], double ParDepFun[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_set_binary_parameters)(i1, i2,
                                                                    ParRedFun, ndf, ParDepFun);
}

extern "C" void trend_getreducedrhot_(double* t, double x[], double* rhored, double *tred)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_getreducedrhot)(t, x, rhored, tred);
}

extern "C" double trend_free_energy_(double x[], double* t, double* v,
                                     double* dydv, double* dydt,
                                     double* d2ydt2, double* d2ydv2,
                                     double* d2ydvdt)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_free_energy)(x, t, v, dydv, dydt,
                                                          d2ydt2, d2ydv2, d2ydvdt);
}

extern "C" double trend_internal_energy_(double x[], double* t, double* v,
                                         double* dudv, double* dudt)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_internal_energy)(x, t, v, dudv, dudt);
}

extern "C" void trend_thermotv_(double* t, double* v, double n[],
                                double lnfug[], double lnfugT[],
                                double lnfugV[], double* lnfugn)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_thermotv)(t,v,n,lnfug,lnfugT,
                                                       lnfugV,lnfugn);
}

extern "C" void trend_calcfres_(double* t, double* v, double n[],
                                double* F, double* F_T, double* F_V,
                                double F_n[], double* F_TT, double* F_TV,
                                double* F_VV, double F_Tn[],
                                double F_Vn[], double* F_nn)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_calcfres)(t,v,n,F,F_T,F_V,F_n,
                                                       F_TT,F_TV,F_VV,F_Tn,
                                                       F_Vn,F_nn);
}

extern "C" void trend_calcfid_(double* t, double* v, double n[],
                               double* F, double* F_T, double* F_V,
                               double* F_TT, double* F_TV,
                               double* F_VV, double* F_n,
                               double* F_Tn, double* F_Vn,
                               double* F_nn)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_calcfid)(t,v,n,F,F_T,F_V,
                                                      F_TT,F_TV,F_VV,F_n,
                                                      F_Tn,F_Vn,F_nn);
}

extern "C" double trend_specvol_(double* t, double* P, double n[],
                                 int* phase)
{
  //std::cout <<"\n" << *t << " " << *P << " " << n << " " << "\n" << std::flush;
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_specvol)(t,P,n,phase);
}

extern "C" int trend_voltest_(double* t, double* P, double* V, double n[],
                              int* phase)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_voltest)(t,P,V,n,phase);
}

extern "C" double trend_rmix_(double z[])
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_rmix)(z);
}

extern "C" double trend_aux_sat_prop_(double* T, int* i, double* P,
                                      double* vv, double* vl)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_aux_sat_prop)(T,i,P,vv,vl);
}

extern "C" double trend_ideal_(double* T, int* i, double* Cp,
                               double* h, double* s)
{
  int thread_index = 0;
#ifdef USEOMP
  thread_index = omp_get_thread_num();
#endif
  return (*trend_if[thread_index].fptr_trend_ideal)(T,i,Cp,h,s);
}

extern "C" void trend_close_so_()
{
  for(std::vector<trend_pointers>::size_type i=0; i<trend_if.size(); ++i) {
    std::cout << "Closing " << trend_if[i].filename << "\n";
    dlclose(trend_if[i].handle);
  }
}
