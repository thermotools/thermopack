/* This source file automatically generated on 2023-06-07 using 
   FortWrap wrapper generator version 2.2.2 */

#ifndef BASEEOS_H_
#define BASEEOS_H_

#ifdef _MSC_VER
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif


#ifndef SWIG // Protect declarations from SWIG
#include <cstdlib>
#include "InterfaceDefs.h"

extern "C" {
  void base_eos__baseeos_init_wrap(ADDRESS c_this, const char* ident);
  void base_eos__fideal_wrap(ADDRESS c_this, float* T, float* V, const float n[], float* Fid, float** Ft, float** Fv, float* Fn[]);
  void base_eos__pressure_wrap(ADDRESS c_this, float* T, float* V, const float n[], float* p);
  void base_eos__set_tc_wrap(ADDRESS c_this, float* Tc);
}
#endif // SWIG

/**
 * \brief Base EoS class
 */
class DLLEXPORT BaseEos {

protected:
  // BaseEos can not be instantiated
  BaseEos() {}

public:
  virtual ~BaseEos() {}

  void BaseEos_init(const char* ident);

  void Fideal(float T, float V, const float n[], float* Fid, float** Ft=nullptr, float** Fv=nullptr, float* Fn[]=nullptr);

  void pressure(float T, float V, const float n[], float* p);

  void set_Tc(float Tc);

  virtual void Fres(float T, float V, const float n[], float* Fres, float** Ft=nullptr, float** Fv=nullptr, float* Fn[]=nullptr) = 0;

  ADDRESS data_ptr;
};

#endif /* BASEEOS_H_ */
