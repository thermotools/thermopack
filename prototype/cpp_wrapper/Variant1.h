/* This source file automatically generated on 2023-05-10 using 
   FortWrap wrapper generator version 2.2.2 */

#ifndef VARIANT1_H_
#define VARIANT1_H_

#ifdef _MSC_VER
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif


#ifndef SWIG // Protect declarations from SWIG
#include <cstdlib>
#include "InterfaceDefs.h"
#include "VariantEoS.h"

extern "C" {
  void allocate_variant1_(ADDRESS *caddr);
  void deallocate_variant1_(ADDRESS caddr);
  void variants__variant1_ctor_sub_wrap(ADDRESS c_this, int* ident, int* nc, float* Tc, float* Vc, int* var1, float* var2);
  void variants__variant1_internal_comp_wrap(ADDRESS c_this, float* T, float* V, const float n[], float* computed);
  void variants__variant1_fres_wrap(ADDRESS c_this, float* T, float* V, const float n[], float* Fres, float** Ft, float** Fv, float* Fn[]);
}
#endif // SWIG

class DLLEXPORT Variant1 : public VariantEoS {

public:
  Variant1(int ident, int nc, float Tc, float Vc, int var1, float var2);
  ~Variant1();

  void internal_comp(float T, float V, const float n[], float* computed);

  void Fres(float T, float V, const float n[], float* Fres, float** Ft=nullptr, float** Fv=nullptr, float* Fn[]=nullptr);

};

#endif /* VARIANT1_H_ */
