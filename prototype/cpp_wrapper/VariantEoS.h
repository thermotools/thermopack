/* This source file automatically generated on 2023-05-10 using 
   FortWrap wrapper generator version 2.2.2 */

#ifndef VARIANTEOS_H_
#define VARIANTEOS_H_

#ifdef _MSC_VER
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif


#ifndef SWIG // Protect declarations from SWIG
#include <cstdlib>
#include "InterfaceDefs.h"
#include "BaseEos.h"

extern "C" {
  void variants__variant_common_comp_wrap(ADDRESS c_this, float* mixed_common);
}
#endif // SWIG

class DLLEXPORT VariantEoS : public BaseEos {

protected:
  // VariantEoS can not be instantiated
  VariantEoS() {}

public:
  virtual ~VariantEoS() {}

  void variant_common_comp(float* mixed_common);

  virtual void internal_comp(float T, float V, const float n[], float* computed) = 0;

};

#endif /* VARIANTEOS_H_ */
