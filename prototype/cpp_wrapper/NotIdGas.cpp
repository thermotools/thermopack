/* This source file automatically generated on 2023-05-10 using 
   FortWrap wrapper generator version 2.2.2 */

#include "NotIdGas.h"

// Constructor:
NotIdGas::NotIdGas(int ident, int nc, float Tc, float Vc) {
  data_ptr = nullptr;
  allocate_notidgas_(&data_ptr); // Allocate Fortran derived type
  notidgas_mod__notidgas_ctor_sub_wrap(data_ptr, &ident, &nc, &Tc, &Vc); // Fortran Constructor
}

// Destructor:
NotIdGas::~NotIdGas() {
  deallocate_notidgas_(data_ptr); // Deallocate Fortran derived type
}

void NotIdGas::Fres(float T, float V, const float n[], float* Fres, float** Ft, float** Fv, float* Fn[]) {
  notidgas_mod__notidgas_fres_wrap(data_ptr, &T, &V, n, Fres, Ft, Fv, Fn);
}

