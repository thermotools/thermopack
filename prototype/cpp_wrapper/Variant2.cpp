/* This source file automatically generated on 2023-06-07 using 
   FortWrap wrapper generator version 2.2.2 */

#include <cstring> // For strcpy
#include "Variant2.h"

// Constructor:
Variant2::Variant2(int ident, int nc, float Tc, float Vc, int var1, float var2) {
  data_ptr = nullptr;
  allocate_variant2_(&data_ptr); // Allocate Fortran derived type
  variants__variant2_ctor_sub_wrap(data_ptr, &ident, &nc, &Tc, &Vc, &var1, &var2); // Fortran Constructor
}

// Constructor:
Variant2::Variant2(const char* ident) {
  data_ptr = nullptr;
  allocate_variant2_(&data_ptr); // Allocate Fortran derived type
  // Create C array for Fortran input string data
  char ident_c__[10+1];
  if (ident) {
    strncpy(ident_c__, ident, 10+1); ident_c__[10] = 0; // strncpy protects in case ident is too long
    for (size_t i=strlen(ident_c__); i<10+1; i++) ident_c__[i] = ' '; // Add whitespace for Fortran
  }
  variants__variant2_db_ctor_sub_wrap(data_ptr, ident ? ident_c__ : nullptr); // Fortran Constructor
}

// Destructor:
Variant2::~Variant2() {
  deallocate_variant2_(data_ptr); // Deallocate Fortran derived type
}

void Variant2::internal_comp(float T, float V, const float n[], float* computed) {
  variants__variant2_internal_comp_wrap(data_ptr, &T, &V, n, computed);
}

void Variant2::Fres(float T, float V, const float n[], float* Fres, float** Ft, float** Fv, float* Fn[]) {
  variants__variant2_fres_wrap(data_ptr, &T, &V, n, Fres, Ft, Fv, Fn);
}

