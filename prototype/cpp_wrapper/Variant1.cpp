/* This source file automatically generated on 2023-06-07 using 
   FortWrap wrapper generator version 2.2.2 */

#include <cstring> // For strcpy
#include "Variant1.h"

// Constructor:
Variant1::Variant1(int ident, int nc, float Tc, float Vc, int var1, float var2) {
  data_ptr = nullptr;
  allocate_variant1_(&data_ptr); // Allocate Fortran derived type
  variants__variant1_ctor_sub_wrap(data_ptr, &ident, &nc, &Tc, &Vc, &var1, &var2); // Fortran Constructor
}

// Constructor:
Variant1::Variant1(const char* ident) {
  data_ptr = nullptr;
  allocate_variant1_(&data_ptr); // Allocate Fortran derived type
  // Create C array for Fortran input string data
  char ident_c__[10+1];
  if (ident) {
    strncpy(ident_c__, ident, 10+1); ident_c__[10] = 0; // strncpy protects in case ident is too long
    for (size_t i=strlen(ident_c__); i<10+1; i++) ident_c__[i] = ' '; // Add whitespace for Fortran
  }
  variants__variant1_db_ctor_sub_wrap(data_ptr, ident ? ident_c__ : nullptr); // Fortran Constructor
}

// Destructor:
Variant1::~Variant1() {
  deallocate_variant1_(data_ptr); // Deallocate Fortran derived type
}

void Variant1::internal_comp(float T, float V, const float n[], float* computed) {
  variants__variant1_internal_comp_wrap(data_ptr, &T, &V, n, computed);
}

void Variant1::Fres(float T, float V, const float n[], float* Fres, float** Ft, float** Fv, float* Fn[]) {
  variants__variant1_fres_wrap(data_ptr, &T, &V, n, Fres, Ft, Fv, Fn);
}

