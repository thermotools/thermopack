/* This source file automatically generated on 2023-06-07 using 
   FortWrap wrapper generator version 2.2.2 */

#include <cstring> // For strcpy
#include "BaseEos.h"

void BaseEos::BaseEos_init(const char* ident) {
  // Create C array for Fortran input string data
  char ident_c__[10+1];
  if (ident) {
    strncpy(ident_c__, ident, 10+1); ident_c__[10] = 0; // strncpy protects in case ident is too long
    for (size_t i=strlen(ident_c__); i<10+1; i++) ident_c__[i] = ' '; // Add whitespace for Fortran
  }
  base_eos__baseeos_init_wrap(data_ptr, ident ? ident_c__ : nullptr);
}

void BaseEos::Fideal(float T, float V, const float n[], float* Fid, float** Ft, float** Fv, float* Fn[]) {
  base_eos__fideal_wrap(data_ptr, &T, &V, n, Fid, Ft, Fv, Fn);
}

void BaseEos::pressure(float T, float V, const float n[], float* p) {
  base_eos__pressure_wrap(data_ptr, &T, &V, n, p);
}

void BaseEos::set_Tc(float Tc) {
  base_eos__set_tc_wrap(data_ptr, &Tc);
}

