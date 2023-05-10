/* This source file automatically generated on 2023-05-10 using 
   FortWrap wrapper generator version 2.2.2 */

#include "BaseEos.h"

void BaseEos::Fideal(float T, float V, const float n[], float* Fid, float** Ft, float** Fv, float* Fn[]) {
  base_eos__fideal_wrap(data_ptr, &T, &V, n, Fid, Ft, Fv, Fn);
}

void BaseEos::pressure(float T, float V, const float n[], float* p) {
  base_eos__pressure_wrap(data_ptr, &T, &V, n, p);
}

void BaseEos::set_Tc(float Tc) {
  base_eos__set_tc_wrap(data_ptr, &Tc);
}

