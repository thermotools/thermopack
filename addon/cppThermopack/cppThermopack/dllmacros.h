#pragma once

typedef void* ADDRESS;

#ifdef __INTEL_THERMOPACK__ // INTEL FORTRAN (x64)
    #define MOD_PREFIX
    #define MOD_INTERTXT _mp_
    #define MOD_POSTFIX _
    #define FORTRAN_POSTFIX _
#else // GNU FORTRAN mapping
    #define MOD_PREFIX __
    #define MOD_INTERTXT _MOD_
    #define MOD_POSTFIX
    #define FORTRAN_POSTFIX _
#endif

#define _CONCAT(x, y) x##y
#define _NEST_CONCAT(x, y) _CONCAT(x, y)
#define get_export_name(MOD, METHOD) _NEST_CONCAT(_NEST_CONCAT(MOD_PREFIX, MOD),_NEST_CONCAT(_NEST_CONCAT(MOD_INTERTXT, METHOD), MOD_POSTFIX))