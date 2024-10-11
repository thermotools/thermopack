cmake_minimum_required(VERSION 3.19)
project(ThermoPack LANGUAGES Fortran)

string(ASCII 27 Esc)
set(ColorRed "${Esc}[31m")
set(ColorBlue "${Esc}[34m")

if(NOT MSVC)
    execute_process(COMMAND bash -c "arch" OUTPUT_VARIABLE PROC)
    set(tp_flags_common "-cpp -fPIC -fdefault-real-8 -fdefault-double-8 -frecursive -fopenmp -Wno-unused-function -Wno-unused-variable")

    include(CheckCompilerFlag)
    check_compiler_flag(Fortran "-arch arm64" arm64_supported)
    if(arm64_supported)
        set(gf_proc "-arch arm64")
        set(gf_march "-arch arm64 -fno-expensive-optimizations")
    else()
        set(gf_proc "-mieee-fp")
        set(gf_march "-march=x86-64 -msse2")
    endif()

    set(tp_debug_flags "${gf_proc} ${tp_flags_common} -g -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow -Wno-unused-dummy-argument -Wall")
    set(tp_profile_flags "${tp_flags_common} -g -pg")
    set(tp_optim_flags "${tp_flags_common} -O3 ${gf_march} -funroll-loops")

    if(APPLE)
        set(CMAKE_FORTRAN_COMPILER /opt/homebrew/bin/gfortran)
        set(CMAKE_OSX_ARCHITECTURES "arm64" CACHE STRING "The OSX architecture")
    endif()
else()
    # Build using: cmake --build . --config Release
    set(tp_flags_common "/nologo /fpp /fpe:0 /names:lowercase /assume:underscore /fp:precise /extend-source:132 /iface:cref /Qdiag-disable:10448")
    set(tp_optim_flags "${tp_flags_common}")
    set(tp_debug_flags "${tp_flags_common} /check:bounds /traceback")
endif()

# Check if the build type is not set and set it to Release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
endif()

# Set FORTRAN compile flags for thermopack and lapack
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${tp_optim_flags}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${tp_debug_flags}")

if(MSVC)
    if(POLICY CMP0077)
        set(CMAKE_POLICY_DEFAULT_CMP0077 NEW)
    endif()
    # Disable single/complex/complex16 as it is not used by thermopack
    set(BUILD_SINGLE OFF)
    set(BUILD_COMPLEX OFF)
    set(BUILD_COMPLEX16 OFF)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/lapack)
    # Add flags not comatible with lapack
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} /real-size:64 ")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /real-size:64 /warn:all,noexternal /check:all,noarg_temp_created,nopointers")
endif(MSVC)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/src "thermopack")

option(test "Build test suite" OFF)
if(test)
    cmake_policy(SET CMP0074 NEW)
    find_package(PFUNIT QUIET)
    if (NOT PFUNIT_FOUND)
        message("${ColorRed}Did not find pFUnit. If you have installed pFUnit, make sure that PFUNIT_DIR is set. Falling back to building pFUnit from source...")
        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/pFUnit)
    else()
        message(STATUS "Found pFUnit: ${PFUNIT_DIR}")
    endif()
    message(STATUS "Creating run_unittests target")
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/unittests)
endif()
