string(ASCII 27 Esc)
set(ColorRed "${Esc}[31m")
set(ColorBlue "${Esc}[34m")

cmake_minimum_required(VERSION 3.5)
project(ThermoPack LANGUAGES Fortran)

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
    message(${ColorRed}"cmake not configured for windows!")
    return()
    # Need to set up external link to lapack (preferrably a tagged relase, ex. v3.12.0):
    # git clone https://github.com/Reference-LAPACK/lapack.git
    # git checkout v3.12.0
    # Build using: cmake --build . --config Release
    set(tp_flags_common "/nologo /fpp /fpe:0 /names:lowercase /assume:underscore /real-size:64 /fp:precise /extend-source:132 /iface:cref")
    set(tp_optim_flags "${tp_flags_common}")
    set(tp_debug_flags "${tp_flags_common} /check:bounds /warn:all,noexternal /traceback /check:all,noarg_temp_created,nopointers")
endif()

if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(tp_fortran_flags ${tp_debug_flags})
else()
    set(CMAKE_BUILD_TYPE "Release")
    message(STATUS "CMAKE_BUILD_TYPE not specified, defaulting to Release.")
    set(tp_fortran_flags ${tp_optim_flags})
endif()

# Set FORTRAN compile flags for thermopack and lapack
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${tp_fortran_flags}")

if(MSVC)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/external/lapack)
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
