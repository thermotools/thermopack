cmake_minimum_required(VERSION 3.16)
project(pycThermopack)

option(RECOMPILE_THERMOPACK "Recompile thermopack?" OFF)
if(RECOMPILE_THERMOPACK)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../.. "thermopack")
endif()