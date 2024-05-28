cmake_minimum_required(VERSION 3.2)
project(cpp_demo)
set(CPP_HEADER_DIR ${CMAKE_SOURCE_DIR}/../include)
file(GLOB CPP_HEADERS ${CPP_HEADER_DIR}/*.h)

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-Wfatal-errors")

add_executable(cpp_demo main.cpp)
target_include_directories(cpp_demo PUBLIC ${CPP_HEADER_DIR})

add_library(thermopack SHARED IMPORTED)
set_target_properties(thermopack PROPERTIES IMPORTED_LOCATION ${CMAKE_SOURCE_DIR}/../../../bin/dynamic/libthermopack_optim_gfortran_Darwin.dynlib)

target_link_libraries(cpp_demo PUBLIC thermopack)