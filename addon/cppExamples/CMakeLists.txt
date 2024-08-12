# Example for Compiling and linking C++ executables using thermopack.
# This example assumes that libthermopack.dylib has already been built and installed with `make install`
# The environment variable THERMOPACK_DIR should point to the root directory of ThermoPack (where thermopack-config.cmake is found)
# this can be set with `export THERMOPACK_DIR=<path/to/thermopack>`.
#
# Note: On macOS (arm64), you might need to
#   export CC=/opt/homebrew/bin/gcc-13
#   export CXX=/opt/homebrew/bin/g++-13
# before running cmake, in order to force compilation for arm64.

project(cppExamples)
cmake_minimum_required(VERSION 3.2)

string(ASCII 27 Esc)
set(ColourRed "${Esc}[31m")
set(ColourBlue "${Esc}[34m")

set(THERMOPACK_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../..")

# Searches for thermopack-config.cmake in the directory ${THERMOPACK_DIR}, which can be set with `export THERMOPACK_DIR=<path/to/thermopack>`
# Once thermopack is found, it provides some convenience variables shown below, as well as the exported target `thermopack` which can be used for linking directly.
find_package(THERMOPACK REQUIRED)
if (NOT THERMOPACK_INSTALLED)
    message("${ColourRed}Thermopack was found but is not installed. Exiting ...")
    return()
endif()

message("${ColourBlue}Thermopack was found at : ${THERMOPACK_ROOT}")
message("${ColourBlue}Using libthermopack at : ${THERMOPACK_LIB}")
message("${ColourBlue}Using cppThermopack headers at : ${THERMOPACK_INCLUDE}")
message("${ColourBlue}Linking thermopack using exported target thermopack")

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wfatal-errors -std=c++17")

set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")


add_executable(basic basic.cpp)
target_link_libraries(basic PUBLIC thermopack)

add_executable(tp_properties tp_properties.cpp)
target_link_libraries(tp_properties PUBLIC thermopack)

add_executable(tv_properties tv_properties.cpp)
target_link_libraries(tv_properties PUBLIC thermopack)

add_executable(flashes flashes.cpp)
target_link_libraries(flashes PUBLIC thermopack)

add_executable(saturation saturation.cpp)
target_link_libraries(saturation PUBLIC thermopack)
