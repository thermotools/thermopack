# Compile and link minimal C++ wrapper for ThermoPack. Assumes that libthermopack has been built, and that the
# libthermopack archive is found at ${libthermopack_path}, which is set at the top of this script.
# Note: On macOS (arm64), you might need to 
#   export CC=/opt/homebrew/bin/gcc-13
#   export CXX=/opt/homebrew/bin/g++-13
# before running cmake, in order to force compilation for arm64.
cmake_minimum_required(VERSION 3.2)

string(ASCII 27 Esc)
set(ColourRed "${Esc}[31m")
set(ColourBlue "${Esc}[34m")

set(libthermopack_path "${CMAKE_SOURCE_DIR}/../../../bin/libthermopack_optim_gfortran_Darwin.a")
set(libgfortran_path "/opt/homebrew/lib/gcc/13/libgfortran.dylib")

project(cpp_demo)
set(CPP_HEADER_DIR ${CMAKE_SOURCE_DIR}/../include)
file(GLOB CPP_HEADERS ${CPP_HEADER_DIR}/*.h)

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wfatal-errors -std=c++17")

set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")


add_executable(cpp_demo main.cpp)
target_include_directories(cpp_demo PUBLIC ${CPP_HEADER_DIR})

find_package(LAPACK)

if (NOT LAPACK_FOUND)
    message("${ColourRed}Did not find Lapack! exiting ...")
    return()
endif()

message("${ColourBlue}Using libthermopack at : ${libthermopack_path}")
message("${ColourBlue}Using libgfortran at : ${libgfortran_path}")

target_link_libraries(cpp_demo PUBLIC ${libthermopack_path} ${libgfortran_path} ${LAPACK_LIBRARIES})