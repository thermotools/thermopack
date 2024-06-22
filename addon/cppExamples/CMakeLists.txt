# Example for Compiling and linking C++ executables using thermopack.
# This example assumes that libthermopack.dylib has already been built. The path to libthermopack.dylib
# is set in `libthermopack_path`, you will likely need to modify this.
# 
# Note: If you want to build with static linkage, you also need to link libgfortran.a, libquadmath.a, and
# lapack, as shown in CMakelists_static.txt.
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

set(libthermopack_path "${CMAKE_SOURCE_DIR}/../../bin/dynamic/libthermopack_optim_gfortran_Darwin.dylib")
set(THERMOPACK_HEADERS "${CMAKE_SOURCE_DIR}/../cppThermoPack")

message("${ColourBlue}Using libthermopack at : ${libthermopack_path}")
message("${ColourBlue}Using cppThermopack headers at : ${THERMOPACK_HEADERS}")

set(CMAKE_BUILD_TYPE "Release")
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wfatal-errors -std=c++17")

set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")


add_executable(basic basic.cpp)
target_include_directories(basic PUBLIC ${THERMOPACK_HEADERS})
target_link_libraries(basic PUBLIC ${libthermopack_path})

add_executable(tp_properties tp_properties.cpp)
target_include_directories(tp_properties PUBLIC ${THERMOPACK_HEADERS})
target_link_libraries(tp_properties PUBLIC ${libthermopack_path})

add_executable(tv_properties tv_properties.cpp)
target_include_directories(tv_properties PUBLIC ${THERMOPACK_HEADERS})
target_link_libraries(tv_properties PUBLIC ${libthermopack_path})

add_executable(flashes flashes.cpp)
target_include_directories(flashes PUBLIC ${THERMOPACK_HEADERS})
target_link_libraries(flashes PUBLIC ${libthermopack_path})

