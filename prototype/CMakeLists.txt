cmake_minimum_required(VERSION 3.12)
cmake_policy(SET CMP0127 NEW)
cmake_policy(SET CMP0048 NEW)

set(PYBIND11_ROOT_DIR /usr/local/include/pybind11)

set(CXX "/opt/homebrew/bin/g++-13")

project(DemoPack LANGUAGES Fortran CXX)
set(CMAKE_FORTRAN_COMPILER /opt/homebrew/bin/gfortran)
# enable_language(CXX)
# set(CMAKE_CXX_COMPILER /opt/homebrew/bin/g++-13)
set(CMAKE_OSX_ARCHITECTURES "arm64" CACHE STRING "The OSX architecture")
set(CMAKE_CXX_FLAGS "-std=c++17 -arch arm64 -Wfatal-errors")


add_subdirectory(./src fortran_lib)
add_subdirectory(${PYBIND11_ROOT_DIR} pybind11)
add_subdirectory(./cpp_wrapper cpp_wrapper)
add_subdirectory(./fortran_test fortran_test)
add_subdirectory(./cpp_test cpp_test)