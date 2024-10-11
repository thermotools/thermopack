---
layout: default
version: 
title: Getting Started - C++
permalink: /vcurrent/getting_started_cpp.html
---

The ThermoPack C++ wrapper is a header-only wrapper designed to be compatible with `stl` containers like `std::vector`, and to support use of `std::unique_ptr` and cousins. In short, it is intended to make using ThermoPack together with modern C++ as painless as possible.

- [Building ThermoPack](#building-thermopack)
- [ThermoPack headers](#thermopack-headers)
- [Including and linking ThermoPack](#including-and-linking-thermopack)
- [Library structure](#library-structure)
- [Examples](#examples)

# Building ThermoPack

The ThermoPack C++ wrapper is a header-only wrapper, that simplifies linking to the `libthermopack` library that is built and installed with the `CMakeLists.txt` in the root directory of ThermoPack.

To build the required library,

```bash
git clone https://github.com/thermotools/thermopack.git
cd thermopack
mkdir build
cd build
cmake ..
make install
```

This should generate the file `thermopack/addon/cppThermopack/libthermopack.[so/dylib]`, which you will later be linking to.

# ThermoPack headers

The header files you will need are found in `thermopack/addon/cppThermoPack/cppThermoPack`. 

# Including and linking ThermoPack

*Note:* Before including ThermoPack in your project, ensure that you have compiled and installed ThermoPack by following the instructions for [building with cmake](source_build.html).

An example `CMakeLists.txt` is found in `thermopack/addon/cppExamples`. In essence, all that is required to include ThermoPack is to add the following to your `CMakeLists.txt`:

```cmake
set(THERMOPACK_DIR "/path/to/thermopack")
find_library(THERMOPACK REQUIRED)

add_executable(<my_program> <my_source.cpp>)
add_library(<my_lib> <my_lib_source.cpp>)
# etc.

target_link_libraries(<my_program> thermopack) # find_library ensures that the exported target "thermopack" is available
target_link_libraries(<my_lib> thermopack)
# etc.
```

The environment variable `THERMOPACK_DIR` can also be set using
```bash
export THERMOPACK_DIR=/path/to/thermopack
```
and should point to the top-level directory of the thermopack-package (where `thermopack-config.cmake` is found). After the `find_library` command has been run, several convenience variables will have been defined:
* `THERMOPACK_INSTALLED` : `"TRUE"` if thermopack is installed, `"FALSE"` otherwise
* `THERMOPACK_ROOT` : Path to directory where `thermopack-config.cmake` was found
* `THERMOPACK_LIB` : Path to thermopack dynamic library
* `THERMOPACK_INCLUDE` : Path to thermopack C++ headers
* `thermopack` : Imported shared library target with headers (this is what you want to link to using `target_link_libraries`)

# Library structure

The ThermoPack C++ wrapper defines a class structure that is almost identical to the structure in the python wrapper. All equations of state inherit from the `Thermo` class (defined in `thermo.h`), and only implement initialization and some minor utility methods.

In addition, several utility structures are defined in `utils.h`. These include the `Property` and `VectorProperty` classes, which are used to hold computed values and derivatives. The structures support implicit conversion to respectively `double` and `std::vector<double>`, such that their existence can be ignored unless you are interested in computing derivatives.

The result of phase equilibrium calculations, such as flash- and saturation point calculations, are returned as a `FlashResult` object, which holds the attributes
* `double T` - Temperature (K)
* `double p` - Pressure (Pa)
* `double betaV` - Vapour fraction
* `double betaL` - Liquid fraction
* `std::vector<double> z` - Total composition
* `std::vector<double> x` - Liquid composition
* `std::vector<double> y` - Vapour composition
* `std::string flash_type` - Information about the calculation this object originated from

# Examples

Some sample programs using ThermoPack in C++ are found in the directory `thermopack/addon/cppExamples`. To build them, navigate to that directory, and

```bash
mkdir build
cd build
cmake ..
make
```

This will build the examples as executables in the `build` directory.

Currently implemented examples are
* `basic` - Demonstrates initialization of some equations of state
* `flashes` - Flash calculations
* `tp_properties` - Compute properties at given temperature and pressure
* `tv_properties` - Compute properties at given temperature and volume
* `saturation` - Compute dew- and bubble points, and the critical point of a mixture