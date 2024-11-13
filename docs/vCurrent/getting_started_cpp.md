---
layout: default
version: 
title: Getting Started - C++
permalink: /vcurrent/getting_started_cpp.html
---

The ThermoPack C++ wrapper is a header-only wrapper designed to be compatible with `stl` containers like `std::vector`, and to support use of `std::unique_ptr` and cousins. In short, it is intended to make using ThermoPack together with modern C++ as painless as possible.

- [Installing ThermoPack](#installing-thermopack)
- [ThermoPack headers](#thermopack-headers)
- [Including and linking ThermoPack](#including-and-linking-thermopack)
- [Library structure](#library-structure)
- [Examples](#examples)

# Installing ThermoPack

The ThermoPack C++ wrapper is a header-only wrapper, that simplifies linking to the `libthermopack` binary. A pre-built ThermoPack library can be downloaded from the [GitHub release page](https://github.com/thermotools/thermopack/releases).

Once you have downloaded and unzipped the appropriate zip file for your system, you should see a directory structure like
```
|-- thermopack-<system>/
  |-- thermopack-config.cmake
  |-- addon/
    |-- cppThermopack/
  |-- installed/
```
Make sure to set the environment variable `THERMOPACK_DIR` to the `thermopack-<system>` directory (where `thermopack-config.cmake` is located) with `export THERMOPACK_DIR=path/to/thermopack-<system>`. You can also add this to your `.bash_profile` or similar.

## Building ThermoPack

If you prefer to build ThermoPack from source:
```bash
git clone https://github.com/thermotools/thermopack.git
cd thermopack
mkdir build
cd build
cmake ..
make install
cd ..
export THERMOPACK_DIR=${PWD}
```
This should generate the file `thermopack/installed/libthermopack.[so/dylib]`, which you will later be linking to.

You may want to add `export THERMOPACK_DIR=path/to/thermopack` to your `.bash_profile`, or equivalent, as well.

If you have any issues, see the [installation guide.](source_build.html)

# ThermoPack headers

The header files you will need are found in `thermopack/addon/cppThermoPack/cppThermoPack`. 

# Including and linking ThermoPack

*Note:* Before including ThermoPack in your project, ensure that you have compiled and installed ThermoPack by following the instructions for [building with cmake](source_build.html), and that the environment variable `THERMOPACK_DIR` is set to the directory where `thermopack-config.cmake` is located. 

An example `CMakeLists.txt` is found in `thermopack/addon/cppExamples`. In essence, all that is required to include ThermoPack is to add the following to your `CMakeLists.txt`:

```cmake
set(THERMOPACK_DIR "/path/to/thermopack") # Not nessecary if THERMOPACK_DIR is set as an environment variable
find_library(THERMOPACK REQUIRED)

add_executable(<my_program> <my_source.cpp>)
add_library(<my_lib> <my_lib_source.cpp>)
# etc.

target_link_libraries(<my_program> thermopack) # find_library ensures that the imported target "thermopack" is available
target_link_libraries(<my_lib> thermopack)
# etc.
```

*When building on Linux* You should add the line
```
set_target_properties(<my_target> PROPERTIES 
                            INSTALL_RPATH ${THERMOPACK_INSTALL_DIR}
                            INSTALL_RPATH_USE_LINK_PATH TRUE)
```
to your `CMakeLists.txt`. This will add the `THERMOPACK_INSTALL_DIR` to your library/programs runtime search path (`@rpath`) and ensure that your binary will find the thermopack dynamic library at runtime if it is moved or installed to some other location than where it is built.

After the `find_library(THERMOPACK REQUIRED)` command has been run, several convenience variables will have been defined:
* `THERMOPACK_FOUND` : `TRUE` if thermopack was found, `FALSE` otherwise
* `THERMOPACK_INSTALLED` : `TRUE` if the thermopack dynamic library is found, `FALSE` otherwise
* `THERMOPACK_ROOT` : Path to directory where `thermopack-config.cmake` was found
* `THERMOPACK_INSTALLED_DIR` : Path to directory in which thermopack has installed files
* `THERMOPACK_LIB` : Path to thermopack dynamic library
* `THERMOPACK_STATIC_LIB` : Path to thermopack static library (archive)
* `THERMOPACK_INCLUDE` : Path to thermopack C++ headers
* `thermopack` : Imported shared library target with headers (this is what you want to link to using `target_link_libraries`)

# Library structure

The ThermoPack C++ wrapper defines a class structure that is almost identical to the structure in the python wrapper. All equations of state inherit from the `Thermo` class (defined in `thermo.h`), and only implement initialization and possibly some utility methods.

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

*Note*: Before compiling the examples, you must have installed ThermoPack. ThermoPack will not be built as part of the build procedure for the examples.

Some sample programs using ThermoPack in C++ are found in the directory [`thermopack/addon/cppExamples`](https://github.com/thermotools/thermopack/tree/main/addon/cppExamples). To build them, navigate to that directory, and

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