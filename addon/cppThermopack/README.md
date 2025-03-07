# Thermopack C++ interface

ThermoPack can be interfaced from C++ using the header-only `cppThermopack` wrapper. This wrapper is set up to be compatible with `cmake`, such that `find_library` can be utilized to include the headers and link the ThermoPack binary.

See the docs at [thermotools.github.io/thermopack](https://thermotools.github.io/thermopack/vcurrent/getting_started_cpp.html) for guides to setting up and using the wrapper. The raw docfiles are found in `docs/vcurrent/getting_started_cpp.md`.

Some example programs using the `cppThermopack` wrapper, and an example `CMakeLists.txt` are found in `addon/cppExamples`.