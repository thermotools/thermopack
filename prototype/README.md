# ThermoPack v3.0.0 Prototyping

Here, a proof of concept is being developed to ensure that fortwrap is capable of wrapping the functionality we are looking
to implement in ThermoPack v3, specifically the class structure.

FortWrap is a pure python parser that generates a lightweight C++ wrapper, with pybind11 bindings for Fortran source code. 
To install it: 

```
git clone https://github.com/thermotools/fortwrap.git
git switch -c iso_c_bindings # The current working branch
pip install .
```

The test case is currently:

* `constants.f90` : Module that is `use`'d by all other modules, but is not wrapped.
* `BaseEoS` : Abstract class 
   * Inheriting classes must implement `Fres`
   * Implements `Fideal`, dummy function for ideal gas residual helmholtz energy
   * Implements `pressure`, which calls `Fres` and `Fideal`
* `NotIdGas` : In `directeos.f90` - Concrete class inheriting from `BaseEoS`
* `VariantEoS` : In `Variants.f90` - Abstract class inheriting from `BaseEoS`
   * Inheriting classes must override `internal_comp`
   * Also contains two parameters not found in `BaseEoS` (`param1` and `param2`)
   * Implements `variant_common_comp`
   * `Variant1` : Concrete class inheriting from `VariantEoS`
     * Implements `internal_comp`
     * Implements `Fres`, which calls `internal_comp` and `variant_common_comp`
   * `Variant2` : Concrete class inheriting from `VariantEoS`
     * Implements `internal_comp`, but differently than `Variant1`
     * Implements `Fres`, which calls `internal_comp` and `variant_common_comp`
* A (very simple/minimal) compdatadb, with utils, such that EoS objects can be initialized from identifier strings
  * A python script in `fluids` is used to manipulate the database, but currently does not generate the fortran-side database
  * `Variant1` and `Variant2` have been tested to initialize correctly for different species and parameter references
  * Wrapping of these constructors has not been attempted yet.

The wrapping procedure (e.g. running `build.sh`)
 * Generates `ISO_C_BINDINGS` wrappers for the fortran methods
 * Generates a C++ wrapper, with pybind11 bindings. Wrapper code is placed in `prototype/cpp_wrapper`
 * The Python wrapper must currently be written manually, but the api is equivalent to the current thermopack api
   * The python wrapper is found in `prototype/py_wrapper`

# Status

Current status:
 * Pybind11 bindings must use `std::vector<T>` for list input from python, while the C++ wrapper uses arrays for input variables. Therefore, the lambdas must pass `var.data()` to the internal C++ method. This has been tested manually, but not automated yet. 
 * See above for status on the `compdatadb` and initializing from identifier strings
 * Something weird is going on with the top-level `CMakeLists.txt`, which causes it to constantly think it has changed the `CMAKE_CXX_COMPILER` and rerun indefinitely.
   * To remedy, run `cmake .. -D CMAKE_CXX_COMPILER=/opt/homebrew/bin/g++-13`
 * Also, extra compiler flags are being added somewhere, causing the compiler to use `-std=c++11`, not `-std=c++17`, and attempt to compile for `x86` instead of `arm64`.
   * This is also fixed by explicitly specifying the compiler when calling `cmake` (the above fix).

Moving forward:

 * Implement Fortran-side initialisation from a struct that holds parameters.
   * Partially complete - 
     * Wrapping of these constructors has not been tested.
     * Having several constructors (C++ side polymorphic constructors) has not been attempted yet.
 * Implement internal computations that are not wrapped, to ensure that it is possible to make a minimal wrapper.
 * Implement an external generic method (e.g. numerical solver) that accepts a function pointer of some sort, that can be used on a type bound procedure.
   * For example: A method that takes `eos.pressure` as input, and solves `p(T, V, n) = p0` for e.g. `V`.
   * This method does not need to be wrapped.

To play around:
 * `cpp_test/main.cpp`, `fortran_test/main.f90`, and `pytest.py` contain working examples of initialising various models and running dummy-computations in each layer.
 * To build the project, navigate to the `prototype` directory (this directory), and

```
mkdir build
cmake .. && make
```

  * This will build the files
    * `build/fortran_lib/libdemopack.a` - The Fortran library
    * `build/cpp_wrapper/libdemopack_cpp.a` - The C++ library (without python bindings)
    * `build/cpp_wrapper/libdemo-cpython-<python_version>-<system>.so`
  * The build system makes some assumptions regarding where to find `pybind11` (set in `PYBIND11_ROOT_DIR` of `prototype/CMakeLists.txt`) as well as the C++ compiler (also set in `prototype/CMakeLists.txt`). 
    * It should not be necessary to modify the `CMakeLists.txt` files in the `prototype/src` or `prototype/cpp_wrapper` directories.
  * To `import` the library to python, the import path must point to the `libdemo-cpython-<python_version>-<system>.so` file, and the imported module is called `libdemo`, such as

```
from build.cpp_wrapper import libdemo #Importing from the `prototype` directory
```

# Interfacing

There are essentially two interfaces that are handled here `Fortran <-> C++` and `C++ <-> Python`. These interfaces both have
some requirements that must be fulfilled, which results in some translation being necessary within the C++ wrapper. Currently,
an attempt is being made to ensure that all this translation happens within lambda functions in the pybind11 bindings.

The result is that the C++ wrapper is closer to Fortran than python, and that the C++ wrapper uses raw pointers
and arrays, rather than stl containers.

In short: Fortran has no concept of stl containers, and works only with pointers. Python (through pybind11) on the other hand works very well with
e.g. `std::vector`, and has no concept of pointers.

Parameters that are fortran `optional`, must be `pointer to pointer` on the C++ side.
If the parameter is "not" passed, the innermost of the pointers is set to `nullptr`, such that fortran recieves a pointer to 
`nullptr`, and behaves as if the parameter was not recieved :

```
// In C++
float* val = nullptr;
float** val_p = &val;

! In Fortran
present(val_p) ! Returns .false.
```

Parameters that are fortran `intent(inout)` or `intent(out)` must be `pointer` on the C++ side, and `np.array` on the python side.
Using pybind11, a pointer to the underlying memory of the `np.array` is retrieved and passed to the C++ wrapper, which forwards 
the pointer to Fortran.

| Modifiable | Optional | Python                   | C++                        | Fortran  |
|------------|----------|--------------------------|----------------------------|----------|
| No         | No       | float                    | float                      | real     |
| No         | No       | list                     | std::vector => array       | array    |
| ---        | ---      | ------                   | ------                     | -----    |
| Yes        | No       | float => np.array<float> | np.array<float> => float*  | real     |
| Yes        | No       | list[T] => np.array<T>   | np.array<T> => T*          | real     |
| ---        | ---      | ------                   | ------                     | -----    |
| Yes        | Yes      | float => np.array<float> | np.array<float> => float** | real     |
| Yes        | Yes      | list[T] => np.array<T>   | np.array<T> => T**         | real     |

# Current workflow

First: Navigate to the top-level directory of the `fortwrap` project (where `setup.py` is found) and `pip install -e .`

Then: Navigate to the `thermopack/prototype` directory (this directory, from now referred to as `demo`). 

Run the script `build.sh`, which does the following:

 * Compile the Fortran source
 * Build the archive `libdemo_fortran.a`
 * Move the archive and `.mod` files to the `demo/wrappers` directory
 * Run `python -m fortwrap -g -i ../FortWrapOptions.txt` from the `demo/src` directory
 * Compile the resulting `FortranISOWrappers.f90` and add it to the `libdemo_fortran.a` archive
 * Compile the `.cpp` files in the `demo/wrappers` directory
 * Compile `demo/main.cpp`

To compile the C++ wrappers with pybind11, you must additionally run `cmake .. && make` from the `demo/wrappers/build` directory.

The `demo/wrappers/CMakeLists.txt` does the following:

 * Define the `pybind_module` target
 * Add the `demo/wrappers/.cpp` files to the target
    * It is the binding module `pybind11_bindings.cpp` that is used, not `bindings.cpp`
 * Link to the archive `libdemo_fortran.a`
 * Link to `libgfortran.dylib`

The scripts `rebuild_and_run.sh` and `rebuild_fortran_prog.sh` will build (and run) the files `main.cpp` and `main.f90`.