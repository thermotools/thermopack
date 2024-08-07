# Fortran-side test suite

## Compiling and Running tests

### With `cmake`

The project is set up to search for `pFUnit` and use your installation if you have it installed. Set the environment variable `PFUNIT_DIR` before building if you have already installed `pFUnit`. If `pFUnit` is not found, the version of `pFUnit` under `external/pFUnit` will be built and used.

Build and run unittests by executing

```bash
mkdir build
cd build
cmake -Dtest=ON ..
make
./unittests/run_unittests
```

from the ThermoPack root directory (`..`). Note that the value of the `test` option is cached, so if you have previously built ThermoPack, you may need to clear the `CMakeCache.txt` in the build directory.

### With only `make`
The test-suite is set up and maintained to be compatible with the [`pFUnit` fork under the ThermoTools project](https://github.com/thermotools/pFUnit). To build the test suite you must first clone and build `pFUnit`.

To compile the test-suite the environment variable `PFUNIT_DIR` must be set to the path of the directory containing the directories `PFUNIT-4.x` and `FARGPARSE-1.x`, where `x` is some subversion number. This corresponds to running
```bash
$ export PFUNIT_DIR="<some_path>/pFUnit/build/installed"
$ make unittests_debug
```
from the thermopack root directory.

The test suit is run by running the executable
```bash
$ ./run_unittests
```

### Known possible issues
If your `PYTHONPATH` is not set up the way `pFUnit` expects, you may need to make the changes to `pFUnitParser.py` referenced in [PR#34](https://github.com/thermotools/thermopack/pull/34).

When setting the PFUNIT_DIR, <some_path> should be an absolute path. On Ubuntu, you cannot use the '~' alias for your home directory.

## Writing tests

## Adding or disabling tests

The tests to be run by the test suite are included by adding the test files to `./testSuites.inc`. The individual tests in each sub-suite (the `*.pf`-files) are subroutines preceeded by the `@Test` decorator. 

Commenting out a line from `testSuites.inc` will disable the entire sub-suite. Commenting out the `@Test` decorator for a subroutine in a sub-suite will disable that specific test.