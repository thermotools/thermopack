# Fortran-side test suite

## Compiling and Running tests

To compile the test-suite the environment variable `PFUNIT_DIR` must be set to the path of the directory containing the directories `PFUNIT-4.x` and `FARGPARSE-1.x`, where `x` is some subversion number. If `pFUnit` has been built follwing the readme-instructions found there at the time of writing, this corresponds to
```bash
$ export PFUNIT_DIR="<some_path>/pFUnit/build/installed"
```
then, the test-suite can be compiled by running
```bash
$ make unittests_debug
```
from the thermopack root directory.

The test suit is run by runnning the executable
```bash
$ ./run_unittests
```

## Writing tests

## Adding or disabling tests

The tests to be run by the test suite are included by adding the test files to `./testSuites.inc`. The individual tests in each sub-suite (the `*.pf`-files) are subroutines preceeded by the `@Test` decorator. 

Commenting out a line from `testSuites.inc` will disable the entire sub-suite. Commenting out the `@Test` decorator for a subroutine in a sub-suite will disable that specific test.