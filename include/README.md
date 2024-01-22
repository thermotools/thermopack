# ThermoPack C/C++ interface

ThermoPack can be interfaced from C/C++ using the header file `thermopack.h` in this directory. The file `test.cpp` and 
`Makefile` in the `test_includes` directory serve as a minimal example of how to link ThermoPack to a C program.

To build the test file (`test.cpp`) navigate to the top-level thermopack directory (the one above this directory), and
run the commands

```
make [optim/debug]
cd include/test_includes/
make
```

The first command will build the thermopack library, which is placed in the `bin` directory. The second `make` command 
compiles `test.cpp` and links to the thermopack library. The options `optim/debug` are used to toggle debug flags off/on.


# If something goes wrong

* The path to your Fortran and C compiler are set in the top of the `Makefile`, in the variables `FC` and `CC`. Modify these
as needed.
* If you get the error message: `ld: Assertion failed: (_file->_atomsArrayCount == computedAtomCount && "more atoms allocated than expected"), function parse, file macho_relocatable_file.cpp, line 2061.`
  * You may be using a Mac, and may need to update `Xcode` command line tools, as there appears to be a bug in a specific version.
    * See [here](https://github.com/iains/gcc-12-branch/issues/6#issuecomment-1260282797) for more info on the bug.
  * See [here](https://stackoverflow.com/questions/15417619/how-do-you-update-xcode-on-osx-to-the-latest-version) for info
on updating `Xcode`.
* If none of the above solve the problem, please feel free to leave an Issue.