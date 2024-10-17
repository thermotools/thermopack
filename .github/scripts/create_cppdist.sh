#!/bin/bash

# Leverage cibuildwheels to generate self-contained binary distributables for thermopack
# cibuildwheels has integration with delocate and auditwheel for macOS and Linux.
# These tools "repair" wheels by identifying the runtime dependencies of the thermopack dynamic library,
#   copying the dependencies to some known location within the wheel, and modifying the RPATH of the library
#   to the relative path to the copied dependencies.

# Thus: We can unzip a wheel that has been repaired by auditwheel or delocate, and retrieve the thermopack
#   dynamic library from the wheel. As long as we also bring along the runtime dependencies to the same location
#   relative to the thermopack dylib, they will be found at runtime.

# This allows us to distribute the thermopack binary to systems which don't have libgfortran and friends installed.
# This also allows us to build libraries that depend on the thermopack binary, without needing to compile thermopack 
#   from source: We can just use `curl` to download the bundled thermopack binary, runtime dependencies and headers.

# Finally: By adding thermopack-config.cmake to this bundle, and ensuring that the headers and thermopack binary are 
#   located in the correct locations relative to thermopack-config.cmake, this minimal bundle can be used with cmake
#   without any issues. If we want to make the distributed bundle work with fortran as well, we can just add the .mod 
#   files to the bundle, and add a THERMOPACK_MOD variable to thermopack-config.cmake.

set -e

platforms=("macOS-latest" "macOS-12" "ubuntu-latest" "windows-latest")
dylib_dirs=(".dylibs" ".dylibs" "../thermopack.libs" "NO_DYLIB_DIR")

length=${#platforms[@]}
for ((i=0; i<$length; i++)); do
    platform=${platforms[$i]}
    whldir="wheel-v3-${platform}"
    dylib_dir=${dylib_dirs[$i]}
    if [ -d "$whldir" ]; then
        echo "--- Processing directory: $whldir ---"
        echo "Directory contains: "
        ls -AR $whldir

        # Create dist directory and move config- and header files to expected locations
        distdir="thermopack-${platform}"
        mkdir -p ${distdir}/installed
        mkdir -p ${distdir}/addon/cppThermopack
        cp thermopack-config.cmake ${distdir}
        cp -r addon/cppThermopack/cppThermopack ${distdir}/addon/cppThermopack

        echo "--- Set up dist directory ${distdir} ---"
        ls -AR $distdir
        
        echo "--- Unpacking wheel ..." 
        cd $whldir
        unzip thermopack*
        cd ..
        mv ${whldir}/thermopack/libthermopack* ${distdir}/installed

        # Copy dynamic dependencies to expected location (relative to thermopack dylib)
        if [ -d ${whldir}/thermopack/${dylib_dir} ]; then
            mkdir -p ${distdir}/installed/${dylib_dir}
            mv ${whldir}/thermopack/${dylib_dir}/* ${distdir}/installed/${dylib_dir}
        fi

        echo "--- Completed move to dist directory ${distdir} ---"
        ls -AR ${distdir}

        zip -r ${distdir} ${distdir}
        echo "--- Contents of ${distdir}.zip ---"
        unzip -l ${distdir}.zip
    else
        echo "Directory ${whldir} does not exist."
    fi
done