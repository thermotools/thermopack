#!/bin/bash
set -e

platforms=("macOS-latest" "macOS-12" "ubuntu-latest" "windows-latest")
dylib_dirs=(".dylibs" ".dylibs" "../thermopack.libs" "NO_DYLIB_DIR")

length=${#platforms[@]}
for ((i=0; i<$length; i++)); do
    platform=${platforms[$i]}
    whldir="wheel-v3-${platform}"
    dylib_dir=${dylib_dirs[$i]}
    if [ -d "$whldir" ]; then
        echo "Processing directory: $whldir"
        echo "Directory contains: "
        ls -AR $whldir

        # Create dist directory and move config- and header files to expected locations
        distdir="thermopack-${platform}"
        if [ "${platform}" == "windows-latest" ]; then
            mkdir -p ${distdir}/installed
        else
            mkdir -p ${distdir}/installed/${dylib_dir}
        fi
        mkdir -p ${distdir}/addon/cppThermopack
        cp thermopack-config.cmake ${distdir}
        cp -r addon/cppThermopack/cppThermopack ${distdir}/addon/cppThermopack

        echo "Set up dist directory ${distdir}: "
        ls -AR $distdir
        
        echo "Unpacking wheel ..." 
        cd $whldir
        unzip thermopack*
        cd ..
        mv ${whldir}/thermopack/libthermopack* ${distdir}/installed

        # Do not need to relocate any dylib dependencies for windows
        if [ "${platform}" != "windows-latest" ]; then
            mv ${whldir}/thermopack/${dylib_dir}/* ${distdir}/installed/${dylib_dir}
        fi

        echo "Completed move to dist directory ${distdir}:"
        ls -AR ${distdir}

        zip ${distdir}.zip ${distdir}
    else
        echo "Directory $dir does not exist."
    fi
done