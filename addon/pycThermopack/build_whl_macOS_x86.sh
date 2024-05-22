#!/bin/bash

set -e

tp_version="2.2.3"

cd ../../addon/pycThermopack # Ensure that we are running this script from the correct directory

cd ../..

make clean
make optim
cd addon/pycThermopack

python makescript.py optim -diffs=v2

binary_arch="$(lipo -archs thermopack/libthermopack.dylib)"
[[ "${binary_arch}" != "x86_64" ]] &&  echo "Binary file is not x86_64, but " && echo "${binary_arch}" && exit 1

python -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
delocate-wheel -w . -v thermopack-${tp_version}-py3-none-any.whl
mv thermopack-${tp_version}-py3-none-any.whl thermopack-${tp_version}-py3-none-macosx_10_9_x86_64.whl

exit 0
# DANGER ZONE: THIS UPLOADS FILES TO PYPI! ONLY WRITTEN HERE TO REMEMBER THE COMMANDS.

twine upload -r testpypi wheelhouse/thermopack-${tp_version}-py3-none-macosx_10_9_x86_64.whl

exit 0

twine upload wheelhouse/thermopack-${tp_version}-py3-none-macosx_10_9_x86_64.whl
