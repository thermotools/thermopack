#!/bin/bash

set -e

tp_version="2.2.0"

cd ../../addon/pycThermopack # Ensure that we are running this script from the correct directory

rm -r wheelhouse
mkdir wheelhouse

cd ../..

# make clean
make optim
cd addon/pycThermopack

python makescript.py optim -diffs=v2

binary_arch="$(lipo -archs thermopack/libthermopack.dynlib)"
[[ "${binary_arch}" != "arm64" ]] &&  echo "Binary file is not arm64, but " && echo "${binary_arch}" && exit 1

python -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
delocate-wheel -w . -v thermopack-${tp_version}-py3-none-any.whl
mv thermopack-${tp_version}-py3-none-any.whl thermopack-${tp_version}-py3-none-macosx_11_0_arm64.whl

exit 0

twine upload -r testpypi wheelhouse/*