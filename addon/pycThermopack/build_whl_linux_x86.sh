#!/bin/bash

set -e

tp_version="2.2.3"

cd ../../addon/pycThermopack # Ensure that we are running this script from the correct directory
cd ../..

# Modify Makefile.code
original_line="LIBS += -llapack -lblas"
modified_line="LIBS += -L./lapack -llapack -lblas"
sed -i "s|$original_line|$modified_line|g" ./Makefile.code

make clean
make optim
cd addon/pycThermopack

python3.9 makescript.py optim -diffs=v2

# Clean out old build stuff
rm -rf ./thermopack.egg-info/
rm -rf ./build
rm -f ./wheelhouse/thermopack-${tp_version}-py3-none-manylinux_2_28_x86_64.whl

python3.9 -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
#auditwheel show thermopack-${tp_version}-py3-none-any.whl
auditwheel repair thermopack-${tp_version}-py3-none-any.whl -w .

echo "Successfully built and delocated wheel for ThermoPack ${tp_version}"
exit 0

# DANGER ZONE: THIS UPLOADS FILES TO PYPI! ONLY WRITTEN HERE TO REMEMBER THE COMMANDS.

twine upload -r testpypi wheelhouse/thermopack-${tp_version}-py3-none-manylinux_2_28_x86_64.whl

exit 0

twine upload wheelhouse/thermopack-${tp_version}-py3-none-manylinux_2_28_x86_64.whl

