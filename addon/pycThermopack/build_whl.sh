cd ../.. # ~/thermopack/
make clean
make optim
cd addon/pycThermopack
python makescript.py optim
python map_platform_specifics.py

binary_arch="$(lipo -archs thermopack/libthermopack.dynlib)"
[[ "${binary_arch}" != "x86_64" ]] &&  echo "Binary file is not x86_64, but " && echo "${binary_arch}" && exit 1

python -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
delocate-wheel -w fixed_wheels -v thermopack-2.1.0-py3-none-any.whl
cd fixed_wheels
mv thermopack-2.1.0-py3-none-any.whl thermopack-2.1.0-py3-none-macosx_10_9_x86_64.whl
cd ..
rm thermopack-2.1.0-py3-none-any.whl # Prevent using the old wheel by accident if version is updated, but build script is not
