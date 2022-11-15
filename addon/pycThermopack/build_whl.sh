cd ~/thermopack/
make clean
make optim
cd addon/pycThermopack
python makescript.py optim
python map_platform_specifics.py
python -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
delocate-wheel -w fixed_wheels -v thermopack-2.0.0-py3-none-any.whl
cd fixed_wheels
mv thermopack-2.0.0-py3-none-any.whl thermopack-2.0.0-py3-none-macosx_10_9_x86_64.whl