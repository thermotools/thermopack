#Set-PSDebug -Trace 1
$tp_version = $args[0]

echo "Running before_build on windows for version $tp_version"
mkdir build
cd build
cmake .. -G Ninja -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=cl -DCMAKE_CXX_COMPILER=cl -DCMAKE_BUILD_TYPE=Release
echo "cmake finished ..."
cmake --build . --config=Release --target install
echo "build finished ..."
# python -c "import sys; sys.path.insert(0, '../addon/pycThermopack'); import makescript; makescript.windows_make('v3')"
python ../addon/pycThermopack/map_platform_specifics.py --diffs=$tp_version
echo "--- pycThermopack contents ---"
dir ../addon/pycThermopack
echo "--- pycThermopack/thermopack contents ---"
dir ../addon/pycThermopack/thermopack
#Set-PSDebug -Off