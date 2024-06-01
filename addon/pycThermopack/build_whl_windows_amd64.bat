@echo off

set tp_version="2.2.3"

python -c "import makescript; makescript.windows_make('v2')"

:: Clean out old build stuff
if exist "thermopack.egg-info" (
    rmdir /s /q "thermopack.egg-info"
)
if exist "build" (
    rmdir /s /q "build"
)

python -m pip wheel --wheel-dir=wheelhouse .
cd wheelhouse
move thermopack-%tp_version%-py3-none-any.whl thermopack-%tp_version%-py3-none-win_amd64.whl
