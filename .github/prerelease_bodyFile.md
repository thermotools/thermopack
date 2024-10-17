This release is automatically updated whenever a pull request is merged to `main`.

For stable releases of `ThermoPack`, see releases named `vX.Y.Z`.

# Wheels for Python
To install this release, download and unzip the appropriate zip file, then install with `pip install thermopack -f wheel-<version>-<system>/`, where `wheel-<version>-<system>` is the directory created by unzipping the file.

For macOS, the wheels marked `macOS-12` are built for `x86_64` machines (using intel chips), while `macOS-latest` wheels are built for `arm64` machines (Apple Silicon, i.e. M1, M2, etc.).

You can also use
```bash
curl -L -o thermopack.zip https://github.com/thermotools/thermopack/releases/download/Latest-beta/wheel-<version>-<platform>.zip
unzip thermopack.zip
```

# Binaries and headers
Self-contained binary distributions with required headers for using the `cppThermopack` wrapper are contained in the `thermopack-<platform>.zip` assets. Downloading and unzipping the appropriate asset for your system, and running `export THERMOPACK_DIR=/path/to/thermopack-<platform>`, should allow `CMake`'s `find_library` to find `thermopack`. See the [docs on using `cppThermopack](https://thermotools.github.io/thermopack/vcurrent/getting_started_cpp.html) for help on including the headers and linking the `thermopack` library.

The distribution can also be set up from the command line with
```bash
curl -L -o thermopack.zip https://github.com/thermotools/thermopack/releases/download/Latest-beta/thermopack-<platform>.zip
unzip thermopack.zip
export THERMOPACK_DIR=${PWD}/thermopack-<platform>/
```

# Documentation and guides
Documentation and user guides for the latest version are found at [thermotools.github.io/thermopack](thermotools.github.io/thermopack), but are likely not as well maintained as the documentation and guides for stable releases.