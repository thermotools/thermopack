# ThermoPack GUI

The graphical user interface to ThermoPack is found in this directory. The GUI should still be considered an alpha version,
meaning that it has limited functionality and has not been thoroughly tested. Development and testing is an ongoing project.

## Running the app with Python

If you have Python installed, you can run the app by setting up a fresh virtual environment and running 
```bash
git clone https://github.com/thermotools/thermopack.git
cd thermopack/addon/gui
pip install -r requirements.txt
python -m gui
```
This should provide you with all required dependencies and open a new window with the ThermoPack GUI.

## Downloading the app

Pre-bundled executables can be built for macOS using the script `build_gui.sh`. The disk image size surpasses GitHub's 
file size limit, and these are therefore currently not distributed online.

## Building distributables (for maintainers)

The script `build_gui.sh` bundles the ThermoPack GUI app into a disk image for macOS running on arm64. The same script
should also work for macOS running on x86 to build a disk image for that platform. See that script for links to
required dependencies and instructions for building distributables, similar scripts should work for Linux and (hopefully)
Windows.