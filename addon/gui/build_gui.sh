#!/bin/bash
# Script to bundle the thermopack gui app for macOS arm64
# Run the script from the same directory as the script is in
#
# Produces:
#     dist/macOS_arm64/ThermoPack.app
#     dist/macOS_arm64/ThermoPack.dmg
#     build/ (temporary, can be deleted if everything works out)
#     tmp_dist/ (temporary, can be deleted if everything works out)
#
# For tutorial see: https://www.pythonguis.com/tutorials/packaging-pyqt5-applications-pyinstaller-macos-dmg/
# Requires:
#     pyinstaller (pip install pyinstaller) https://pyinstaller.org/en/stable/
#     create-dmg (brew install create-dmg) https://github.com/create-dmg/create-dmg
#
# Note: When bundling the gui, the global APP_ROOT in gui/utils.py must be changed
#       Path(os.path.dirname(__file__)) => Path(os.path.dirname(__file__)).parent
# as the root directory of the bundled app is one level above the root directory of the gui when run as a python module.
# Todo: Update paths such that the above note is not neccessary.
#
# Note: When debugging it may be useful to change the flag `--windowed` to `--onedir`

set -e

# Dumb check that we are running from the correct directory
cd gui
[ ! -f stylesheet.qss ] && echo "Running from the wrong directory!" && exit 0
cd ..

# Retrieve all fluids
[ -d ./gui/fluids ] && rm -r ./gui/fluids
cp -r ../../fluids ./gui/fluids

# Bundle app
pyinstaller gui/__main__.py \
  --noconfirm \
  --distpath ./tmp_dist \
  --workpath ./build \
  --windowed \
  --icon gui/icons/thermopack_icon.icns \
  --name ThermoPack \
  --add-data gui/about.html:. \
  --add-data gui/stylesheet.qss:. \
  --add-data gui/layouts:layouts \
  --add-data gui/images:images \
  --add-data gui/icons:icons \
  --add-data gui/fluids:fluids \
  --collect-all thermopack

# Ensure that distribution directories exist, delete old executable if it exists
# Note: Copying the new executable into the location of an existing executable can corrupt the file at the target location
[ ! -d dist ] && mkdir dist
[ ! -d dist/macOS_arm64 ] && mkdir dist/macOS_arm64
[ -d dist/macOS_arm64/ThermoPack.app ] && rm -r dist/macOS_arm64/ThermoPack.app
cp -r tmp_dist/ThermoPack.app dist/macOS_arm64/ThermoPack.app

# Set up directory to build disk image
cd dist/macOS_arm64
[ -d dmg ] && rm -r dmg
mkdir dmg
cp -r ThermoPack.app dmg/ThermoPack.app
[ -f ThermoPack.dmg ] && rm ThermoPack.dmg

# Build disk image
create-dmg \
  --volname "ThermoPack" \
  --volicon ../../gui/icons/thermopack_icon.icns \
  --background "../../gui/images/dmg_background.png" \
  --window-pos 200 120 \
  --window-size 700 380 \
  --icon-size 100 \
  --icon "ThermoPack.app" 220 60 \
  --hide-extension "ThermoPack.app" \
  --app-drop-link 550 60 \
  ThermoPack.dmg \
  dmg/

rm -r dmg