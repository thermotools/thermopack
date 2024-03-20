#!/bin/bash
# Script to bundle the thermopack gui app
# Requires pyinstaller : https://pyinstaller.org/en/stable/
# Note: When debugging it may be useful to change the flag `--onefile` to `--onedir`
set -e

# Dumb check that we are running from the correct directory
cd gui
[ ! -f stylesheet.qss ] && exit 0
cd ..

# Retrieve all fluids
[ -d ./gui/fluids ] && rm -r ./gui/fluids
cp -r ../../fluids ./gui/fluids

# Bundle app
pyinstaller gui/__main__.py --noconfirm --distpath ./tmp_dist --workpath ./build \
--onefile \
--name ThermoPack \
--add-data gui/about.html:. \
--add-data gui/stylesheet.qss:. \
--add-data gui/layouts:layouts \
--add-data gui/images:images \
--add-data gui/icons:icons \
--add-data gui/fluids:fluids \
--collect-all thermopack

# Ensure that distribution directories exist, delete old executable if it exists
# Note: Copying the new executable into the location of an existing executable can lead to
# a corrupted file.
[ ! -d dist ] && mkdir dist
[ ! -d dist/macOS_arm64 ] && mkdir dist/macOS_arm64
[ -f dist/macOS_arm64/ThermoPack ] && rm dist/macOS_arm64/ThermoPack
cp tmp_dist/ThermoPack dist/macOS_arm64/ThermoPack