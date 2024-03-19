#!/bin/bash
set -e
cd gui
cd ..

[ -d ./gui/fluids ] && rm -r ./gui/fluids
cp -r ../fluids ./gui/fluids

pyinstaller gui/__main__.py --noconfirm --distpath ./gui_dist --workpath ./gui_build \
--onedir \
--name ThermoPack \
--add-data gui/about.html:. \
--add-data gui/stylesheet.qss:. \
--add-data gui/layouts:layouts \
--add-data gui/images:images \
--add-data gui/icons:icons \
--add-data gui/fluids:fluids