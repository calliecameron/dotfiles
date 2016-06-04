#!/bin/bash

sudo apt-get -y install cryptsetup dcfldd gnupg pinentry-gtk2 steghide &&

BUILD_DIR="$(mktemp -d)" &&
cd "${BUILD_DIR}" &&
wget 'https://files.dyne.org/tomb/tomb-2.2.tar.gz' &&
tar -xf 'tomb-2.2.tar.gz' &&
cd 'tomb-2.2' &&
sudo make install &&
cd &&
rm -rf "${BUILD_DIR}"
