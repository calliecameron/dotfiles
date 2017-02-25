#!/bin/bash

sudo apt-get -y install cryptsetup dcfldd gnupg pinentry-gtk2 steghide &&

BUILD_DIR="$(mktemp -d)" &&
cd "${BUILD_DIR}" &&
wget 'https://files.dyne.org/tomb/Tomb-2.3.tar.gz' &&
tar -xf 'Tomb-2.3.tar.gz' &&
cd 'Tomb-2.3' &&
sudo make install &&
cd &&
rm -rf "${BUILD_DIR}"
