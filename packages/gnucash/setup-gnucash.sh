#!/bin/bash

function usage() {
    echo "Usage: $(basename "${0}") install_dir"
    exit 1
}

test -z "${1}" && usage
INSTALL_DIR="$(readlink -f "${1}")"

VERSION='3.2'

sudo apt-get -y install aqbanking-tools gtk+3.0 guile-2.0 guile-2.0-dev gwenhywfar-tools icu-devtools ktoblzcheck libaqbanking-dev libboost-all-dev libdbd-sqlite3 libdbi-dev libglib2.0 libglib2.0-dev libgtk-3-dev libgwenhywfar60 libgwenhywfar60-dev libicu-dev libktoblzcheck1-dev libltdl-dev libofx-dev libsecret-1-0 libtool libwebkit2gtk-4.0-37 libwebkit2gtk-4.0-dev libxml2 libxml++2.6-dev libxml2-utils libxslt1.1 libxslt1-dev python3-pytest swig2.0 texinfo xsltproc &&

BUILD_DIR="$(mktemp -d)" &&
cd "${BUILD_DIR}" &&

# Googletest is a build dependency, but the wiki says to build it locally rather
# than using a package
git clone https://github.com/google/googletest.git &&
cd googletest &&
mkdir mybuild &&
cd mybuild &&
cmake -DBUILD_GMOCK=ON ../ &&
make &&
export GTEST_ROOT="${BUILD_DIR}/googletest/googletest" &&
export GMOCK_ROOT="${BUILD_DIR}/googletest/googlemock" &&

# build gnucash
cd "${BUILD_DIR}" &&
wget "https://sourceforge.net/projects/gnucash/files/gnucash%20%28stable%29/${VERSION}/gnucash-${VERSION}.tar.bz2" &&
tar -xf "gnucash-${VERSION}.tar.bz2" &&
mkdir 'build' &&
cd 'build' &&
cmake "-DCMAKE_INSTALL_PREFIX=${INSTALL_DIR}" "-DCMAKE_PREFIX_PATH=${INSTALL_DIR}" "-DWITH_PYTHON=on" "../gnucash-${VERSION}" &&
make &&
make install &&

# build the docs
cd "${BUILD_DIR}" &&
wget "https://github.com/Gnucash/gnucash-docs/archive/${VERSION}.tar.gz" &&
tar -xf "${VERSION}.tar.gz" &&
cd "gnucash-docs-${VERSION}" &&
mkdir mybuild &&
cd mybuild &&
../autogen.sh &&
../configure "--prefix=${INSTALL_DIR}" &&
make install &&

cd &&
rm -rf "${BUILD_DIR}"
