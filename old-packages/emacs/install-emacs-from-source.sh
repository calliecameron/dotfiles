#!/bin/bash

function usage() {
    echo "Usage: $(basename "${0}") install_dir"
    exit 1
}

test -z "${1}" && usage
INSTALL_DIR="$(readlink -f "${1}")"

sudo apt-get -y install attr autoconf automake build-essential libacl1-dev libasound2-dev libdbus-1-dev libgconf2-dev libgif-dev libgnutls28-dev libgpm-dev libgtk-3-dev libjansson-dev libjpeg-dev liblockfile-dev libm17n-dev libncurses-dev libotf-dev libpng-dev libpoppler-glib-dev libpoppler-private-dev librsvg2-dev libselinux1-dev libsystemd-dev libtiff-dev libwebkit2gtk-4.0-dev libxpm-dev libz-dev paxctl texinfo valgrind

EMACS_VERSION='27.2'

if [ ! -e "${INSTALL_DIR}/bin/emacs" ]; then
    BUILD_DIR="$(mktemp -d)" &&
    cd "${BUILD_DIR}" &&
    wget "https://ftp.gnu.org/gnu/gnu-keyring.gpg" &&
    wget "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz.sig" &&
    wget "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz" &&
    gpg --verify --keyring ./gnu-keyring.gpg "emacs-${EMACS_VERSION}.tar.xz.sig" &&
    tar -xf "emacs-${EMACS_VERSION}.tar.xz" &&
    cd "emacs-${EMACS_VERSION}" &&
    ./configure --with-xwidgets "--prefix=${INSTALL_DIR}" &&
    make &&
    make install &&
    cd &&
    rm -rf "${BUILD_DIR}" || exit 1
fi
exit 0
