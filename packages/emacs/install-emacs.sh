#!/bin/bash
# This is in a separate file so it can be called from CI.

set -eu

function usage() {
    echo "Usage: $(basename "${0}") prefix"
    exit 1
}

test -n "${1:-}" || usage
PREFIX="${1}"
EMACS_VERSION='29.4'

if [ -e "${PREFIX}/bin/emacs" ]; then
    echo 'Emacs already installed'
    exit 0
fi

mkdir -p "${PREFIX}"

sudo apt-get update
sudo apt-get -y install attr autoconf automake build-essential emacs emacs-el \
    emacs-goodies-el libacl1-dev libasound2-dev libdbus-1-dev libgccjit-13-dev \
    libgif-dev libgnutls28-dev libgpm-dev libgtk-4-dev libjansson-dev \
    libjpeg-dev liblcms2-dev liblockfile-dev libm17n-dev libncurses-dev \
    libotf-dev libpng-dev libpoppler-glib-dev libpoppler-private-dev \
    librsvg2-dev libselinux1-dev libsystemd-dev libtiff-dev libtree-sitter-dev \
    libwebkit2gtk-4.1-dev libwebp-dev libxpm-dev libz-dev texinfo valgrind

BUILD_DIR="$(mktemp -d)"
cd "${BUILD_DIR}"
wget "https://ftpmirror.gnu.org/gnu/gnu-keyring.gpg"
wget "https://ftpmirror.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz.sig"
wget "https://ftpmirror.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz"
gpg --verify --keyring ./gnu-keyring.gpg "emacs-${EMACS_VERSION}.tar.xz.sig"
tar -xf "emacs-${EMACS_VERSION}.tar.xz"
cd "emacs-${EMACS_VERSION}"
./configure --with-native-compilation --with-xwidgets "--prefix=${PREFIX}"
make -j "$(nproc)"
make -j "$(nproc)" install
cd
rm -rf "${BUILD_DIR}"
