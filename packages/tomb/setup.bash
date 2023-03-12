function _can-install() {
    linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install cryptsetup gnupg pinentry-gtk2 libgcrypt20-dev &&
    mkdir -p "${PACKAGE_INSTALL_DIR}/bin" &&
    BUILD_DIR="$(mktemp -d)" &&
    cd "${BUILD_DIR}" &&
    curl https://keybase.io/jaromil/pgp_keys.asc | gpg --import &&
    wget 'https://files.dyne.org/tomb/releases/Tomb-2.9.tar.gz' &&
    wget 'https://files.dyne.org/tomb/releases/Tomb-2.9.tar.gz.sha' &&
    wget 'https://files.dyne.org/tomb/releases/Tomb-2.9.tar.gz.asc' &&
    sha256sum -c Tomb-2.9.tar.gz.sha &&
    gpg --verify Tomb-2.9.tar.gz.asc Tomb-2.9.tar.gz &&
    tar -xf 'Tomb-2.9.tar.gz' &&
    cd 'Tomb-2.9' &&
    cp tomb "${PACKAGE_INSTALL_DIR}/bin" &&
    cd extras/kdf-keys &&
    make &&
    cp -t "${PACKAGE_INSTALL_DIR}/bin" tomb-kdb-hexencode tomb-kdb-pbkdf2 tomb-kdb-pbkdf2-gensalt tomb-kdb-pbkdf2-getiter &&
    cd &&
    rm -rf "${BUILD_DIR}"
}
