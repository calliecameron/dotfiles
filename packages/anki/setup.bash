function _can-install() {
    os linux && linux-variant main && can-sudo && graphical && is64bit
}

function _install() {
    mkdir "${PACKAGE_INSTALL_DIR}" "${PACKAGE_INSTALL_DIR}/bin" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget 'https://apps.ankiweb.net/downloads/current/anki-2.0.45-amd64.tar.bz2' &&
    tar -xf 'anki-2.0.45-amd64.tar.bz2' &&
    ln -s "${PACKAGE_INSTALL_DIR}/anki-2.0.45/bin/anki" "${PACKAGE_INSTALL_DIR}/bin/anki" &&
    rm 'anki-2.0.45-amd64.tar.bz2'
}
