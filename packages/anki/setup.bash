function _can-install() {
    os linux && linux-variant main && can-sudo && graphical && is64bit
}

function _install() {
    mkdir "${PACKAGE_INSTALL_DIR}" "${PACKAGE_INSTALL_DIR}/bin" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    curl https://raw.githubusercontent.com/dae/anki/master/pkgkey.asc | gpg --import &&
    wget https://apps.ankiweb.net/downloads/current/anki-2.1.8-checksums.txt &&
    gpg --verify anki-2.1.8-checksums.txt &&
    wget 'https://apps.ankiweb.net/downloads/current/anki-2.1.8-linux-amd64.tar.bz2' &&
    TMPFILE="$(mktemp)" &&
    grep 'anki-2.1.8-linux-amd64.tar.bz2' anki-2.1.8-checksums.txt > "${TMPFILE}" &&
    sha256sum -c "${TMPFILE}" &&
    tar -xf 'anki-2.1.8-linux-amd64.tar.bz2' &&
    ln -s "${PACKAGE_INSTALL_DIR}/anki-2.1.8/bin/anki" "${PACKAGE_INSTALL_DIR}/bin/anki" &&
    rm 'anki-2.1.8-linux-amd64.tar.bz2' "${TMPFILE}"
}
