function _can-install() {
    os linux && linux-variant main && graphical
}

function _install() {
    # This is temporary, until the PPA supports xenial
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget http://download1181.mediafireuserdownload.com/jbdxd778socg/i9ojswkht4eonlk/FreeFileSync_9.2_Ubuntu_16.04_64-bit.tar.gz &&
    tar -xf FreeFileSync_9.2_Ubuntu_16.04_64-bit.tar.gz &&
    cd FreeFileSync &&
    chmod a-x CHANGELOG LICENSE Resources.zip ding.wav gong.wav harp.wav
}
