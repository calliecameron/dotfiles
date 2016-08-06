function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DISPLAY}" ]
}

function _install() {
    # This is temporary, until the PPA supports xenial
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget http://download1208.mediafire.com/eo3eqyx1qpug/i7ral2ixb6xx2x2/FreeFileSync_8.3_Ubuntu_16.04_64-bit.tar.gz &&
    tar -xf FreeFileSync_8.3_Ubuntu_16.04_64-bit.tar.gz &&
    cd FreeFileSync &&
    chmod a-x CHANGELOG LICENSE Resources.zip ding.wav gong.wav harp.wav
}
