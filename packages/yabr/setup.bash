function _can-install() {
    # Also need the QT libraries, but we can assume the setup scripts installed those
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    type qmake &>/dev/null
}

function _install() {
    git clone https://github.com/CallumCameron/yabr.git "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    qmake -qt=5 &&
    make &&

    cp -t "${HOME}/Desktop" launchers/*.desktop
}

function _update() {
    git pull &&
    qmake -qt=5 &&
    make
}
