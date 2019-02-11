function _can-install() {
    os linux && linux-variant main && package-installed dev-tools && can-sudo
}

function _install() {
    sudo apt-get -y install qtbase5-dev
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
