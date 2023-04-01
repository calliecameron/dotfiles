function _can-install() {
    dotfiles-linux-variant main && dotfiles-package-installed dev-tools && dotfiles-can-sudo
}

function _install() {
    sudo apt-get -y install qtbase5-dev
    git clone https://github.com/calliecameron/yabr "${PACKAGE_INSTALL_DIR}" &&
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
