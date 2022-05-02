function _can-install() {
    os linux
}

function _install() {
    git clone https://github.com/seebi/dircolors-solarized.git "${PACKAGE_INSTALL_DIR}"
}

function _update() {
    git pull
}
