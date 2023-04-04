function _can-install() {
    true
}

function _install() {
    git clone https://github.com/seebi/dircolors-solarized.git "${PACKAGE_INSTALL_DIR}"
}

function _update() {
    git pull
}
