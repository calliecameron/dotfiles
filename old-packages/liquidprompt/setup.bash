function _install() {
    git clone https://github.com/nojhan/liquidprompt "${PACKAGE_INSTALL_DIR}"
}

function _update() {
    git pull
}
