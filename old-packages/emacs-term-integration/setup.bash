function _can-install() {
    true
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    # We keep a local copy of these for machines where Emacs isn't installed
    git clone 'https://github.com/calliecameron/term-cmd' "${PACKAGE_INSTALL_DIR}/term-cmd" &&
    git clone 'https://github.com/calliecameron/term-alert' "${PACKAGE_INSTALL_DIR}/term-alert"
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}/term-cmd" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/term-alert" &&
    git pull
}
