function _can-install() {
    true
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    git clone 'https://github.com/calliecameron/emacs-launchers' "${PACKAGE_INSTALL_DIR}/emacs-launchers"
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}/emacs-launchers" &&
    git pull
}
