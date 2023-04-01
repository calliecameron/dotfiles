function _can-install() {
    dotfiles-known-linux-variant && dotfiles-can-sudo
}

function _install() {
    local TARGET='/etc/sudoers.d/umask'
    sudo cp "${PACKAGE_CONF_DIR}/umask" "${TARGET}" &&
    sudo chown root:root "${TARGET}" &&
    sudo chmod ug=r "${TARGET}" &&
    sudo chmod o-rwx "${TARGET}"
}

function _update() {
    _install
}
