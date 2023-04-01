function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-redshift.sh"
}
