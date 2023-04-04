function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo
}

function _install() {
    "${PACKAGE_SOURCE_DIR}/setup-redshift.sh"
}
