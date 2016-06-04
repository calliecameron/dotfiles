function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-iptables.sh"
}
