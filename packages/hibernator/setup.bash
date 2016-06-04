function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ]
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-hibernator.sh"
}
