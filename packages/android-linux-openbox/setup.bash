function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'android' ]
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-android-openbox.sh"
}
