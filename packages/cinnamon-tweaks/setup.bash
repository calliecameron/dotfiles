function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DISPLAY}" ] &&
    package-installed emacs &&
    package-installed vlc &&
    pgrep cinnamon &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-cinnamon.sh"
}

function _update() {
    _install
}
