function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    # Don't run at startup
    sudo systemctl stop smbd nmbd &&
    sudo systemctl disable smbd nmbd
}

function _update() {
    _install
}
